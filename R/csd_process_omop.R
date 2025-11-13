

#' Concept Set Distribution - OMOP Version
#'
#' @param cohort cohort for SQUBA testing; required fields:
#' - `site`
#' - `person_id`
#' - `start_date`
#' - `end_date`
#' @param domain_tbl input table defining the domains listed in the annotated concept set
#'                   four columns:
#'                   - `domain` the name of the CDM table associated with the concept; should match what is listed in the annotated concept set
#'                   - `concept_field` the name of the field in the domain table where the concepts are located
#'                   - `date_field` the name of the field in the domain table with the date that should be used for time-based filtering
#'                   - `vocabulary_field` PCORnet only; set to NA
#'
#' @param concept_set an annotated concept set CSV file with the following columns:
#' - `concept_id` required for OMOP; the concept_id of interest
#' - `concept_code` required for PCORnet; the code of interest
#' - `concept_name` optional; the descriptive name of the concept
#' - `vocabulary_id` required for PCORnet; the vocabulary of the code - should match what is listed in the domain table's vocabulary_field
#' - `variable` required; a string label grouping one concept code into a larger variable definition
#' - `domain` required; the name of the CDM table where the concept is stored - multiple domains can be included in the file, but only one domain should be listed per row
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#' - `single`: run the function for a single site
#' - `multi`: run the function for multiple sites
#' @param anomaly_or_exploratory Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#' level summary of the data to examine the fact representation within the cohort. Anomaly detection
#' analyses are specialized to identify outliers within the cohort.
#' @param num_concept_combined when `multi_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#' this argument is a boolean that will ensure that `concept1` and `concept2` meet some minimal threshold for including in the jaccard index
#' if `TRUE`, then *both* conditions for `num_concept_1` and `num_concept_2` should be met; if `FALSE` then just one condition needs to be met.
#' @param num_concept_1  when `multi_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#' this argument is an integer and requires a minimum number of times that the *first* concept appears in the dataset
#' @param num_concept_2 when `multi_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#' this argument is an integer and requires a minimum number of times that the *second* concept appears in the dataset
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param age_groups If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age`: the minimum age for the group (i.e. 10)
#' - `max_age`: the maximum age for the group (i.e. 20)
#' - `group`: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param time logical to determine whether to output the check across time
#' @param time_span when time = TRUE, a vector of two dates for the observation period of the study
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe summarizing the distribution of code usage for each user defined variable
#'
#' @import argos
#' @import squba.gen
#' @importFrom stringr str_wrap
#'
#' @keywords internal
#'
csd_process_omop <- function(cohort,
                             domain_tbl= conceptsetdistribution::csd_domain_file,
                             concept_set = conceptsetdistribution::csd_concept_set,
                             multi_or_single_site = 'single',
                             anomaly_or_exploratory='exploratory',
                             num_concept_combined = FALSE,
                             num_concept_1 = 30,
                             num_concept_2 = 30,
                             p_value = 0.9,
                             age_groups = FALSE, #read_codeset('age_group_definitions'),
                             time = TRUE,
                             time_span = c('2012-01-01', '2020-01-01'),
                             time_period = 'year'
){

  ## parameter summary output
  # output_type <- suppressWarnings(param_summ(check_string = 'csd',
  #                                            as.list(environment())))

  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  # Set up grouped list

  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}

  site_output <- list()

  # Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL,
                                omop_or_pcornet = 'omop') %>%
    group_by(!!! syms(grouped_list))

  # Execute function
  if(! time) {


    for(k in 1:length(site_list_adj)) {

      site_list_thisrnd <- site_list_adj[[k]]

      # filters by site
      cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

      if(multi_or_single_site=='single' & anomaly_or_exploratory=='anomaly') {
        variable_compute <- check_code_dist_ssanom_omop(cohort_codedist = cohort_site,
                                                        concept_set = concept_set,
                                                        domain_tbl = domain_tbl,
                                                        num_concept_combined = num_concept_combined,
                                                        num_concept_1 = num_concept_1,
                                                        num_concept_2 = num_concept_2)
      } else {
        variable_compute <- check_code_dist_csd_omop(cohort_codedist = cohort_site,
                                                     concept_set = concept_set,
                                                     time = time,
                                                     time_span = time_span,
                                                     time_period = time_period,
                                                     domain_tbl = domain_tbl)
      }


      site_output[[k]] <- variable_compute %>% mutate(site=site_list_thisrnd)

    }

    csd_tbl <- reduce(.x=site_output,
                      .f=dplyr::union)

    if(multi_or_single_site == 'multi' & anomaly_or_exploratory == 'anomaly'){

      csd_tbl_int <- compute_dist_anomalies(df_tbl = csd_tbl,
                                            grp_vars = c('variable', 'concept_id'),
                                            var_col = 'prop_concept',
                                            denom_cols = c('variable', 'ct_denom'))

      csd_tbl_final <- detect_outliers(df_tbl = csd_tbl_int,
                                       tail_input = 'both',
                                       p_input = p_value,
                                       column_analysis = 'prop_concept',
                                       column_variable = 'concept_id')

    }else{csd_tbl_final <- csd_tbl}

  } else {

    csd_tbl <- compute_fot(cohort = cohort_prep,
                           site_list = site_list_adj,
                           site_col = site_col,
                           time_span = time_span,
                           time_period = time_period,
                           reduce_id = NULL,
                           check_func = function(dat){
                             check_code_dist_csd_omop(cohort_codedist = dat,
                                                      concept_set = concept_set,
                                                      domain_tbl = domain_tbl,
                                                      time = TRUE,
                                                      time_span = time_span,
                                                      time_period = time_period)
                           })

    if(multi_or_single_site == 'multi' & anomaly_or_exploratory=='anomaly') {

      lookup <- csd_tbl %>% ungroup() %>% distinct(variable, concept_id)

      csd_tbl_ms <- ms_anom_euclidean(fot_input_tbl = csd_tbl,
                                      grp_vars = c('site', 'concept_id'),
                                      var_col = 'prop_concept')

      csd_tbl_final <- csd_tbl_ms %>% left_join(lookup)

    }else if(multi_or_single_site == 'single' & anomaly_or_exploratory=='anomaly'){

      csd_tbl_ss <- anomalize_ss_anom_la(fot_input_tbl = csd_tbl,
                                         grp_vars = 'concept_id',
                                         time_var = 'time_start',
                                         var_col = 'prop_concept')

      csd_tbl_final <- csd_tbl_ss

    }else{csd_tbl_final <- csd_tbl}

  }

  # cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
  #                      output function in csd_output: ', output_type, '.')))

  return(csd_tbl_final %>% replace_site_col())


}
