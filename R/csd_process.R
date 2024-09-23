

#' CSD Process function
#'
#' add csv output of main table and csv output of param summary
#' (that function needs updating to be more generalizable)
#'
#' @param cohort cohort for SSDQA testing; required fields:
#'               `site` | `person_id` | `start_date` | `end_date` where start and end date
#' @param domain_tbl tbl that is similar to the SCV check;
#'                   four columns: `domain` | `concept_field` | `date_field` | `vocabulary_field`
#'
#'                   the vocabulary_field column is only required for the PCORnet CDM
#' @param concept_set concept set CSV file with the following columns:
#'                    `concept_id` | `concept_code` | `concept_name` | `vocabulary_id` | `variable` | `domain`
#'                    The variable field is required to categorize each concept set into a particular variable
#'                    The domain is required so that the function knows which table to join to in order to derive counts
#' @param multi_or_single_site direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param anomaly_or_exploratory direction to determine what kind of check to run; a string
#'                               that is either `anomaly` or `exploratory`
#' @param num_concept_combined when `mult_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#'                             this argument is an integer and will ensure that `concept1` and `concept2` meet
#'                             some minimal threshold for including in the jaccard index; if `TRUE`, then
#'                             *both* conditions for `num_concept_1` and `num_concept_2` should be met;
#'                             if `FALSE` then just one condition needs to be met.
#' @param num_concept_1  when `mult_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#'                             this argument is an integer and requires a minimum number of times that
#'                             the *first* concept appears in the dataset
#' @param num_concept_2 when `mult_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#'                             this argument is an integer and requires a minimum number of times that
#'                             the *second* concept appears in the dataset
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param age_groups If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'
#'                     If you would not like to stratify by age group, leave the argument as NULL
#' @param time logical to determine whether to output the check across time
#' @param time_span when `time = TRUE`, a vector of two dates for the observation period of the study
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return for `single- and multi-` and `exploratory` analyses, the output is:
#'             `site` | `variable` | `ct_denom` | `concept_id` | `ct_concept` | `prop_concept`
#'         for `single` and `anomaly` analyses, the output is:
#'             `site` | `concept1`| `concept2`| `cocount`| `concept1_ct` | `concept2_ct` | `concept_count_union` |`jaccard_index` |
#'             `concept1_prop` | `concept2_prop` | `variable` |
#'             where `concept_count_union` is how often a pair of codes appear in the full dataset
#'             and `concept1_ct` and `concept2_ct` are how often each appear in the dataset
#'         for any that are `across time`, the output is:
#'             `site` | `time_start` | `time_increment` | `variable` | `ct_denom` | `concept_id` | `ct_concept` | `prop_concept`
#'
#' @import argos
#' @import ssdqa.gen
#' @importFrom stringr str_wrap
#'
#' @export
#'
csd_process <- function(cohort,
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
  output_type <- suppressWarnings(param_summ(check_string = 'csd',
                                             as.list(environment())))

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

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL) %>%
    group_by(!!! syms(grouped_list))

  # Execute function
  if(! time) {


    for(k in 1:length(site_list_adj)) {

      site_list_thisrnd <- site_list_adj[[k]]

      # filters by site
      cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

      if(multi_or_single_site=='single' & anomaly_or_exploratory=='anomaly') {
        variable_compute <- check_code_dist_ssanom(cohort_codedist = cohort_site,
                                                   concept_set = concept_set,
                                                   domain_tbl = domain_tbl,
                                                   num_concept_combined = num_concept_combined,
                                                   num_concept_1 = num_concept_1,
                                                   num_concept_2 = num_concept_2)
      } else {
        variable_compute <- check_code_dist_csd(cohort_codedist = cohort_site,
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


    cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL) %>%
      group_by(!!! syms(grouped_list))

    csd_tbl <- compute_fot(cohort = cohort_prep,
                           site_list = site_list_adj,
                           site_col = site_col,
                           time_span = time_span,
                           time_period = time_period,
                           reduce_id = NULL,
                           check_func = function(dat){
                             check_code_dist_csd(cohort_codedist = dat,
                                                 concept_set = concept_set,
                                                 domain_tbl = domain_tbl,
                                                 time = TRUE)
                           })

    if(multi_or_single_site == 'multi' & anomaly_or_exploratory=='anomaly') {

      lookup <- csd_tbl %>% ungroup() %>% distinct(variable, concept_id)

      csd_tbl_ms <- ms_anom_euclidean(fot_input_tbl = csd_tbl,
                                      grp_vars = c('site', 'concept_id'),
                                      var_col = 'prop_concept')

      csd_tbl_final <- csd_tbl_ms %>% left_join(lookup)

    }else if(multi_or_single_site == 'single' & anomaly_or_exploratory=='anomaly'){

      csd_tbl_ss <- anomalize_ss_anom_at(fot_input_tbl = csd_tbl,
                                         grp_vars = 'concept_id',
                                         time_var = 'time_start',
                                         var_col = 'prop_concept')

      csd_tbl_final <- csd_tbl_ss

    }else{csd_tbl_final <- csd_tbl}

  }

  cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output function in csd_output: ', output_type, '.')))

  return(csd_tbl_final %>% replace_site_col())


}
