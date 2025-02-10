

#' Base CSD function
#'
#' @param cohort_codedist the cohort to pass in
#' @param concept_set the concept set passed in through `csd_process`;
#'                    concept set CSV file with the following columns:
#'                    `concept_id` | `concept_code` | `concept_name` | `vocabulary_id` | `category` | `variable` | `domain`
#'                    The variable field is required to categorize each concept set into a particular variable
#'                    The domain is required so that the function knows which table to join to in order to derive counts
#' @param domain_tbl domain table passed in through `csd_process`;
#'                    tbl that is similar to the SCV check;
#'                   four columns: `domain` | `source_col` | `concept_col` | `date_col`;
#'                   the required columns for the csd check are only `domain_tbl`, `concept_col`, `date_col`
#' @param time logical to determine whether to output the check across time
#' @param time_span when `time = TRUE`, a vector of two dates for the observation period of the study
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return returns variable and their concept mappings, both in counts and in proportions;
#'         when `time = TRUE`, then output is given across time, and proportions computed within each variable
#'
check_code_dist_csd_pcnt <- function(cohort_codedist,
                                     concept_set,
                                     time = FALSE,
                                     time_span,
                                     time_period,
                                     domain_tbl = conceptsetdistribution::csd_domain_file){


  domain_filter <-
    concept_set %>% select(domain) %>% distinct() %>%
    inner_join(domain_tbl)
  concept_set_db <- copy_to_new(df=concept_set, name='concept_set')

  fact_tbl_final <- list()

  for(i in 1:nrow(domain_filter)) {

    dates <- domain_filter$date_field[[i]]

    domain_tbl_name <- domain_filter$domain[[i]]
    domain_tbl_cdm <- cohort_codedist %>%
      inner_join(cdm_tbl(domain_tbl_name)) %>%
      filter(!!sym(dates) >= start_date,
             !!sym(dates) <= end_date)
    final_col <- domain_filter$concept_field[[i]]
    vocab_col <- domain_filter$vocabulary_field[[i]]

    join_cols <- set_names('concept_code', final_col)

    if(!is.na(vocab_col)){
      join_cols2 <- set_names('vocabulary_id', vocab_col)
      join_cols <- join_cols %>% append(join_cols2)
    }

    if(time){
      fact_tbl <-
        domain_tbl_cdm %>%
        filter(!!sym(dates) >= time_start,
               !!sym(dates) <= time_end) %>%
        inner_join(concept_set_db,
                   by=join_cols) %>%
        select(all_of(group_vars(cohort_codedist)),
               all_of(final_col),
               variable,
               time_start,
               time_increment) %>%
        group_by(time_start,
                 time_increment,
                 .add=TRUE) %>%
        rename('concept_code' = final_col)
    } else {
      fact_tbl <-
        domain_tbl_cdm %>%
        inner_join(concept_set_db,
                   by=join_cols) %>%
        select(all_of(group_vars(cohort_codedist)),
               all_of(final_col),
               variable) %>%
        rename('concept_code' = final_col)

    }

    cts <-
      fact_tbl %>%
      group_by(
        concept_code,
        variable,
        .add=TRUE
      ) %>%
      summarise(ct_concept=n()) %>%
      collect()

    fact_tbl_final[[i]] <- cts
  }

  fact_tbl_final_reduce <-
    reduce(.x = fact_tbl_final,
           .f= dplyr::union)

  denom <-
    fact_tbl_final_reduce %>%
    ungroup(concept_code) %>%
    group_by(variable,
             .add=TRUE) %>%
    summarise(ct_denom=sum(ct_concept)) %>%
    collect()

  props <-
    denom %>%
    inner_join(fact_tbl_final_reduce, multiple='all') %>%
    mutate(prop_concept = round(ct_concept/ct_denom, 2),
           concept_code = as.character(concept_code))


}


#' Base CSD function only for `single site, anomaly, no time`
#'
#' @param cohort_codedist the cohort to pass in
#' @param concept_set the concept set passed in through `csd_process`;
#'                    concept set CSV file with the following columns:
#'                    `concept_id` | `concept_code` | `concept_name` | `vocabulary_id` | `category` | `variable` | `domain`
#'                    The variable field is required to categorize each concept set into a particular variable
#'                    The domain is required so that the function knows which table to join to in order to derive counts
#' @param domain_tbl domain table passed in through `csd_process`;
#'                    tbl that is similar to the SCV check;
#'                   four columns: `domain` | `source_col` | `concept_col` | `date_col`;
#'                   the required columns for the csd check are only `domain_tbl`, `concept_col`, `date_col`
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
#'
#' @return the jaccard index of two different concepts for a given variable
#'
check_code_dist_ssanom_pcnt <- function(cohort_codedist,
                                   concept_set,
                                   num_concept_combined = FALSE,
                                   num_concept_1 = 30,
                                   num_concept_2 = 30,
                                   domain_tbl = conceptsetdistribution::csd_domain_file){


  domain_filter <-
    concept_set %>% select(domain) %>% distinct() %>%
    inner_join(domain_tbl)
  concept_set_db <- copy_to_new(df=concept_set, name='concept_set')
  variable_list <- concept_set_db %>% distinct(variable) %>% pull()

      variable_summary <- list()

    for(i in variable_list) {

      variable_filtered <-
        concept_set_db %>% filter(variable == i)

      domain_num <-
        variable_filtered %>% select(domain) %>% distinct() %>% pull()

      variable_combined <- list()

      for(n in 1:length(domain_num)) {

        domain_name <- domain_num[[n]]

        final_col <-
          domain_filter %>%
          filter(domain == domain_name) %>%
          select(concept_field) %>% pull()

        date_col <-
          domain_filter %>%
          filter(domain == domain_name) %>%
          select(date_field) %>% pull()

        join_cols <- set_names('concept_code', final_col)

        vocab_col <-
          domain_filter %>%
          filter(domain == domain_name) %>%
          select(vocabulary_field) %>% pull()

        if(!is.na(vocab_col)){

          join_cols2 <- set_names('vocabulary_id', vocab_col)
          join_cols <- join_cols %>% append(join_cols2)

        }

        one_domain_tbl <-
          cohort_codedist %>%
          inner_join(
            cdm_tbl(domain_name)
          ) %>%
          filter(!!sym(date_col) >= start_date,
                 !!sym(date_col) <= end_date) %>%
          inner_join(variable_filtered,
                     by=join_cols) %>%
          select(patid,
                 all_of(group_vars(cohort_codedist)),
                 all_of(final_col),
                 variable) %>%
          rename('concept_code'=final_col) %>%
          mutate(domain = domain_name) %>%
          group_by(patid,concept_code, variable, domain,
                   .add=TRUE) %>%
          summarise(ct=n()) %>%
          compute_new(temporary=TRUE)

        variable_combined[[n]] <- one_domain_tbl

      }

      variable_flattened <- reduce(.x=variable_combined,
                                   .f=dplyr::union)

      row_check <- tally(ungroup(variable_flattened)) %>% pull()

      if(row_check != 0){
        var_domain_lookup <-
          variable_flattened %>%
          ungroup %>% select(concept_code,variable) %>% distinct() %>%  collect()

        jaccards <- compute_jaccard(variable_flattened,
                                    var_col = 'concept_code',
                                    omop_or_pcornet = 'pcornet') %>%
          mutate(variable = i)

        variable_summary[[i]] <- jaccards
      }else{
        cli::cli_inform('No variable matches -- skipping to next')
        next
      }

    }

      combined <- reduce(.x=variable_summary,
                         .f=dplyr::union)

      if(! num_concept_combined) {
        combined_filtered <-
          combined %>%
          filter(concept1_ct > num_concept_1 | concept2_ct > num_concept_2)
      } else {combined_filtered <-
        combined %>%
        filter(concept1_ct > num_concept_1,
             concept2_ct > num_concept_2)}

      x_vars_meansd <-
        combined_filtered %>%
        group_by(variable) %>%
        summarise(var_jaccard_mean=mean(jaccard_index),
                  var_jaccard_sd=sd(jaccard_index))

      tbl_input <-
        combined_filtered %>%
        inner_join(x_vars_meansd) %>%
        mutate(above_sd=
                 case_when(jaccard_index > (var_jaccard_mean + var_jaccard_sd) ~ TRUE,
                           TRUE ~ FALSE)) %>%
        mutate(across(where(is.double), \(x) round(x, digits=3)))



}
