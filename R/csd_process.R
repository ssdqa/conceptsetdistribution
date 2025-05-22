
#' Concept Set Distribution
#'
#' This is an information representation module that will compute the distribution of concept usage in a
#' user-provided annotated concept set. The user will define the domains of interest (`domain_tbl`) and
#' provide the concept set to be evaluated (`concept_set`). Sample versions of these inputs are
#' included as data in the package and are accessible with `conceptsetdistribution::`.
#' Results can optionally be stratified by site, age group, and/or time.
#' This function is compatible with both the OMOP and the PCORnet CDMs based on the
#' user's selection.
#'
#' @param cohort *tabular input* | cohort for SQUBA testing; required fields:
#' - `site` | *character*
#' - `person_id` | `patid` | *integer* / *character*
#' - `start_date` | *date*
#' - `end_date` | *date*
#' @param domain_tbl *tabular input* | input table defining the domains listed in the annotated concept set
#'                   four columns:
#'                   - `domain` the name of the CDM table associated with the concept; should match what is listed in the annotated concept set
#'                   - `concept_field` the name of the field in the domain table where the concepts are located
#'                   - `date_field` the name of the field in the domain table with the date that should be used for time-based filtering
#'                   - `vocabulary_field` PCORnet only; set to NA
#' @param concept_set *tabular input* | an annotated concept set CSV file with the following columns:
#' - `concept_id` | *integer* |  required for OMOP; the concept_id of interest
#' - `concept_code` | *character* | required for PCORnet; the code of interest
#' - `concept_name` | *character* | optional; the descriptive name of the concept
#' - `vocabulary_id` | *character* | required for PCORnet; the vocabulary of the code - should match what is listed in the domain table's vocabulary_field
#' - `variable` | *character* | required; a string label grouping one concept code into a larger variable definition
#' - `domain` | *character* | required; the name of the CDM table where the concept is stored - multiple domains can be included in the file, but only one domain should be listed per row
#' @param omop_or_pcornet *string* | Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [csd_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [csd_process_pcornet()] function against a PCORnet CDM instance
#' @param multi_or_single_site *string* | Option to run the function on a single vs multiple sites
#' - `single`: run the function for a single site
#' - `multi`: run the function for multiple sites
#' @param anomaly_or_exploratory *string* | Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#' level summary of the data to examine the fact representation within the cohort. Anomaly detection
#' analyses are specialized to identify outliers within the cohort.
#' @param num_concept_combined *boolean* | when `multi_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#' this argument is a boolean that will ensure that `concept1` and `concept2` meet some minimal threshold for including in the jaccard index
#' if `TRUE`, then *both* conditions for `num_concept_1` and `num_concept_2` should be met; if `FALSE` then just one condition needs to be met.
#' @param num_concept_1  *integer* | when `multi_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#' this argument is an integer and requires a minimum number of times that the *first* concept appears in the dataset
#' @param num_concept_2 *integer* | when `multi_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#' this argument is an integer and requires a minimum number of times that the *second* concept appears in the dataset
#' @param p_value *numeric* | the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param age_groups *tabular input* | If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#' - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#' - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param time *boolean* | logical to determine whether to output the check across time
#' @param time_span *vector - length 2* | when time = TRUE, a vector of two dates for the observation period of the study
#' @param time_period *string* | when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe summarizing the distribution of code usage for each user defined variable
#'
#' @import argos
#' @import squba.gen
#' @importFrom stringr str_wrap
#'
#' @example inst/example-csd_process_output.R
#'
#' @export
#'
csd_process <- function(cohort,
                        domain_tbl= conceptsetdistribution::csd_domain_file,
                        concept_set = conceptsetdistribution::csd_concept_set,
                        omop_or_pcornet,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        num_concept_combined = FALSE,
                        num_concept_1 = 30,
                        num_concept_2 = 30,
                        p_value = 0.9,
                        age_groups = FALSE,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'csd',
                                             as.list(environment())))

  if(tolower(omop_or_pcornet) == 'omop'){

    csd_rslt <- csd_process_omop(cohort = cohort,
                                 domain_tbl= domain_tbl,
                                 concept_set = concept_set,
                                 multi_or_single_site = multi_or_single_site,
                                 anomaly_or_exploratory = anomaly_or_exploratory,
                                 num_concept_combined = num_concept_combined,
                                 num_concept_1 = num_concept_1,
                                 num_concept_2 = num_concept_2,
                                 p_value = p_value,
                                 age_groups = age_groups,
                                 time = time,
                                 time_span = time_span,
                                 time_period = time_period)

  }else if(tolower(omop_or_pcornet) == 'pcornet'){

    csd_rslt <- csd_process_pcornet(cohort = cohort,
                                    domain_tbl= domain_tbl,
                                    concept_set = concept_set,
                                    multi_or_single_site = multi_or_single_site,
                                    anomaly_or_exploratory = anomaly_or_exploratory,
                                    num_concept_combined = num_concept_combined,
                                    num_concept_1 = num_concept_1,
                                    num_concept_2 = num_concept_2,
                                    p_value = p_value,
                                    age_groups = age_groups,
                                    time = time,
                                    time_span = time_span,
                                    time_period = time_period)

  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  rslt_with_opt <- csd_rslt %>% mutate(output_function = output_type$string)

  cli::boxx(c('You can optionally use this dataframe in the accompanying',
  '`csd_output` function. Here are the parameters you will need:', '', output_type$vector, '',
  'See ?csd_output for more details.'), padding = c(0,1,0,1),
  header = cli::col_cyan('Output Function Details'))

  return(rslt_with_opt)


}
