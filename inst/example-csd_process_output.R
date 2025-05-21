
#' Source setup file
source(system.file('setup.R', package = 'conceptsetdistribution'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'csd_process_test',
                      working_directory = getwd(),
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = system.file('extdata',
                                        package = 'conceptsetdistribution'),
                      cdm_schema = NA)

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Prepare input tables
csd_domain_tbl <- dplyr::tibble(domain = 'condition_occurrence',
                                concept_field = 'condition_concept_id',
                                date_field = 'condition_start_date',
                                vocabulary_field = NA)

csd_concept_tbl <- read_codeset('dx_hypertension') %>%
  dplyr::mutate(domain = 'condition_occurrence',
                variable = 'hypertension')

#' Execute `csd_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
csd_process_example <- csd_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   domain_tbl = csd_domain_tbl,
                                   concept_set = csd_concept_tbl)

csd_process_example

#' Execute `csd_output` function
csd_output_example <- csd_output(process_output = csd_process_example,
                                 concept_set = csd_concept_tbl,
                                 vocab_tbl = NULL)

csd_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(csd_output_example)
