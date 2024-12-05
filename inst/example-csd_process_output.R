
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
  dplyr::mutate(start_date = as.Date(-5000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Execute `csd_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
csd_process_example <- csd_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   domain_tbl = csd_domain_file,
                                   concept_set = csd_concept_set)

csd_process_example

#' Execute `csd_output` function
csd_output_example <- csd_output(process_output = csd_process_example,
                                 output_function = 'csd_ss_exp_cs',
                                 concept_set = csd_concept_set,
                                 vocab_tbl = NULL)

csd_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_ssdqa()`

make_interactive_ssdqa(csd_output_example)
