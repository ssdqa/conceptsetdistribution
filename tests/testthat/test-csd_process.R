## Testing error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(csd_process(cohort = cht,
                           multi_or_single_site = 'test',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'omop'))
})


test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(csd_process(cohort = cht,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'test',
                           omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(csd_process(cohort = cht,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'test'))
})


## Generally checking that code runs
test_that('csd ss/ms exp nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'csd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  csd_vars <- tidyr::tibble('domain' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date')

  csd_concept <- read_codeset('dx_hypertension')

  expect_no_error(csd_process(cohort = cohort,
                              domain_tbl = csd_vars,
                              concept_set = csd_concept,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              omop_or_pcornet = 'omop'))
})

test_that('csd ss anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'csd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  csd_vars <- tidyr::tibble('domain' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date')

  csd_concept <- read_codeset('dx_hypertension')

  expect_no_error(csd_process(cohort = cohort,
                              domain_tbl = csd_vars,
                              concept_set = csd_concept,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop'))
})


test_that('csd ms anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'csd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  csd_vars <- tidyr::tibble('domain' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date')

  csd_concept <- read_codeset('dx_hypertension')

  expect_warning(csd_process(cohort = cohort,
                              domain_tbl = csd_vars,
                              concept_set = csd_concept,
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop'))
})

test_that('csd ss exp at -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'csd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  csd_vars <- tidyr::tibble('domain' = 'condition_occurrence',
                            'concept_field' = 'condition_concept_id',
                            'date_field' = 'condition_start_date')

  csd_concept <- read_codeset('dx_hypertension')

  expect_no_error(csd_process(cohort = cohort,
                              domain_tbl = csd_vars,
                              concept_set = csd_concept,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              time = TRUE,
                              time_period = 'year',
                              time_span = c('1990-01-01', '2000-01-01'),
                              omop_or_pcornet = 'omop'))
})


test_that('csd ss exp nt -- pcornet', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'csd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  csd_vars <- tidyr::tibble('domain' = 'diagnosis',
                            'concept_field' = 'dx',
                            'date_field' = 'admit_date',
                            'vocabulary_field' = 'dx_type')

  csd_concept <- read_codeset('dx_hypertension')

  expect_error(csd_process(cohort = cohort,
                           domain_tbl = csd_vars,
                           concept_set = csd_concept,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'exploratory',
                           time = FALSE,
                           omop_or_pcornet = 'pcornet'))
})
