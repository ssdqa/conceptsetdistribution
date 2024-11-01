
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(csd_output(process_output = tbl_test,
                          output_function = 'csd_test'))
})

test_that('ss exp nt', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a'),
                            variable = c('test', 'test', 'test'),
                            concept_id = c(1,2,3),
                            ct_denom = c(100,100,100),
                            ct_concept = c(50,25,25),
                            prop_concept = c(0.5, 0.25, 0.25))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ss_exp_nt',
                             vocab_tbl = NULL,
                             concept_set = NULL))

})

test_that('ms exp nt', {

  tbl_test <- tidyr::tibble(site = c('a', 'b', 'c'),
                            variable = c('test', 'test', 'test'),
                            concept_id = c(1,2,3),
                            ct_denom = c(100,100,100),
                            ct_concept = c(50,25,25),
                            prop_concept = c(0.5, 0.25, 0.25))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ms_exp_nt',
                             vocab_tbl = NULL,
                             concept_set = NULL))

})

test_that('ss anom nt', {

  tbl_test <- tidyr::tibble('concept1' = 1,
                            'concept2' = 2,
                            'cocount' = 50,
                            'concept1_ct' = 75,
                            'concept2_ct' = 100,
                            'concept_count_union' = 65,
                            'jaccard_index' = 0.8,
                            'concept1_prop' = 0.6,
                            'concept2_prop' = 0.5,
                            'variable' = 'test',
                            'var_jaccard_mean' = 0.8,
                            'var_jaccard_sd' = 0.1,
                            'above_sd' = TRUE)

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ss_anom_nt',
                             vocab_tbl = NULL,
                             concept_set = NULL))

})

test_that('ms exp nt', {

  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c'),
                            'variable' = c('test', 'test', 'test'),
                            'concept_id' = c(1,2,3),
                            'ct_denom' = c(100,100,100),
                            'ct_concept' = c(50,25,25),
                            'prop_concept' = c(0.5, 0.25, 0.25),
                            'mean_val' = c(0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3),
                            'analysis_eligible' = c('yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier'))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ms_exp_nt',
                             vocab_tbl = NULL,
                             concept_set = NULL))

})


test_that('ss exp at', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a'),
                            time_start = c('2009-01-01', '2010-01-01', '2011-01-01'),
                            time_increment = c('year', 'year', 'year'),
                            variable = c('test', 'test', 'test'),
                            concept_id = c(1,1,1),
                            ct_denom = c(100,100,100),
                            ct_concept = c(50,25,25),
                            prop_concept = c(0.5, 0.25, 0.25))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ss_exp_at',
                             filtered_var = 'test',
                             vocab_tbl = NULL,
                             concept_set = NULL))

})

test_that('ms exp at', {

  tbl_test <- tidyr::tibble(site = c('a', 'a', 'a', 'b', 'b', 'b'),
                            time_start = c('2009-01-01', '2010-01-01', '2011-01-01',
                                           '2009-01-01', '2010-01-01', '2011-01-01'),
                            time_increment = c('year', 'year', 'year',
                                               'year', 'year', 'year'),
                            variable = c('test', 'test', 'test',
                                         'test', 'test', 'test'),
                            concept_id = c(1,1,1,1,1,1),
                            ct_denom = c(100,100,100,100,100,100),
                            ct_concept = c(50,25,25,50,25,25),
                            prop_concept = c(0.5, 0.25, 0.25,0.5,0.25,0.25))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ms_exp_at',
                             filtered_var = 'test',
                             filter_concept = 1,
                             vocab_tbl = NULL,
                             concept_set = NULL))

})

test_that('ss anom at', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a'),
                            'time_start' = c('2009-01-01', '2010-01-01', '2011-01-01'),
                            'time_increment' = c('year', 'year', 'year'),
                            'variable' = c('test', 'test', 'test'),
                            'concept_id' = c(1,1,1),
                            'ct_denom' = c(100,100,100),
                            'ct_concept' = c(50,25,25),
                            'prop_concept' = c(0.5, 0.25, 0.25),
                            'observed' = c(0.5, 0.6, 0.7),
                            'season' = c(1,2,3),
                            'trend' = c(1,2,3),
                            'remainder' = c(0.46, 0.57, 0.69),
                            'seasonadj' = c(1,2,3),
                            'anomaly' = c('Yes', 'No', 'Yes'),
                            'anomaly_direction' = c(-1,0,1),
                            'anomaly_score' = c(1,2,3),
                            'recomposed_l1' = c(0.44, 0.6, 0.5),
                            'recomposed_l2' = c(0.84, 0.8, 0.8),
                            'observed_clean' = c(0.46, 0.57, 0.69))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ss_anom_at',
                             filtered_var = 'test',
                             filter_concept = 1,
                             vocab_tbl = NULL,
                             concept_set = NULL))

})


test_that('ms exp at', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'b', 'b', 'b'),
                            'time_start' = c('2009-01-01', '2010-01-01', '2011-01-01',
                                           '2009-01-01', '2010-01-01', '2011-01-01'),
                            'time_increment' = c('year', 'year', 'year',
                                               'year', 'year', 'year'),
                            'variable' = c('test', 'test', 'test',
                                         'test', 'test', 'test'),
                            'concept_id' = c(1,1,1,1,1,1),
                            'ct_denom' = c(100,100,100,100,100,100),
                            'ct_concept' = c(50,25,25,50,25,25),
                            'prop_concept' = c(0.5, 0.25, 0.25,0.5,0.25,0.25),
                            'mean_allsiteprop' = c(0.83, 0.83, 0.83, 0.83, 0.83, 0.83),
                            'median' = c(0.87, 0.87, 0.87, 0.87, 0.87, 0.87),
                            'date_numeric' = c(17000, 17000, 17000, 17000, 17000, 17000),
                            'site_loess' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73),
                            'dist_eucl_mean' = c(0.84,0.84,0.84,0.84,0.84,0.9))

  expect_no_error(csd_output(process_output = tbl_test,
                             output_function = 'csd_ms_anom_at',
                             filtered_var = 'test',
                             filter_concept = 1,
                             vocab_tbl = NULL,
                             concept_set = NULL))

})
