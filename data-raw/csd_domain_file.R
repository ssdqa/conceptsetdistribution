## code to prepare `csd_domain_file` dataset goes here

csd_domain_file <- tibble::tibble(domain = c('diagnosis', 'drug_exposure', 'measurement'),
                                  concept_field = c('dx', 'drug_concept_id', 'measurement_concept_id'),
                                  date_field = c('admit_date', 'drug_exposure_start_date', 'measurement_date'),
                                  vocabulary_field = c('dx_type',NA,NA))

usethis::use_data(csd_domain_file, overwrite = TRUE)
