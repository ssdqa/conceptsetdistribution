## code to prepare `csd_domain_file` dataset goes here

csd_domain_file <- tibble::tibble(domain = c('condition_occurrence', 'drug_exposure', 'measurement'),
                                  concept_field = c('condition_concept_id', 'drug_concept_id', 'measurement_concept_id'),
                                  date_field = c('condition_start_date', 'drug_exposure_start_date', 'measurement_date'),
                                  vocabulary_field = c(NA,NA,NA))

usethis::use_data(csd_domain_file, overwrite = TRUE)
