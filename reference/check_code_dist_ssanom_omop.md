# Base CSD function only for `single site, anomaly, no time`

Base CSD function only for `single site, anomaly, no time`

## Usage

``` r
check_code_dist_ssanom_omop(
  cohort_codedist,
  concept_set,
  num_concept_combined = FALSE,
  num_concept_1 = 30,
  num_concept_2 = 30,
  domain_tbl = conceptsetdistribution::csd_domain_file
)
```

## Arguments

- cohort_codedist:

  the cohort to pass in

- concept_set:

  the concept set passed in through `csd_process`; concept set CSV file
  with the following columns: `concept_id` \| `concept_code` \|
  `concept_name` \| `vocabulary_id` \| `category` \| `variable` \|
  `domain` The variable field is required to categorize each concept set
  into a particular variable The domain is required so that the function
  knows which table to join to in order to derive counts

- num_concept_combined:

  when `mult_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is an integer and will ensure that `concept1`
  and `concept2` meet some minimal threshold for including in the
  jaccard index; if `TRUE`, then *both* conditions for `num_concept_1`
  and `num_concept_2` should be met; if `FALSE` then just one condition
  needs to be met.

- num_concept_1:

  when `mult_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is an integer and requires a minimum number
  of times that the *first* concept appears in the dataset

- num_concept_2:

  when `mult_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is an integer and requires a minimum number
  of times that the *second* concept appears in the dataset

- domain_tbl:

  domain table passed in through `csd_process`; tbl that is similar to
  the SCV check; four columns: `domain` \| `source_col` \| `concept_col`
  \| `date_col`; the required columns for the csd check are only
  `domain_tbl`, `concept_col`, `date_col`

## Value

the jaccard index of two different concepts for a given variable
