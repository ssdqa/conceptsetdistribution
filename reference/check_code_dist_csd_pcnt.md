# Base CSD function

Base CSD function

## Usage

``` r
check_code_dist_csd_pcnt(
  cohort_codedist,
  concept_set,
  time = FALSE,
  time_span,
  time_period,
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

- time:

  logical to determine whether to output the check across time

- time_span:

  when `time = TRUE`, a vector of two dates for the observation period
  of the study

- time_period:

  when time = TRUE, this argument defines the distance between dates
  within the specified time period. defaults to `year`, but other time
  periods such as `month` or `week` are also acceptable

- domain_tbl:

  domain table passed in through `csd_process`; tbl that is similar to
  the SCV check; four columns: `domain` \| `source_col` \| `concept_col`
  \| `date_col`; the required columns for the csd check are only
  `domain_tbl`, `concept_col`, `date_col`

## Value

returns variable and their concept mappings, both in counts and in
proportions; when `time = TRUE`, then output is given across time, and
proportions computed within each variable
