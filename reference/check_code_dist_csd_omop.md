# Base CSD function

Base CSD function

## Usage

``` r
check_code_dist_csd_omop(
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

  the cohort of interest

- concept_set:

  an annotated concept set CSV file with the following columns:

  - `concept_id` required for OMOP; the concept_id of interest

  - `concept_code` required for PCORnet; the code of interest

  - `concept_name` optional; the descriptive name of the concept

  - `vocabulary_id` required for PCORnet; the vocabulary of the code -
    should match what is listed in the domain table's vocabulary_field

  - `variable` required; a string label grouping one concept code into a
    larger variable definition

  - `domain` required; the name of the CDM table where the concept is
    stored - multiple domains can be included in the file, but only one
    domain should be listed per row

- time:

  logical to determine whether to output the check across time

- time_span:

  when time = TRUE, a vector of two dates for the observation period of
  the study

- time_period:

  when time = TRUE, this argument defines the distance between dates
  within the specified time period. defaults to `year`, but other time
  periods such as `month` or `week` are also acceptable

- domain_tbl:

  input table defining the domains listed in the annotated concept set
  four columns: - `domain` the name of the CDM table associated with the
  concept; should match what is listed in the annotated concept set -
  `concept_field` the name of the field in the domain table where the
  concepts are located - `date_field` the name of the field in the
  domain table with the date that should be used for time-based
  filtering - `vocabulary_field` PCORnet only; set to NA

## Value

returns variable and its concept mappings, both in counts and in
proportions; when time = TRUE, then output is given across time, and
proportions computed within each variable
