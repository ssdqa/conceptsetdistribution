# Concept Set Distribution - OMOP Version

Concept Set Distribution - OMOP Version

## Usage

``` r
csd_process_omop(
  cohort,
  domain_tbl = conceptsetdistribution::csd_domain_file,
  concept_set = conceptsetdistribution::csd_concept_set,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  num_concept_combined = FALSE,
  num_concept_1 = 30,
  num_concept_2 = 30,
  p_value = 0.9,
  age_groups = FALSE,
  time = TRUE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  cohort for SQUBA testing; required fields:

  - `site`

  - `person_id`

  - `start_date`

  - `end_date`

- domain_tbl:

  input table defining the domains listed in the annotated concept set
  four columns: - `domain` the name of the CDM table associated with the
  concept; should match what is listed in the annotated concept set -
  `concept_field` the name of the field in the domain table where the
  concepts are located - `date_field` the name of the field in the
  domain table with the date that should be used for time-based
  filtering - `vocabulary_field` PCORnet only; set to NA

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

- multi_or_single_site:

  Option to run the function on a single vs multiple sites

  - `single`: run the function for a single site

  - `multi`: run the function for multiple sites

- anomaly_or_exploratory:

  Option to conduct an exploratory or anomaly detection analysis.
  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- num_concept_combined:

  when `multi_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is a boolean that will ensure that `concept1`
  and `concept2` meet some minimal threshold for including in the
  jaccard index if `TRUE`, then *both* conditions for `num_concept_1`
  and `num_concept_2` should be met; if `FALSE` then just one condition
  needs to be met.

- num_concept_1:

  when `multi_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is an integer and requires a minimum number
  of times that the *first* concept appears in the dataset

- num_concept_2:

  when `multi_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is an integer and requires a minimum number
  of times that the *second* concept appears in the dataset

- p_value:

  the p value to be used as a threshold in the multi-site anomaly
  detection analysis

- age_groups:

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and include it as the
  `age_groups` function parameter:

  - `min_age`: the minimum age for the group (i.e. 10)

  - `max_age`: the maximum age for the group (i.e. 20)

  - `group`: a string label for the group (i.e. 10-20, Young Adult,
    etc.)

  If you would *not* like to stratify by age group, leave the argument
  as NULL

- time:

  logical to determine whether to output the check across time

- time_span:

  when time = TRUE, a vector of two dates for the observation period of
  the study

- time_period:

  when time = TRUE, this argument defines the distance between dates
  within the specified time period. defaults to `year`, but other time
  periods such as `month` or `week` are also acceptable

## Value

a dataframe summarizing the distribution of code usage for each user
defined variable
