# Concept Set Distribution

This is an information representation module that will compute the
distribution of concept usage in a user-provided annotated concept set.
The user will define the domains of interest (`domain_tbl`) and provide
the concept set to be evaluated (`concept_set`). Sample versions of
these inputs are included as data in the package and are accessible with
`conceptsetdistribution::`. Results can optionally be stratified by
site, age group, and/or time. This function is compatible with both the
OMOP and the PCORnet CDMs based on the user's selection.

## Usage

``` r
csd_process(
  cohort,
  domain_tbl = conceptsetdistribution::csd_domain_file,
  concept_set = conceptsetdistribution::csd_concept_set,
  omop_or_pcornet,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  num_concept_combined = FALSE,
  num_concept_1 = 30,
  num_concept_2 = 30,
  p_value = 0.9,
  age_groups = NULL,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year"
)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- domain_tbl:

  *tabular input* \|\| **required**

  A table or CSV file defining the domains listed in the annotated
  concept set. This input should contain four columns:

  - `domain` \| *character* \| a string identifying the CDM table, as
    listed in the annotated concept set, where the concept of interest
    can be identified

  - `concept_field` \| *character*\| the string name of the field in the
    domain table where the concepts are located

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `vocabulary_field` \| *character* \| for PCORnet applications, the
    name of the field in the domain table with a vocabulary identifier
    to differentiate concepts from one another (ex: dx_type); can be set
    to NA for OMOP applications

  To see an example of what this input should look like, see
  [`?conceptsetdistribution::csd_domain_file`](https://ssdqa.github.io/conceptsetdistribution/reference/csd_domain_file.md)

- concept_set:

  *tabular input* \|\| **required**

  A table or CSV containing the concepts to be investigated, plus some
  additional metadata. This input should contain one of following:

  - `concept_id` \| *integer* \| the concept_id of interest (required
    for OMOP)

  - `concept_code` \| *character* \| the code of interest (required for
    PCORnet)

  And both of:

  - `variable` \| *character* \| a string label grouping one concept
    code into a larger variable definition

  - `domain` \| *character* \| the name of the CDM table where the
    concept can be found

  For certain PCORnet applications, it should also contain

  - `vocabulary_id` \| *character* \| the vocabulary of the code, which
    should match what is listed in the domain table's `vocabulary_field`

  To see an example of what this input should look like, see
  [`?conceptsetdistribution::csd_concept_set`](https://ssdqa.github.io/conceptsetdistribution/reference/csd_concept_set.md)

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`csd_process_omop()`](https://ssdqa.github.io/conceptsetdistribution/reference/csd_process_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`csd_process_pcornet()`](https://ssdqa.github.io/conceptsetdistribution/reference/csd_process_pcornet.md)
    function against a PCORnet CDM instance

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \|\| defaults to `exploratory`

  A string, either `anomaly` or `exploratory`, indicating what type of
  results should be produced.

  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- num_concept_combined:

  *boolean* \|\| defaults to `FALSE`

  When `multi_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument is a boolean that will ensure that `concept1`
  and `concept2` meet some minimal threshold for inclusion in the
  Jaccard index computation.

  if `TRUE`, then *both* conditions for `num_concept_1` and
  `num_concept_2` should be met; if `FALSE` then just one condition
  needs to be met.

- num_concept_1:

  *integer* \|\| defaults to `30`

  When `multi_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument indicates the minimum number of times that
  the *first* concept appears in the dataset during the Jaccard index
  computation

- num_concept_2:

  *integer* \|\| defaults to `30`

  When `multi_or_single_site` = `single` and `anomaly_or_exploratory` =
  `anomaly`, this argument indicates the minimum number of times that
  the *second* concept appears in the dataset during the Jaccard index
  computation

- p_value:

  *numeric* \|\| defaults to `0.9`

  The p value to be used as a threshold in the Multi-Site, Anomaly
  Detection, Cross-Sectional analysis

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

## Value

This function will return a dataframe summarizing the distribution of
code usage for each user defined variable. For a more detailed
description of output specific to each check type, see the PEDSpace
metadata repository

## Examples

``` r
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
#> Connected to: :memory:@NA
#> To see environment settings, run `get_argos_default()`

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
#> Error: '/home/runner/work/conceptsetdistribution/conceptsetdistribution/docs/reference//home/runner/work/_temp/Library/conceptsetdistribution/extdata/dx_hypertension.csv' does not exist.

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
#> Error: object 'csd_concept_tbl' not found

csd_process_example
#> Error: object 'csd_process_example' not found

#' Execute `csd_output` function
csd_output_example <- csd_output(process_output = csd_process_example,
                                 concept_set = csd_concept_tbl,
                                 vocab_tbl = NULL)
#> Error: object 'csd_process_example' not found

csd_output_example
#> Error: object 'csd_output_example' not found

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(csd_output_example[[1]])
#> Error: object 'csd_output_example' not found
```
