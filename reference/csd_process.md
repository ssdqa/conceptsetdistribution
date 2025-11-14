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

getwd()
#> [1] "/home/runner/work/conceptsetdistribution/conceptsetdistribution/docs/reference"

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'csd_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
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

#' Execute `csd_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
csd_process_example <- csd_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   domain_tbl = csd_domain_tbl,
                                   concept_set = csd_concept_tbl) %>%
  suppressMessages()
#> ┌ Output Function Details ──────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying     │
#> │ `csd_output` function. Here are the parameters you will need: │
#> │                                                               │
#> │ Always Required: process_output                               │
#> │ Required for Check: num_variables, num_mappings               │
#> │ Optional: concept_set, vocab_tbl                              │
#> │                                                               │
#> │ See ?csd_output for more details.                             │
#> └───────────────────────────────────────────────────────────────┘

csd_process_example
#> # A tibble: 1 × 7
#>   variable     ct_denom concept_id ct_concept prop_concept site  output_function
#>   <chr>           <int> <chr>           <int>        <dbl> <chr> <chr>          
#> 1 hypertension        5 320128              5            1 comb… csd_ss_exp_cs  

#' Execute `csd_output` function
csd_output_example <- csd_output(process_output = csd_process_example,
                                 concept_set = csd_concept_tbl,
                                 vocab_tbl = NULL) %>%
  suppressMessages()

csd_output_example[[1]]

csd_output_example[[2]]


  
    Concept Reference Table
    
  
  
  {"x":{"tag":{"name":"Reactable","attribs":{"data":{"site":["combined"],"variable":["hypertension"],"denom_col":[5]},"columns":[{"id":"site","name":"site","type":"character","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"left"},{"id":"variable","name":"variable","type":"character","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","html":true,"align":"left"},{"id":"denom_col","name":"Total Count","type":"numeric","na":"NA","minWidth":125,"style":"function(rowInfo, colInfo) {\nconst rowIndex = rowInfo.index + 1\nif (colInfo.id === 'site' & rowIndex === 1) {\n  return { backgroundColor: '#FF4D6F', color: '#FFFFFF' }\n}\n\n}","cell":["5"],"html":true,"align":"right"}],"searchable":true,"defaultPageSize":10,"showPageSizeOptions":false,"pageSizeOptions":[10,25,50,100],"paginationType":"numbers","showPagination":true,"showPageInfo":true,"minRows":1,"height":"auto","theme":{"color":"#333333","backgroundColor":"#FFFFFF","stripedColor":"rgba(128,128,128,0.05)","style":{"font-family":"system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif","fontSize":"16px"},"tableStyle":{"borderTopStyle":"solid","borderTopWidth":"2px","borderTopColor":"#D3D3D3"},"headerStyle":{"fontWeight":"normal","backgroundColor":"transparent","borderBottomStyle":"solid","borderBottomWidth":"2px","borderBottomColor":"#D3D3D3"},"groupHeaderStyle":{"fontWeight":"normal","backgroundColor":"transparent","borderBottomStyle":"solid","borderBottomWidth":"2px","borderBottomColor":"#D3D3D3"},"cellStyle":{"fontWeight":"normal"}},"elementId":"uzbiykapwo","dataKey":"2e9ecd14f91854aca304e1b0fda02dbc"},"children":[]},"class":"reactR_markup"},"evals":["tag.attribs.columns.0.style","tag.attribs.columns.1.style","tag.attribs.columns.2.style"],"jsHooks":[]}


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(csd_output_example[[1]])

{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='ggiraph-svg' role='graphics-document' id='svg_6a1fd3da403bf818' viewBox='0 0 432 360'>\n <defs id='svg_6a1fd3da403bf818_defs'>\n  <clipPath id='svg_6a1fd3da403bf818_c1'>\n   <rect x='0' y='0' width='432' height='360'/>\n  <\/clipPath>\n  <clipPath id='svg_6a1fd3da403bf818_c2'>\n   <rect x='57.03' y='49.84' width='290.82' height='268.95'/>\n  <\/clipPath>\n  <clipPath id='svg_6a1fd3da403bf818_c3'>\n   <rect x='57.03' y='23.32' width='290.82' height='26.52'/>\n  <\/clipPath>\n <\/defs>\n <g id='svg_6a1fd3da403bf818_rootg' class='ggiraph-svg-rootg'>\n  <g clip-path='url(#svg_6a1fd3da403bf818_c1)'>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round' class='ggiraph-svg-bg'/>\n   <rect x='0' y='0' width='432' height='360' fill='#FFFFFF' fill-opacity='1' stroke='none'/>\n  <\/g>\n  <g clip-path='url(#svg_6a1fd3da403bf818_c2)'>\n   <polyline points='57.03,184.31 347.85,184.31' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='202.44,318.79 202.44,49.84' fill='none' stroke='#EBEBEB' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <rect id='svg_6a1fd3da403bf818_e1' x='81.26' y='72.25' width='242.35' height='224.13' fill='#FBB761' fill-opacity='1' stroke='none' title='Essential hypertension'/>\n   <text x='199.72' y='187.42' font-size='6.4pt' font-family='DejaVu Sans'>1<\/text>\n  <\/g>\n  <g clip-path='url(#svg_6a1fd3da403bf818_c3)'>\n   <text x='173.52' y='35.03' font-size='6.6pt' font-family='DejaVu Sans' fill='#1A1A1A' fill-opacity='1'>hypertension<\/text>\n   <text x='162.52' y='44.54' font-size='6.6pt' font-family='DejaVu Sans' fill='#1A1A1A' fill-opacity='1'> Total Mappings: 1<\/text>\n  <\/g>\n  <g clip-path='url(#svg_6a1fd3da403bf818_c1)'>\n   <text x='173.52' y='330.14' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>hypertension<\/text>\n   <text x='162.52' y='339.64' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'> Total Mappings: 1<\/text>\n   <text x='18.53' y='187.52' font-size='6.6pt' font-family='DejaVu Sans' fill='#4D4D4D' fill-opacity='1'>320128<\/text>\n   <text transform='translate(13.50,214.02) rotate(-90.00)' font-size='8.25pt' font-family='DejaVu Sans'>concept_id<\/text>\n   <text x='364.28' y='142.38' font-size='8.25pt' font-family='DejaVu Sans'>Proportion<\/text>\n   <image x='364.28' y='149.01' width='17.28' height='86.4' preserveAspectRatio='none' xlink:href='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAEsCAYAAAACUNnVAAAAHUlEQVQ4jWP4vT3xPxMDAwPDKDFKjBKjxCgxXAgA1SgFadr2HN0AAAAASUVORK5CYII=' xmlns:xlink='http://www.w3.org/1999/xlink'/>\n   <polyline points='378.11,192.21 381.56,192.21' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.37' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='367.74,192.21 364.28,192.21' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.37' stroke-linejoin='round' stroke-linecap='butt'/>\n   <text x='387.04' y='195.42' font-size='6.6pt' font-family='DejaVu Sans'>1<\/text>\n   <text x='57.03' y='15.1' font-size='9.9pt' font-family='DejaVu Sans'>Top 10 Concepts For Top 10 Variables<\/text>\n  <\/g>\n <\/g>\n<\/svg>","js":null,"uid":"svg_6a1fd3da403bf818","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px;text-align:left; ; position:absolute;pointer-events:none;z-index:999;}","placement":"doc","opacity":0.9,"offx":10,"offy":10,"use_cursor_pos":true,"use_fill":false,"use_stroke":false,"delay_over":200,"delay_out":500},"hover":{"css":".hover_data_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_data_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_data_SVGID_ { fill:orange;stroke:black; }\nline.hover_data_SVGID_, polyline.hover_data_SVGID_ { fill:none;stroke:orange; }\nrect.hover_data_SVGID_, polygon.hover_data_SVGID_, path.hover_data_SVGID_ { fill:orange;stroke:none; }\nimage.hover_data_SVGID_ { stroke:orange; }","reactive":true,"nearest_distance":null},"hover_inv":{"css":""},"hover_key":{"css":".hover_key_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_key_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_key_SVGID_ { fill:orange;stroke:black; }\nline.hover_key_SVGID_, polyline.hover_key_SVGID_ { fill:none;stroke:orange; }\nrect.hover_key_SVGID_, polygon.hover_key_SVGID_, path.hover_key_SVGID_ { fill:orange;stroke:none; }\nimage.hover_key_SVGID_ { stroke:orange; }","reactive":true},"hover_theme":{"css":".hover_theme_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_theme_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_theme_SVGID_ { fill:orange;stroke:black; }\nline.hover_theme_SVGID_, polyline.hover_theme_SVGID_ { fill:none;stroke:orange; }\nrect.hover_theme_SVGID_, polygon.hover_theme_SVGID_, path.hover_theme_SVGID_ { fill:orange;stroke:none; }\nimage.hover_theme_SVGID_ { stroke:orange; }","reactive":true},"select":{"css":".select_data_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_data_SVGID_ { stroke:none;fill:red; }\ncircle.select_data_SVGID_ { fill:red;stroke:black; }\nline.select_data_SVGID_, polyline.select_data_SVGID_ { fill:none;stroke:red; }\nrect.select_data_SVGID_, polygon.select_data_SVGID_, path.select_data_SVGID_ { fill:red;stroke:none; }\nimage.select_data_SVGID_ { stroke:red; }","type":"multiple","only_shiny":true,"selected":[]},"select_inv":{"css":""},"select_key":{"css":".select_key_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_key_SVGID_ { stroke:none;fill:red; }\ncircle.select_key_SVGID_ { fill:red;stroke:black; }\nline.select_key_SVGID_, polyline.select_key_SVGID_ { fill:none;stroke:red; }\nrect.select_key_SVGID_, polygon.select_key_SVGID_, path.select_key_SVGID_ { fill:red;stroke:none; }\nimage.select_key_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"select_theme":{"css":".select_theme_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_theme_SVGID_ { stroke:none;fill:red; }\ncircle.select_theme_SVGID_ { fill:red;stroke:black; }\nline.select_theme_SVGID_, polyline.select_theme_SVGID_ { fill:none;stroke:red; }\nrect.select_theme_SVGID_, polygon.select_theme_SVGID_, path.select_theme_SVGID_ { fill:red;stroke:none; }\nimage.select_theme_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"zoom":{"min":1,"max":1,"duration":300,"default_on":false},"toolbar":{"position":"topright","pngname":"diagram","tooltips":null,"fixed":false,"hidden":[],"delay_over":200,"delay_out":500},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}
```
