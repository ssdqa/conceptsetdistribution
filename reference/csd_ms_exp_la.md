# *Multi Site, Exploratory, Longitudinal*

*Multi Site, Exploratory, Longitudinal*

## Usage

``` r
csd_ms_exp_la(
  process_output,
  concept_col,
  facet = NULL,
  filtered_var,
  filtered_concept,
  output_value = "prop_concept",
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  dataframe output by `csd_process`

- concept_col:

  the name of the column from the concept_set used to identify concepts
  should be either `concept_id` or `concept_code`

- facet:

  the variables by which you would like to facet the graph; defaults to
  NULL

- filtered_var:

  the variable(s) to perform the anomaly detection for

- filtered_concept:

  the concept_id(s) of interest for the analysis

- output_value:

  the numerical column in the data that should be displayed in the
  output

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a line graph with one facet per code displaying the proportion of usage
for each site

a reference table with total counts of each code across the entire user
selected time period
