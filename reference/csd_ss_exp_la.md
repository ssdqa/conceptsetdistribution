# *Single Site, Exploratory, Longitudinal*

*Single Site, Exploratory, Longitudinal*

## Usage

``` r
csd_ss_exp_la(
  process_output,
  concept_col,
  facet = NULL,
  filtered_var,
  num_mappings = 10,
  output_value = "prop_concept"
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

  the variable to perform the anomaly detection for

- num_mappings:

  an integer indicating the number of top codes for the filtered_var of
  interest that should be displayed

- output_value:

  the numerical column in the data that should be displayed

## Value

a line graph with one facet per variable displaying the proportion of
mapped codes across the user selected time period

a reference table with total counts of each code across the entire user
selected time period
