# *Single Site, Exploratory, Cross-Sectional*

*Single Site, Exploratory, Cross-Sectional*

## Usage

``` r
csd_ss_exp_cs(
  process_output,
  concept_col,
  facet = NULL,
  num_codes = 10,
  num_mappings = 10
)
```

## Arguments

- process_output:

  the output from `csd_process`

- concept_col:

  the name of the column from the concept_set used to identify concepts
  should be either `concept_id` or `concept_code`

- facet:

  variables to facet by; defaults to NULL

- num_codes:

  an integer to represent the top number of codes to include in the
  mappings for the exploratory analyses; will pick the codes based on
  the highest count of the most commonly appearing variables;

- num_mappings:

  an integer to represent the top number of mappings for a given
  variable in the exploratory analyses

## Value

a list with two elements: 1) heatmap for to `n` concepts (`num_codes`)
and `x` variables (`num_mappings`), with proportion for each concept. 2)
a table with each mapping and the total variable count
