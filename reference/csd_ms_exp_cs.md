# *Multi Site, Exploratory, Cross-Sectional*

*Multi Site, Exploratory, Cross-Sectional*

## Usage

``` r
csd_ms_exp_cs(
  process_output,
  facet = NULL,
  concept_col,
  num_codes = 10,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  dataframe output by `csd_process`

- facet:

  the variables by which you would like to facet the graph; defaults to
  NULL

- concept_col:

  the name of the column from the concept_set used to identify concepts
  should be either `concept_id` or `concept_code`

- num_codes:

  the number of top codes per variable that should be displayed in the
  table

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a searchable and filterable table with mappings, proportion of
representation, and denominator counts for the number of codes selected
in `num_codes`
