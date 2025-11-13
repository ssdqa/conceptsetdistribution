# *Multi-Site, Anomaly, Cross-Sectional*

*Multi-Site, Anomaly, Cross-Sectional*

## Usage

``` r
csd_ms_anom_cs(
  process_output,
  concept_col,
  text_wrapping_char = 80,
  filtered_var,
  comparison_col = "prop_concept",
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  output from `csd_process`

- concept_col:

  the name of the column from the concept_set used to identify concepts
  should be either `concept_id` or `concept_code`

- text_wrapping_char:

  the number of characters for the `concept_name` or `concept_id` to
  display on heatmap; limited to 80

- filtered_var:

  the variable to perform the analysis on from the data frame; column
  name that contains the variable names should be labeled `variable`

- comparison_col:

  the column that computes the quantitative value for comparison across
  sites; in `csd` check, it is the `prop_concept`

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of usage for a
given code, and the size of the dot represents the mean code usage
across all sites
