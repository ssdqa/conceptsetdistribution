# **Multi-Site, Anomaly, Longitudinal**

**Multi-Site, Anomaly, Longitudinal**

## Usage

``` r
csd_ms_anom_la(
  process_output,
  filter_concept,
  concept_col,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  output from `csd_process`

- filter_concept:

  the concept_id that should be used for the output

- concept_col:

  the name of the column from the concept_set used to identify concepts
  should be either `concept_id` or `concept_code`

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

three graphs:

1.  Loess smoothed line graph that shows the proportion of a code across
    time with the Euclidean Distance associated with each line

2.  same as (1) but displaying the raw, unsmoothed proportion

3.  a radial bar graph displaying the Euclidean Distance value for each
    site, where the color is the average proportion across time

THIS GRAPH SHOWS ONLY ONE CONCEPT AT A TIME!
