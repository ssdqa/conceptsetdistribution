# *Single Site, Anomaly, Cross-Sectional*

*Single Site, Anomaly, Cross-Sectional*

## Usage

``` r
csd_ss_anom_cs(process_output, vocab_tbl = NULL, filtered_var)
```

## Arguments

- process_output:

  the output from `csd_process`

- vocab_tbl:

  OPTIONAL: the location of an external vocabulary table containing
  concept names for the provided codes. if not NULL, concept names will
  be available in either a reference table or in a hover tooltip

- filtered_var:

  the variable to perform the jaccard similarity index for

## Value

for a given variable, a heatmap of the jaccard index for each concept
pair
