# *Single Site, Anomaly, Longitudinal*

*Single Site, Anomaly, Longitudinal*

## Usage

``` r
csd_ss_anom_la(
  process_output,
  concept_col,
  filtered_var,
  filter_concept,
  facet = NULL
)
```

## Arguments

- process_output:

  dataframe output by `csd_process`

- concept_col:

  the name of the column from the concept_set used to identify concepts
  should be either `concept_id` or `concept_code`

- filtered_var:

  the variable to perform the anomaly detection for

- filter_concept:

  the concept_id of interest for the analysis

- facet:

  the variables by which you would like to facet the graph; defaults to
  NULL

## Value

if analysis was executed by year or greater, a P Prime control chart is
returned with outliers marked with orange dots

        if analysis was executed by month or smaller, an STL regression is
        conducted and outliers are marked with red dots. the graphs representing
        the data removed in the regression are also returned
