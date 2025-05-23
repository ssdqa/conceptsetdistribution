% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csd_output_subfuncs.R
\name{csd_ms_exp_la}
\alias{csd_ms_exp_la}
\title{\emph{Multi Site, Exploratory, Longitudinal}}
\usage{
csd_ms_exp_la(
  process_output,
  concept_col,
  facet = NULL,
  filtered_var,
  filtered_concept,
  output_value = "prop_concept"
)
}
\arguments{
\item{process_output}{dataframe output by \code{csd_process}}

\item{concept_col}{the name of the column from the concept_set used to identify concepts
should be either \code{concept_id} or \code{concept_code}}

\item{facet}{the variables by which you would like to facet the graph;
defaults to NULL}

\item{filtered_var}{the variable(s) to perform the anomaly detection for}

\item{filtered_concept}{the concept_id(s) of interest for the analysis}

\item{output_value}{the numerical column in the data that should be displayed
in the output}
}
\value{
a line graph with one facet per code displaying the proportion of usage for
each site

a reference table with total counts of each code across the entire user selected
time period
}
\description{
\emph{Multi Site, Exploratory, Longitudinal}
}
