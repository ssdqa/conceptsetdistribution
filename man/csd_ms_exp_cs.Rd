% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csd_output_subfuncs.R
\name{csd_ms_exp_cs}
\alias{csd_ms_exp_cs}
\title{\emph{Multi Site, Exploratory, Cross-Sectional}}
\usage{
csd_ms_exp_cs(process_output, facet = NULL, concept_col, num_codes = 10)
}
\arguments{
\item{process_output}{dataframe output by \code{csd_process}}

\item{facet}{the variables by which you would like to facet the graph;
defaults to NULL}

\item{concept_col}{the name of the column from the concept_set used to identify concepts
should be either \code{concept_id} or \code{concept_code}}

\item{num_codes}{the number of top codes per variable that should be
displayed in the table}
}
\value{
a searchable and filterable table with mappings, proportion of representation, and
denominator counts for the number of codes selected
in \code{num_codes}
}
\description{
\emph{Multi Site, Exploratory, Cross-Sectional}
}
