% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csd_output.R
\name{csd_output}
\alias{csd_output}
\title{CSD Output}
\usage{
csd_output(
  process_output,
  output_function,
  concept_set = conceptsetdistribution::csd_concept_set,
  vocab_tbl = vocabulary_tbl("concept"),
  num_variables = 10,
  num_mappings = 10,
  filtered_var = "general_jia",
  filter_concept = 81893,
  facet = NULL,
  text_wrapping_char = 80,
  output_value = "prop_concept"
)
}
\arguments{
\item{process_output}{the output from \code{csd_process}}

\item{output_function}{the name of the output function that should be used provided in the \code{parameter_summary} csv
file that is output to the provided results folder after running the \code{csd_process} function}

\item{concept_set}{the same concept set used in the \code{csd_process} function; only required if \code{vocab_tbl} is not NULL}

\item{vocab_tbl}{OPTIONAL: the location of an external vocabulary table containing concept names for
the provided codes. if not NULL, concept names will be available in either a reference
table or in a hover tooltip}

\item{num_variables}{an integer to represent the top number of variables to include for the exploratory analyses;
will pick based on the most commonly appearing variables;}

\item{num_mappings}{an integer to represent the top number of mappings for a given variable in the exploratory analyses;
will pick based on the highest count of the most commonly appearing variables;}

\item{filtered_var}{for both \verb{single- and multi- site anomaly tests without time measurements} and
\verb{single- and multi- site exploratory tests with time measurements}, the variables
to focus on}

\item{filter_concept}{for @ss_anom_at, @ms_exp_at, and @ms_anom_at, the specific code that should
be the focus of the analysis}

\item{facet}{variables to facet by; defaults to NULL}

\item{text_wrapping_char}{an integer to limit the length of text on an axis before wrapping is enforced;
used in @ms_anom_nt}

\item{output_value}{the column in \code{process_output} that should be used in the visualization
relevant for @ss_exp_at, @ms_anom_nt, and @ms_exp_at}
}
\value{
a graph to visualize the results from \code{csd_process} based on the parameters provided; see documentation
for individual subfunctions for details on specific output
}
\description{
CSD Output
}
