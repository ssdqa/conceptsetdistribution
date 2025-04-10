% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csd_output.R
\name{csd_output}
\alias{csd_output}
\title{Concept Set Distribution -- Output Generation}
\usage{
csd_output(
  process_output,
  output_function,
  concept_set = NULL,
  vocab_tbl = NULL,
  num_variables = 10,
  num_mappings = 10,
  filter_variable = NULL,
  filter_concept = NULL,
  text_wrapping_char = 80,
  output_value = "prop_concept"
)
}
\arguments{
\item{process_output}{the output from \code{csd_process}}

\item{output_function}{the name of the output function that should be executed, provided in the message output
to the console after \code{csd_process} has been executed}

\item{concept_set}{the same concept set used in the \code{csd_process} function; only required if \code{vocab_tbl} is not NULL}

\item{vocab_tbl}{OPTIONAL: the location of an external vocabulary table containing concept names for
the provided codes. if not NULL, concept names will be available in either a reference
table or in a hover tooltip}

\item{num_variables}{an integer to represent the top number of variables to include for the exploratory analyses;
will pick based on the most commonly appearing variables;}

\item{num_mappings}{an integer to represent the top number of mappings for a given variable in the exploratory analyses;
will pick based on the highest count of the most commonly appearing variables;}

\item{filter_variable}{for both \verb{single- and multi- site anomaly tests without time measurements} and
\verb{single- and multi- site exploratory tests with time measurements}, the variables
to focus on}

\item{filter_concept}{for \code{ss_anom_la}, \code{ms_exp_la}, and \code{ms_anom_la}, the specific code that should
be the focus of the analysis}

\item{text_wrapping_char}{an integer to limit the length of text on an axis before wrapping is enforced;
used in \code{ms_anom_cs}}

\item{output_value}{the numerical column in \code{process_output} that should be used in the visualization
relevant for \code{ss_exp_la}, \code{ms_anom_cs}, and \code{ms_exp_la}}
}
\value{
a graph to visualize the results from \code{csd_process} based on the parameters provided; see documentation
for individual subfunctions for details on specific output
}
\description{
Using the tabular output generated by \code{csd_process}, this function will build a graph to
visualize the results. Each function configuration will output a bespoke ggplot. Theming can
be adjusted by the user after the graph has been output using \code{+ theme()}. Most graphs can
also be made interactive using \code{make_interactive_squba()}
}
\examples{

#' Source setup file
source(system.file('setup.R', package = 'conceptsetdistribution'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'csd_process_test',
                      working_directory = getwd(),
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = system.file('extdata',
                                        package = 'conceptsetdistribution'),
                      cdm_schema = NA)

#' Build mock study cohort
cohort <- cdm_tbl('person') \%>\% dplyr::distinct(person_id) \%>\%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id \%in\% c(1:6), 'synth1', 'synth2'))

#' Prepare input tables
csd_domain_tbl <- dplyr::tibble(domain = 'condition_occurrence',
                                concept_field = 'condition_concept_id',
                                date_field = 'condition_start_date',
                                vocabulary_field = NA)

csd_concept_tbl <- read_codeset('dx_hypertension') \%>\%
  dplyr::mutate(domain = 'condition_occurrence',
                variable = 'hypertension')

#' Execute `csd_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
csd_process_example <- csd_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   domain_tbl = csd_domain_tbl,
                                   concept_set = csd_concept_tbl)

csd_process_example

#' Execute `csd_output` function
csd_output_example <- csd_output(process_output = csd_process_example,
                                 output_function = 'csd_ss_exp_cs',
                                 concept_set = csd_concept_tbl,
                                 vocab_tbl = NULL)

csd_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(csd_output_example)
}
