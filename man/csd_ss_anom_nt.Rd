% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csd_output_subfuncs.R
\name{csd_ss_anom_nt}
\alias{csd_ss_anom_nt}
\title{\emph{Single Site, Anomaly, No Time}}
\usage{
csd_ss_anom_nt(
  process_output,
  vocab_tbl = vocabulary_tbl("concept"),
  filtered_var = "general_jia"
)
}
\arguments{
\item{process_output}{the output from \code{csd_process}}

\item{vocab_tbl}{OPTIONAL: the location of an external vocabulary table containing concept names for
the provided codes. if not NULL, concept names will be available in either a reference
table or in a hover tooltip}

\item{filtered_var}{the variable to perform the jaccard similarity index for}
}
\value{
for a given variable, a heatmap of the jaccard index for each concept pair;
if the user hovers over the heatmap, the co-occurrence count, jaccard score for the pair,
mean jaccard score for the variable, and concepts will show.
}
\description{
\emph{Single Site, Anomaly, No Time}
}
