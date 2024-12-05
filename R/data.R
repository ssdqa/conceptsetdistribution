
#' CSD Sample Domain File
#'
#' A sample version of the file structure expected for the `domain_tbl`
#' parameter in the `csd_process` function. The user should recreate
#' this file and include their own domain definitions.
#'
#' @format ## `csd_domain_file`
#' A data frame with 3 columns
#' \describe{
#'   \item{domain}{The name of the CDM table associated with the domain where the concept of interest can be found. Should match the domain listed in the concept_set file.}
#'   \item{concept_field}{The name of the column in the domain table that contains the concepts of interest listed in the concept_set file.}
#'   \item{date_field}{The name of the column in the domain table that contains dates to be used for time-based filtering.}
#'   \item{vocabulary_field}{(PCORnet only) The name of the column in the domain table where the vocabulary type is stored}
#' }
#'
"csd_domain_file"


#' CSD Sample Concept Set
#'
#' A sample version of the file structure expected for the `concept_set`
#' parameted in the `csd_process` function. The user should recreate this
#' file and include their own clinical concepts.
#'
#' @format ## `csd_concept_set`
#' A data frame with 6 columns
#' \describe{
#'   \item{concept_id}{The OMOP concept_id; if the PCORnet CDM is being used, default this column to a random integer like the row number}
#'   \item{concept_code}{The original code associated with the concept_id}
#'   \item{concept_name}{(optional)The string name of the concept}
#'   \item{vocabulary_id}{The vocabulary associated with the concept; if the PCORnet CDM is being used, ensure that the values of this field match the vocabulary abbreviations used in the CDM itself}
#'   \item{variable}{A string label for the variable associated with the concept}
#'   \item{domain}{The domain table where the concept should be identified. This should match the domain listed in the domain_tbl file}
#' }
#'
"csd_concept_set"
