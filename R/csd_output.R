
#' CSD Output
#'
#' @param process_output the output from `csd_process`
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv
#'                        file that is output to the provided results folder after running the `csd_process` function
#' @param concept_set the same concept set used in the `csd_process` function; only required if `vocab_tbl` is not NULL
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param num_variables an integer to represent the top number of variables to include for the exploratory analyses;
#'                      will pick based on the most commonly appearing variables;
#' @param num_mappings an integer to represent the top number of mappings for a given variable in the exploratory analyses;
#'                     will pick based on the highest count of the most commonly appearing variables;
#' @param filtered_var for both `single- and multi- site anomaly tests without time measurements` and
#'                     `single- and multi- site exploratory tests with time measurements`, the variables
#'                     to focus on
#' @param filter_concept for @ss_anom_at, @ms_exp_at, and @ms_anom_at, the specific code that should
#'                       be the focus of the analysis
#' @param facet variables to facet by; defaults to NULL
#' @param text_wrapping_char an integer to limit the length of text on an axis before wrapping is enforced;
#'                           used in @ms_anom_nt
#' @param output_value the column in `process_output` that should be used in the visualization
#'                     relevant for @ss_exp_at, @ms_anom_nt, and @ms_exp_at
#'
#' @return a graph to visualize the results from `csd_process` based on the parameters provided; see documentation
#'         for individual subfunctions for details on specific output
#'
csd_output <- function(process_output,
                       output_function,
                       concept_set = conceptsetdistribution::csd_concept_set,
                       vocab_tbl = vocabulary_tbl('concept'),
                       num_variables = 10,
                       num_mappings = 10,
                       filtered_var = 'general_jia',
                       filter_concept = 81893,
                       facet=NULL,
                       text_wrapping_char = 80,
                       output_value = 'prop_concept'){

  ## check concept col
  concept_col <- ifelse('concept_id' %in% colnames(process_output), 'concept_id', 'concept_code')

  if('concept_id' %in% colnames(process_output)){
    process_output <- process_output %>% mutate(concept_id = as.integer(concept_id))
  }else{process_output <- process_output}

  ## Get concept names from vocabulary table
  if(output_function != 'csd_ss_anom_nt'){
    process_output <- join_to_vocabulary(tbl = process_output %>%
                                           inner_join(select(concept_set, concept_id, concept_code,
                                                             vocabulary_id)),
                                         vocab_tbl = vocab_tbl,
                                         col = concept_col,
                                         vocab_col = concept_col)
  }

  ## Run output functions
  if(output_function == 'csd_ss_exp_nt'){
    csd_output <- csd_ss_exp_nt(process_output=process_output,
                                concept_col = concept_col,
                                num_codes = num_variables,
                                num_mappings = num_mappings)
  }else if(output_function == 'csd_ss_anom_nt'){
    csd_output <- csd_ss_anom_nt(process_output,
                                 vocab_tbl = vocab_tbl,
                                 filtered_var = filtered_var)
  }else if(output_function == 'csd_ss_exp_at'){
    csd_output <- csd_ss_exp_at(process_output,
                                concept_col = concept_col,
                                facet=facet,
                                filtered_var = filtered_var,
                                num_mappings = num_mappings,
                                output_value=output_value)
  }else if(output_function == 'csd_ss_anom_at'){
    csd_output <- csd_ss_anom_at(process_output=process_output,
                                 concept_col = concept_col,
                                 filter_concept = filter_concept,
                                 filtered_var=filtered_var,
                                 facet=facet)
  }else if(output_function == 'csd_ms_exp_nt'){
    csd_output <- csd_ms_exp_nt(process_output=process_output,
                                concept_col = concept_col,
                                facet=facet,
                                num_codes = num_variables)
  }else if(output_function == 'csd_ms_anom_nt'){
    csd_output <- csd_ms_anom_nt(process_output=process_output,
                                 concept_col = concept_col,
                                 text_wrapping_char=text_wrapping_char,
                                 filtered_var=filtered_var,
                                 comparison_col=output_value)
  }else if(output_function == 'csd_ms_exp_at'){
    csd_output <- csd_ms_exp_at(process_output = process_output,
                                concept_col = concept_col,
                                filtered_var = filtered_var,
                                filtered_concept = filter_concept,
                                output_value = output_value,
                                facet = facet
    )
  }else if(output_function == 'csd_ms_anom_at'){
    csd_output <- csd_ms_anom_at(process_output=process_output,
                                 concept_col = concept_col,
                                 filter_concept=filter_concept)
  }else(cli::cli_abort('Please enter a valid output_function for this check'))

  return(csd_output)

}
