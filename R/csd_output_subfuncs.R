
#'
#' @import ggplot2
#' @import gt
#' @import cli
#' @import ggiraph
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unite
#' @importFrom tidyr tibble
#' @importFrom qicharts2 qic
#' @importFrom timetk plot_anomalies
#' @importFrom timetk plot_anomalies_decomp
#' @importFrom graphics text
#' @importFrom stats sd
#' @importFrom utils head
#' @importFrom purrr set_names
#' @importFrom plotly layout
#' @importFrom patchwork plot_layout
#'
NULL


#' *Single Site, Exploratory, Cross-Sectional*
#'
#'
#' @param process_output the output from `csd_process`
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param num_codes an integer to represent the top number of codes to include in the mappings for the exploratory analyses;
#'                  will pick the codes based on the highest count of the most commonly appearing variables;
#' @param num_mappings an integer to represent the top number of mappings for a given variable in the exploratory analyses
#' @param facet variables to facet by; defaults to NULL
#'
#' @return a list with two elements:
#'        1) heatmap for to `n` concepts (`num_codes`)  and `x` variables (`num_mappings`),
#'        with proportion for each concept.
#'        2) a table with each mapping and the total variable count
#'
csd_ss_exp_cs <- function(process_output,
                          concept_col,
                          facet = NULL,
                          num_codes = 10,
                          num_mappings = 10){

  # picking columns / titles
    denom <-  'ct_denom'
    col <- 'variable'
    map_col <- concept_col
    prop <- 'prop_concept'
    title <- paste0('Top ', num_mappings, ' Concepts For Top ', num_codes, ' Variables')


  if(num_codes > 12){cli::cli_abort('Please only select up to 12 variables to maintain readability in the output.')}

  ## filter output down to most common codes, selecting a user-provided number
  topcodes <- process_output %>%
    ungroup() %>%
    group_by(!! sym(col)) %>%
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    summarise(total_sum = sum(!! sym(denom))) %>%
    arrange(desc(total_sum)) %>%
    slice(1:num_codes)

  ref <- process_output %>%
    ungroup() %>%
    inner_join(topcodes)

  nmap_total <- ref %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    summarise(nmap = n())

  nmap_top <- ref %>%
    select(col, map_col, all_of(facet), prop) %>%
    distinct() %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(!!sym(prop))) %>%
    slice(1:num_mappings)

  final <-
    ref %>%
    inner_join(nmap_top) %>%
    left_join(nmap_total) %>%
    mutate(xaxis = paste0(!!sym(col), '\n Total Mappings: ', nmap))

  facet <- facet %>% append('xaxis')

    plt <- final %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)),
                                 fill = !!sym(prop))) +
      geom_tile_interactive(aes(tooltip = concept_name)) +
      geom_text(aes(label = !!sym(prop)), size = 3, color = 'black') +
      scale_fill_squba(palette = 'diverging', discrete = FALSE) +
      facet_wrap((facet), scales = 'free') +
      theme_minimal() +
      #theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = '',
           y = map_col,
           fill = 'Proportion')

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                 'tooltip' = TRUE)

  ref_tbl <- generate_ref_table(tbl = final, #%>% mutate(concept_id = as.integer(concept_id)) %>%
                                  #select(-concept_name),
                                id_col = col,
                                name_col = col,
                                denom = denom)

  output <- list(plt, ref_tbl)

  return(output)
}


#' *Single Site, Anomaly, Cross-Sectional*
#'
#'
#' @param process_output the output from `csd_process`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param filtered_var the variable to perform the jaccard similarity index for
#'
#' @return for a given variable, a heatmap of the jaccard index for each concept pair
#'
csd_ss_anom_cs <- function(process_output,
                           #concept_col,
                           vocab_tbl = NULL,
                           filtered_var){

  ## Check for limit
  var_ct <- process_output %>%
    filter(variable == filtered_var, above_sd == TRUE) %>%
    select(concept1, concept2) %>%
    pivot_longer(cols = c('concept1', 'concept2')) %>%
    distinct(value) %>% summarise(n()) %>% pull()

  if(var_ct > 20){cli::cli_alert_warning('Output has been limited to top 20 concepts to improve visibility on axes.')}
  if(var_ct == 0){cli::cli_abort(paste0('No concept pairs were returned for the variable ', filtered_var, '. Please choose another.'))}

  vars <- process_output %>%
    filter(variable == filtered_var, above_sd == TRUE) %>%
    select(concept1, concept2, concept1_ct, concept2_ct) %>%
    pivot_longer(cols = c('concept1', 'concept2')) %>%
    rename(concept1 = concept1_ct, concept2 = concept2_ct) %>%
    pivot_longer(cols = c(concept1, concept2),
                 names_to = 'name2', values_to = 'value2') %>%
    filter(name == name2) %>%
    distinct(value, value2) %>%
    arrange(desc(value2)) %>% slice(1:20) %>% pull(value)

  ## Join to vocab
  concept_col <- ifelse(class(process_output$concept1) %in% 'character',
                        'concept_code', 'concept_id')

  firstcolnames <- join_to_vocabulary(tbl = process_output,
                                      vocab_tbl = vocab_tbl,
                                      col = 'concept1',
                                      vocab_col = concept_col) %>%
    rename(conceptname1=concept_name) %>% select(concept1, conceptname1)

  secondcolnames <- join_to_vocabulary(tbl = process_output,
                                       vocab_tbl = vocab_tbl,
                                       col = 'concept2',
                                       vocab_col = concept_col) %>%
    rename(conceptname2=concept_name) %>% select(concept2, conceptname2)

  final <-
    process_output %>%
    left_join(firstcolnames) %>%
    left_join(secondcolnames) %>% distinct() %>%
    filter(concept1 %in% vars & concept2 %in% vars)

  ## Output graph
    plt <- final %>% filter(variable==filtered_var) %>% filter(above_sd == TRUE) %>%
      ggplot(aes(x = as.character(concept1), y = as.character(concept2),
             fill = jaccard_index)) +
      geom_tile_interactive(aes(tooltip = paste0('concept1 = ',conceptname1, '; n= ',concept1_ct,'\n','concept2 = ',conceptname2,'; n= ',concept2_ct,
                                                 '\n', 'co-occurrence = ', cocount,
                                                 '\n','jaccard sim = ',jaccard_index,
                                                 '\n', 'mean = ',var_jaccard_mean,'\n','sd = ', var_jaccard_sd))) +
      scale_fill_squba(palette = 'diverging', discrete = FALSE) +
       labs(title = paste0('Jaccard Similarity Index for ', filtered_var, ' Concepts'),
            x = 'concept1',
            y = 'concept2',
            fill = 'Jaccard Index') +
      theme_minimal() #+
      #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)

  return(plt)
}

### ACROSS TIME

#' *Single Site, Anomaly, Longitudinal*
#'
#' @param process_output dataframe output by `csd_process`
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param filtered_var the variable to perform the anomaly detection for
#' @param filter_concept the concept_id of interest for the analysis
#' @param facet the variables by which you would like to facet the graph; defaults to NULL
#'
#' @return if analysis was executed by year or greater, a P Prime control chart
#'         is returned with outliers marked with orange dots
#'
#'         if analysis was executed by month or smaller, an STL regression is
#'         conducted and outliers are marked with red dots. the graphs representing
#'         the data removed in the regression are also returned
#'
csd_ss_anom_la <- function(process_output,
                           concept_col,
                           filtered_var,
                           filter_concept,
                           facet=NULL){

  time_inc <- process_output %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()
  concept_col <- concept_col

  if(time_inc == 'year'){

  facet <- facet %>% append(concept_col) %>% unique()

  c_added <- process_output %>% filter(variable == filtered_var,
                                       !!sym(concept_col) == filter_concept)


  c_final <- c_added %>% group_by(!!!syms(facet), time_start, ct_concept) %>%
    unite(facet_col, !!!syms(facet), sep = '\n')

  c_plot <- qic(data = c_final, x = time_start, y = ct_concept, chart = 'pp', facets = ~facet_col,
                title = 'Control Chart: Code Usage Over Time', show.grid = TRUE, n = ct_denom,
                ylab = 'Proportion', xlab = 'Time')

  op_dat <- c_plot$data

  new_pp <- ggplot(op_dat,aes(x,y)) +
    geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
    geom_line(colour = squba_colors_standard[[12]], size = .5) +
    geom_line(aes(x,cl)) +
    geom_point(colour = squba_colors_standard[[6]] , fill = squba_colors_standard[[6]], size = 1) +
    geom_point(data = subset(op_dat, y >= ucl), color = squba_colors_standard[[3]], size = 2) +
    geom_point(data = subset(op_dat, y <= lcl), color = squba_colors_standard[[3]], size = 2) +
    facet_wrap(~facet1) +
    ggtitle(label = 'Control Chart: Code Usage Over Time') +
    labs(x = 'Time',
         y = 'Proportion')+
    theme_minimal()

  new_pp[['metadata']] <- tibble('pkg_backend' = 'plotly',
                                 'tooltip' = FALSE)

  ref_tbl <- generate_ref_table(tbl = c_added %>% filter(variable == filtered_var,
                                                         !!sym(concept_col) == filter_concept), #%>%
                                  #mutate(concept_id=as.integer(concept_id)),
                                id_col = concept_col,
                                denom = 'ct_concept',
                                name_col = 'concept_name',
                                #vocab_tbl = vocab_tbl,
                                time = TRUE)

  output <- list(new_pp, ref_tbl)

  }else{

    concept_nm <- process_output %>%
      filter(!is.na(concept_name), !!sym(concept_col) == filter_concept) %>%
      distinct(concept_name) %>% pull()

    anomalies <-
      plot_anomalies(.data=process_output %>% filter(!!sym(concept_col) == filter_concept),
                     .date_var=time_start,
                     .interactive = FALSE,
                     .title = paste0('Anomalies for Code ', filter_concept, ': ', concept_nm)) #%>%
      #layout(title = paste0('Anomalies for Code ', filter_concept, ': ', concept_nm))

    decomp <-
      plot_anomalies_decomp(.data=process_output %>% filter(!!sym(concept_col) == filter_concept),
                            .date_var=time_start,
                            .interactive = FALSE,
                            .title = paste0('Anomalies for Code ', filter_concept, ': ', concept_nm)) #%>%
      #layout(title = paste0('Anomalies for Code ', filter_concept, ': ', concept_nm))

    anomalies[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                      'tooltip' = FALSE)
    decomp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output <- list(anomalies, decomp)

  }

  return(output)

}


#' *Single Site, Exploratory, Longitudinal*
#'
#' @param process_output dataframe output by `csd_process`
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param filtered_var the variable to perform the anomaly detection for
#' @param num_mappings an integer indicating the number of top codes for the
#'                     filtered_var of interest that should be displayed
#' @param facet the variables by which you would like to facet the graph;
#'              defaults to NULL
#' @param output_value the numerical column in the data that should be displayed
#'
#' @return a line graph with one facet per variable displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#'
csd_ss_exp_la <- function(process_output,
                          concept_col,
                          facet=NULL,
                          filtered_var,
                          num_mappings = 10,
                          output_value='prop_concept'){

  denom <- 'ct_concept'
  col <- concept_col

  output_value <- output_value

  site_num <-
    process_output %>% ungroup() %>%select(site) %>% distinct() %>% pull()

  if(length(site_num)>1){
    facet <- facet %>% append('site')
  } else {
    facet <- facet %>% append('variable')
  }

  topcodes <- process_output %>%
    filter(variable == filtered_var) %>%
    ungroup() %>%
    group_by(!!sym(col)) %>%
    select(!!sym(col), !!sym(denom), all_of(facet)) %>%
    distinct() %>%
    summarise(total_sum = sum(!! sym(denom))) %>%
    arrange(desc(total_sum)) %>%
    slice(1:num_mappings)

  ref <- process_output %>%
    filter(variable == filtered_var) %>%
    ungroup() %>%
    inner_join(topcodes)

  dat_to_plot <- ref %>%
    mutate(text=paste("Concept: ",!!sym(col),
                      "\nConcept Name: ",concept_name,
                      "\nSite: ",site,
                      "\nValue: ",!!sym(output_value),
                      "\nTime Point: ", time_start))


  ref_tbl <- generate_ref_table(tbl = dat_to_plot %>%
                                  #mutate(concept_id=as.integer(concept_id)) %>%
                                  group_by(site),
                                id_col = col,
                                denom = 'ct_concept',
                                name_col = 'concept_name',
                                time = TRUE)

  p <-dat_to_plot %>% filter(variable == filtered_var)  %>%
    mutate(concept=as.character(!!sym(col))) %>%
    ggplot(aes(y = !!sym(output_value), x = time_start, color = concept,
               group=concept, text=text)) +
    geom_line() +
    facet_wrap((facet)) +
    labs(title = paste0('Top ', num_mappings, ' Concepts for ', filtered_var, ' Over Time'),
         color = col,
         y = 'Proportion',
         x = 'Time') +
    theme_minimal() +
    scale_color_squba()

  p[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)


  output <- list(p, ref_tbl)

  return(output)

}

#' *Multi Site, Exploratory, Longitudinal*
#'
#' @param process_output dataframe output by `csd_process`
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param filtered_var the variable(s) to perform the anomaly detection for
#' @param filtered_concept the concept_id(s) of interest for the analysis
#' @param output_value the numerical column in the data that should be displayed
#'                     in the output
#' @param facet the variables by which you would like to facet the graph;
#'              defaults to NULL
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return a line graph with one facet per code displaying the proportion of usage for
#'         each site
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#'
csd_ms_exp_la <- function(process_output,
                          concept_col,
                          facet=NULL,
                          filtered_var,
                          filtered_concept,
                          output_value='prop_concept',
                          large_n = FALSE,
                          large_n_sites = NULL){

  output_value <- output_value
  concept_col <- concept_col

  site_num <-
    process_output %>% ungroup() %>%select(site) %>% distinct() %>% pull()

  if(length(site_num)>1){
    facet <- facet %>% append('concept_id_label')
  } else {
    facet <- facet %>% append('concept_id_label')
  }


  dat_to_plot <- process_output %>% filter(variable %in% filtered_var,
                                           !!sym(concept_col) %in% filtered_concept) %>%
    mutate(concept_id_label = paste0(variable, '\n', !!sym(concept_col))) %>%
    mutate(text=paste("Concept: ",!!sym(concept_col),
                      "\nConcept Name: ",concept_name,
                      "\nSite: ",site,
                      "\nValue: ",!!sym(output_value),
                      "\nTime Point: ", time_start))

  if(!large_n){

    ref_tbl <- generate_ref_table(tbl = dat_to_plot %>%
                                    #mutate(concept_id=as.integer(concept_id)) %>%
                                    group_by(site),
                                  id_col = concept_col,
                                  denom = 'ct_concept',
                                  name_col = 'concept_name',
                                  time = TRUE)

    p <-dat_to_plot %>%
      mutate(concept_id=as.character(!!sym(concept_col))) %>%
      ggplot(aes(y = !!sym(output_value), x = time_start, color = site,
                 group=site, text=text)) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = paste0('Concepts per Site Over Time'),
           color = 'Site',
           y = 'Proportion',
           x = 'Time') +
      theme_minimal() +
      scale_color_squba()

    p[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)
  }else{

    summ_stats <- dat_to_plot %>%
      group_by(concept_id_label, variable, !!sym(concept_col),
               concept_name, time_start) %>%
      summarise(allsite_median = stats::median(!!sym(output_value)),
                allsite_q1 = stats::quantile(!!sym(output_value), 0.25),
                allsite_q3 = stats::quantile(!!sym(output_value), 0.75)) %>%
      pivot_longer(cols = !c(concept_id_label, variable, !!sym(concept_col),
                             concept_name, time_start),
                   names_to = 'site',
                   values_to = output_value) %>%
      mutate(site = case_when(site == 'allsite_median' ~ 'All Site Median',
                              site == 'allsite_q1' ~ 'All Site Q1',
                              site == 'allsite_q3' ~ 'All Site Q3'),
             text = paste("Concept: ",!!sym(concept_col),
                          "\nConcept Name: ",concept_name,
                          "\nSite: ",site,
                          "\nValue: ",!!sym(output_value),
                          "\nTime Point: ", time_start))

    if(!is.null(large_n_sites)){
      a <- 0.5
      lt <- 'dashed'
    }else{
      a <- 1
      lt <- 'solid'}

    p <- summ_stats %>%
      mutate(concept_id=as.character(!!sym(concept_col))) %>%
      ggplot(aes(y = !!sym(output_value), x = time_start, color = site,
                 group=site, text=text)) +
      geom_line(linewidth = 1, alpha = a, linetype = lt) +
      geom_line(data = dat_to_plot %>% filter(site %in% large_n_sites),
                linewidth = 1) +
      facet_wrap((facet)) +
      labs(title = paste0('Concepts per Site Over Time'),
           color = 'Site',
           y = 'Proportion',
           x = 'Time') +
      theme_minimal() +
      scale_color_squba()

    if(!is.null(large_n_sites)){
      ref_tbl <- generate_ref_table(tbl = dat_to_plot %>%
                                      filter(site %in% large_n_sites) %>%
                                      group_by(site),
                                    id_col = concept_col,
                                    denom = 'ct_concept',
                                    name_col = 'concept_name',
                                    time = TRUE)
    }else{
      ref_tbl <- generate_ref_table(tbl = dat_to_plot %>%
                                      mutate(site = 'all sites') %>%
                                      group_by(site),
                                    id_col = concept_col,
                                    denom = 'ct_concept',
                                    name_col = 'concept_name',
                                    time = TRUE)
    }

  }

  output <- list(p, ref_tbl)

  return(output)

}


#' *Multi Site, Exploratory, Cross-Sectional*
#'
#' @param process_output dataframe output by `csd_process`
#' @param facet the variables by which you would like to facet the graph;
#'              defaults to NULL
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param num_codes the number of top codes per variable that should be
#'                  displayed in the table
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#'
#' @return a searchable and filterable table with mappings, proportion of representation, and
#'         denominator counts for the number of codes selected
#'         in `num_codes`
#'
csd_ms_exp_cs <- function(process_output,
                          facet = NULL,
                          concept_col,
                          num_codes = 10,
                          large_n = FALSE,
                          large_n_sites = NULL){

  # picking columns / titles

  denom <-  'ct_denom'
  col <- 'variable'
  map_col <- concept_col
  prop <- 'prop_concept'
  ct = 'ct_concept'

  ## Enfore site facetting
  facet <- facet %>% append('site') %>% unique()

  ## filter output down to most common codes, selecting a user-provided number

  if(large_n & is.null(large_n_sites)){
    process_output <- process_output %>%
      mutate(site = 'all sites') %>%
      group_by(site, variable) %>%
      mutate(ct_denom = sum(ct_denom)) %>%
      group_by(site, variable, !!sym(concept_col), ct_denom, concept_name) %>%
      summarise(ct_concept = sum(ct_concept)) %>%
      mutate(prop_concept = ct_concept / ct_denom)
  }else if(large_n & !is.null(large_n_sites)){
    process_output <- process_output %>%
      filter(site %in% large_n_sites)
  }

  topcodes <- process_output %>%
    ungroup() %>%
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    group_by(!!! syms(facet)) %>%
    arrange(desc(!! sym(denom))) %>%
    slice(1:num_codes)

  final_filt <- process_output %>%
    inner_join(topcodes)

  table <-
    final_filt %>%
    ungroup() %>%
    select(-denom) %>%
    mutate(pct = !!sym(prop)) %>%
    arrange(!!!syms(facet), desc(ct)) %>%
    #relocate(site) %>%
    select(!!!syms(facet), variable, !!sym(map_col), concept_name, !!sym(ct), !!sym(prop), pct) %>%
    gt::gt() %>%
    cols_nanoplot(columns = pct, plot_type = 'bar',
                  autohide = TRUE, new_col_label = 'percent') %>%
    #gtExtras::gt_plt_bar_pct(column = pct) %>%
    fmt_number(columns = ct, decimals = 0) %>%
    fmt_percent(columns = prop, decimals = 0) %>%
    data_color(palette = squba_colors_standard, columns = c(all_of(facet))) %>%
    tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Variables')) %>%
    opt_interactive(use_search = TRUE,
                    use_filters = TRUE)


  return(table)

}


#' *Multi-Site, Anomaly, Cross-Sectional*
#'
#' @param process_output output from `csd_process`
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param text_wrapping_char the number of characters for the `concept_name` or `concept_id` to
#'                           display on heatmap; limited to 80
#' @param filtered_var the variable to perform the analysis on from the data frame; column name
#'                     that contains the variable names should be labeled `variable`
#' @param comparison_col the column that computes the quantitative value for comparison across sites;
#'                       in `csd` check, it is the `prop_concept`
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of usage for
#'         a given code, and the size of the dot represents the mean code
#'         usage across all sites
#'

csd_ms_anom_cs<-function(process_output,
                         concept_col,
                         text_wrapping_char=80,
                         filtered_var,
                         comparison_col='prop_concept',
                         large_n = FALSE,
                         large_n_sites = NULL){

  cname_samp <- process_output %>% head(1) %>% select(concept_name) %>% pull()
  concept_col <- concept_col

  if(cname_samp == 'No vocabulary table input'){
    concept_label <- concept_col
  }else{concept_label <- 'concept_name'}

  comparison_col = comparison_col

  check_n <- process_output %>%
    filter(anomaly_yn != 'no outlier in group')

  dat_to_plot <- process_output %>% filter(variable == filtered_var) %>%
    mutate(text=paste("Concept: ",!!sym(concept_label),
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))
  if(!large_n){
    if(nrow(check_n) > 0){

      dat_to_plot <- dat_to_plot %>% filter(variable == filtered_var) %>%
        mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))

      plt <-
        ggplot(dat_to_plot, aes(x=site, y=as.character(!!sym(concept_col)), text=text, color=!!sym(comparison_col)))+
        geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
        geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                               aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
        scale_color_squba(palette = 'diverging', discrete = FALSE) +
        scale_shape_manual(values=c(19,8))+
        scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
        theme_minimal() +
        #theme(axis.text.x = element_text(angle=60, hjust = 1, vjust = 1)) +
        labs(y = "Concept",
             size="",
             title=paste0('Anomalous Concepts for ', filtered_var, ' per Site'),
             subtitle = 'Dot size is the mean proportion per concept') +
        guides(color = guide_colorbar(title = 'Proportion'),
               shape = guide_legend(title = 'Anomaly'),
               size = 'none')

      plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                  'tooltip' = TRUE)

      return(plt)

    }else{

      plt <- ggplot(dat_to_plot, aes(x = site, y = as.character(!!sym(concept_col)),
                                     fill = !!sym(comparison_col),
                                     tooltip = text)) +
        geom_tile_interactive() +
        theme_minimal() +
        scale_fill_squba(discrete = FALSE, palette = 'diverging') +
        labs(y = 'Concept',
             x = 'Site',
             fill = 'Proportion')

      # Test Site Score using SD Computation
      test_site_score <- process_output %>%
        mutate(dist_mean = (!!sym(comparison_col) - mean_val)^2) %>%
        group_by(site) %>%
        summarise(n_grp = n(),
                  dist_mean_sum = sum(dist_mean),
                  overall_sd = sqrt(dist_mean_sum / n_grp)) %>%
        mutate(tooltip = paste0('Site: ', site,
                                '\nStandard Deviation: ', round(overall_sd, 3)))

      ylim_max <- test_site_score %>% filter(overall_sd == max(overall_sd)) %>% pull(overall_sd) + 1
      ylim_min <- test_site_score %>% filter(overall_sd == min(overall_sd)) %>% pull(overall_sd) - 1

      g2 <- ggplot(test_site_score, aes(y = overall_sd, x = site, color = site,
                                        tooltip = tooltip)) +
        geom_point_interactive(show.legend = FALSE) +
        theme_minimal() +
        scale_color_squba() +
        geom_hline(yintercept = 0, linetype = 'solid') +
        labs(title = 'Average Standard Deviation per Site',
             y = 'Average Standard Deviation',
             x = 'Site')

      plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                  'tooltip' = TRUE)
      g2[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                 'tooltip' = TRUE)

      opt <- list(plt,
                  g2)

      return(opt)
    }
  }else{
    suppressWarnings(
      far_site <- process_output %>%
        filter(variable == filtered_var) %>%
        mutate(zscr = (!!sym(comparison_col) - mean_val) / sd_val,
               zscr = ifelse(is.nan(zscr), NA, zscr),
               zscr = abs(zscr)) %>%
        group_by(variable, !!sym(concept_col)) %>%
        filter(zscr == max(zscr, na.rm = TRUE)) %>%
        summarise(farthest_site = site,
                  nvar = n())

    )

    if(any(far_site$nvar > 1)){
      far_site <- far_site %>%
        summarise_all(toString) %>% select(-nvar)
    }else{
      far_site <- far_site %>% select(-nvar)
    }

    suppressWarnings(
      close_site <- process_output %>%
        filter(variable == filtered_var) %>%
        mutate(zscr = (!!sym(comparison_col) - mean_val) / sd_val,
               zscr = ifelse(is.nan(zscr), NA, zscr),
               zscr = abs(zscr)) %>%
        group_by(variable, !!sym(concept_col)) %>%
        filter(zscr == min(zscr, na.rm = TRUE)) %>%
        summarise(closest_site = site,
                  nvar = n())
    )

    if(any(close_site$nvar > 1)){
      close_site <- close_site %>%
        summarise_all(toString) %>% select(-nvar)
    }else{
      close_site <- close_site %>% select(-nvar)
    }

    nsite_anom <- process_output %>%
      filter(variable == filtered_var) %>%
      group_by(variable, !!sym(concept_col), anomaly_yn) %>%
      summarise(site_w_anom = n_distinct(site)) %>%
      filter(anomaly_yn == 'outlier') %>%
      ungroup() %>%
      select(-anomaly_yn)

    sitesanoms <- process_output %>%
      filter(variable == filtered_var) %>%
      filter(anomaly_yn == 'outlier') %>%
      group_by(variable, !!sym(concept_col)) %>%
      summarise(site_anoms = toString(site)) %>%
      select(variable, !!sym(concept_col), site_anoms)

    tbl <- process_output %>%
      filter(variable == filtered_var) %>%
      group_by(variable, !!sym(concept_col)) %>%
      mutate(iqr_val = stats::IQR(!!sym(comparison_col))) %>%
      ungroup() %>%
      distinct(variable, !!sym(concept_col), !!sym(concept_label),
               mean_val, sd_val, median_val, iqr_val) %>%
      left_join(nsite_anom) %>%
      left_join(sitesanoms) %>%
      left_join(far_site) %>%
      left_join(close_site) %>%
      mutate(delim = sub("^([^,]+,){5}([^,]+).*", "\\2", site_anoms),
             site_anoms = ifelse(site_w_anom > 5,
                                 stringr::str_replace(site_anoms, paste0(",", delim, '(.*)'), ' . . .'),
                                 site_anoms)) %>%
      select(-delim) %>%
      gt::gt() %>%
      tab_header('Large N Anomaly Detection Summary Table') %>%
      cols_label(variable = 'Variable',
                 site_anoms = 'Site(s) with Anomaly',
                 mean_val = 'Mean',
                 sd_val = 'Standard Deviation',
                 median_val = 'Median',
                 iqr_val = 'IQR',
                 site_w_anom = 'No. Sites w/ Anomaly',
                 farthest_site = 'Site(s) Farthest from Mean',
                 closest_site = 'Site(s) Closest to Mean') %>%
      sub_missing(missing_text = 0,
                  columns = site_w_anom) %>%
      sub_missing(missing_text = '--',
                  columns = c(farthest_site, closest_site, site_anoms)) %>%
      fmt_number(columns = c(mean_val, median_val, sd_val, iqr_val),
                 decimals = 3) %>%
      opt_stylize(style = 2)

    if(!is.null(large_n_sites)){
      dat_to_plot <- dat_to_plot %>% filter(site %in% large_n_sites) %>%
        mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))

      plt<-ggplot(dat_to_plot,
                  aes(x=site, y=as.character(!!sym(concept_col)), text=text, color=!!sym(comparison_col)))+
        geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
        geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                               aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
        scale_color_squba(palette = 'diverging', discrete = FALSE) +
        scale_shape_manual(values=c(19,8))+
        scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
        theme_minimal() +
        #theme(axis.text.x = element_text(angle=60, hjust = 1, vjust = 1)) +
        labs(y = "Concept",
             size="",
             title=paste0('Anomalous Concepts for ', filtered_var, ' per Site'),
             subtitle = 'Dot size is the mean proportion per concept') +
        guides(color = guide_colorbar(title = 'Proportion'),
               shape = guide_legend(title = 'Anomaly'),
               size = 'none')

      plt[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                                  'tooltip' = TRUE)

      opt <- list(plt,
                  tbl)

      return(opt)
    }else{
      return(tbl)
    }
  }

}

#' **Multi-Site, Anomaly, Longitudinal**
#'
#' @param process_output output from `csd_process`
#' @param filter_concept the concept_id that should be used for the output
#' @param concept_col the name of the column from the concept_set used to identify concepts
#'                    should be either `concept_id` or `concept_code`
#' @param large_n a boolean indicating whether the large N visualization, intended for a high
#'                volume of sites, should be used; defaults to FALSE
#' @param large_n_sites a vector of site names that can optionally generate a filtered visualization
#'
#' @return three graphs:
#'    1) Loess smoothed line graph that shows the proportion of a code across time
#'    with the Euclidean Distance associated with each line
#'    2) same as (1) but displaying the raw, unsmoothed proportion
#'    3) a radial bar graph displaying the Euclidean Distance value for each
#'    site, where the color is the average proportion across time
#'
#' THIS GRAPH SHOWS ONLY ONE CONCEPT AT A TIME!
#'

csd_ms_anom_la <- function(process_output,
                           filter_concept,
                           concept_col,
                           large_n = FALSE,
                           large_n_sites = NULL){

  concept_col <- concept_col
  filt_op <- process_output %>% filter(!!sym(concept_col) == filter_concept)

  allsites <-
    filt_op %>%
    select(time_start,!!sym(concept_col),mean_allsiteprop) %>% distinct() %>%
    rename(prop_concept=mean_allsiteprop) %>%
    mutate(site='all site average') %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",prop_concept),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop_concept))

  iqr_dat <- filt_op %>%
    select(time_start,!!sym(concept_col),prop_concept) %>% distinct() %>%
    group_by(time_start, !!sym(concept_col)) %>%
    summarise(q1 = stats::quantile(prop_concept, 0.25),
              q3 = stats::quantile(prop_concept, 0.75))

  dat_to_plot <-
    filt_op %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop_concept,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  if(!large_n){
    p <- dat_to_plot %>%
      ggplot(aes(y = prop_concept, x = time_start, color = site, group = site, text = text_smooth)) +
      geom_line(data=allsites, linewidth=1.1) +
      geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
      scale_color_squba() +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
      labs(y = 'Proportion (Loess)',
           x = 'Time',
           title = paste0('Smoothed Proportion of ', filter_concept, ' Across Time'))

    q <- dat_to_plot %>%
      ggplot(aes(y = prop_concept, x = time_start, color = site,
                 group=site, text=text_raw)) +
      scale_color_squba() +
      geom_line(data=allsites,linewidth=1.1) +
      geom_line(linewidth=0.2) +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
      labs(x = 'Time',
           y = 'Proportion',
           title = paste0('Proportion of ', filter_concept, ' Across Time'))

    t <- dat_to_plot %>%
      distinct(site, dist_eucl_mean, site_loess) %>%
      group_by(site, dist_eucl_mean) %>%
      summarise(mean_site_loess = mean(site_loess)) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nEuclidean Distance: ', dist_eucl_mean,
                              '\nAverage Loess Proportion: ', mean_site_loess)) %>%
      ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
      geom_col_interactive() +
      # geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
      #           show.legend = FALSE) +
      coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
      guides(theta = guide_axis_theta(angle = 0)) +
      theme_minimal() +
      scale_fill_squba(palette = 'diverging', discrete = FALSE) +
      # theme(legend.position = 'bottom',
      #       legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
      #       axis.text.x = element_text(face = 'bold'))
      labs(fill = 'Avg. Proportion \n(Loess)',
           y ='Euclidean Distance',
           x = '',
           title = paste0('Euclidean Distance for ', filter_concept))

    p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

    q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

    t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)

    output <- list(p,
                   q,
                   t)
  }else{
    q <- ggplot(allsites, aes(x = time_start)) +
      geom_ribbon(data = iqr_dat, aes(ymin = q1, ymax = q3), alpha = 0.2) +
      geom_line(aes(y = prop_concept, color = site, group = site, text=text_raw), linewidth=1.1) +
      geom_line(data = dat_to_plot %>% filter(site %in% large_n_sites),
                aes(y = prop_concept, color = site, group = site, text=text_raw),
                linewidth=0.2) +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
      scale_color_squba() +
      labs(x = 'Time',
           y = 'Proportion',
           title = paste0('Proportion of ', filter_concept, ' Across Time'),
           subtitle = 'Ribbon boundaries are IQR')

    if(is.null(large_n_sites)){

      t <- dat_to_plot %>%
        distinct(!!sym(concept_col), dist_eucl_mean) %>%
        ggplot(aes(x = dist_eucl_mean, y = !!sym(concept_col))) +
        geom_boxplot() +
        geom_point(color = 'gray',
                   alpha = 0.75) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              legend.title = element_blank()) +
        scale_fill_squba(palette = 'diverging', discrete = FALSE) +
        labs(x ='Euclidean Distance',
             y = '',
             title = paste0('Distribution of Euclidean Distances'))

    }else{
      t <- dat_to_plot %>%
        distinct(!!sym(concept_col),dist_eucl_mean) %>%
        ggplot(aes(x = dist_eucl_mean, y = !!sym(concept_col))) +
        geom_boxplot() +
        geom_point(data = dat_to_plot %>% filter(site %in% large_n_sites),
                   aes(color = site)) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              legend.title = element_blank()) +
        scale_fill_squba(palette = 'diverging', discrete = FALSE) +
        scale_color_squba() +
        labs(x ='Euclidean Distance',
             y = '',
             title = paste0('Distribution of Euclidean Distances'))
    }

    output <- q + t + plot_layout(ncol = 1, heights = c(5, 1))
  }

  return(output)
}
