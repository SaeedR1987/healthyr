


plot_domain_radar <- function(df,
                              domain_cols,
                              domain_labels = NULL,
                              grouping = NULL,
                              max_value = NULL,
                              file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  library(ggradar)

  a <- 0
  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
    a <- 1
  }

  # if domain labels is NULL, set defaul to domain_cols

  if(is.null(domain_labels)) {domain_labels <- domain_cols}

  # check if length domain_cols == domain_labels

  if( !(length(domain_cols) == length(domain_labels) )) {stop("The number of entries for domain_cols must equal the number of entries for domain_labels.")}

  # check if domain cols are 0/1 in the dataset

  # convert to numeric and handle NAs if they exist. ggradar won't work with NAs

  df[domain_cols] <- lapply(df[domain_cols], as.numeric)

  df[c(domain_cols)][is.na(df[c(domain_cols)])] <- 0

  # create a summary table of the domain results, % reported for each domain

  summary <- df %>%
    dplyr::select(grouping, domain_cols) %>%
    dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::summarise_at(.vars = domain_cols, .funs = mean)

  obs_max <- max(summary[,-1])

  if(is.null(max_value)) {max_value <- obs_max*1.1}


  g <- ggradar(summary, grid.max = max_value, base.size = 1, group.point.size = 1, grid.line.width = 1, values.radar =c(paste0("0%"), paste0(round((max_value/2)*100,2), "%"), paste0(round((max_value)*100,2), "%") ),
               grid.label.size = 3, axis.label.size = 2, legend.position = "none", axis.labels = domain_labels, grid.mid = max_value/2, grid.min = 0) + facet_wrap(facets = "group" ) + theme(strip.text = element_text(size = 15))

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(!is.null(file_path)) {ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
