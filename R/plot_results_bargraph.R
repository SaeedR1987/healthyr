#' Plot Bar Graph Single Variable
#'
#' This function takes a single variable result from a dataframe of results produced by
#' healthyr::analyse_survey_results and creates a bar chart with confidence intervals.
#' If specified can also produce grouped bar charts. Intended for use on already
#' aggregated results, uses stat = "identity".
#'
#' @param results_df Inputs a dataframe of results produced from the
#' healthyr::analyse_survey_results function.
#' @param col_results Inputs a character vector of column names that are formatted as
#' results with CIs in square brackets.
#' @param var_to_plot Inputs a character value specifying the variable to plot.
#' @param var_to_group Inputs a character value specifying the variable to group by, for
#' grouped bar charts.
#' @param file_path Inputs an optional character value specifying the file path location to save
#' an image of the plot. Recommended to specify as a .jpg or .png file.
#' @param wdth Inputs an optional numeric value for the width of the image.
#' @param hght Inputs an optional numeric value fo the height of the image.
#' @param title_name Inputs an optional character value for the title of the plot.
#'
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{plot_results_bargraph(results_df, col_results = c(names(df[,-1]),
#'  var_to_plot = "gam_wfhz_noflag", var_to_group = "admin2") ))}
#' @importFrom rlang .data
plot_results_bargraph <- function(results_df, col_results, var_to_plot, var_to_group, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  results_df <- healthyr::break_apart_cis(results_df, result_cols = col_results) %>%
    dplyr::filter(.data$variable == var_to_plot) %>%
    dplyr::filter(!is.na(.data$point.estimate))

  g <- ggplot2::ggplot(data = results_df, mapping = ggplot2::aes(x = stats::reorder(get(var_to_group), +as.numeric(.data$point.estimate)), y = as.numeric(.data$point.estimate), fill = grDevices::rgb(238, 88, 89, maxColorValue = 255))) +
    ggplot2::geom_bar(stat = "identity") + ggplot2::geom_text(ggplot2::aes(label = .data$point.estimate), vjust = -1) +
    ggplot2::geom_errorbar(ggplot2::aes(x = get(var_to_group), ymin = as.numeric(.data$lower_ci), ymax = as.numeric(.data$upper_ci)), width = 0.3, colour = "black", alpha = 0.9, size = 0.9)+
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none") + ggplot2::labs(x = "Variable", y = "Propotion (%)") + ggplot2::coord_flip()

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
