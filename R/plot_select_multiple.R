#' Plot Select Multiple Results
#'
#' This function creates a plot results for a select multiple indicator that has been
#' aggregated already through the healthyr::analyse_survey_results function.
#'
#' @param results_df Inputs a dataframe of results from the healthyr::analyse_survey_results
#' function.
#' @param select_multiple_cols Inputs a character vector of column names for the select
#' multiple question to plot.
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
#' \dontrun{plot_select_multiple(results_df = myresults,
#' select_multiple_cols = c("barrier.1", "barrier.2", "barrier.3"))}
#' @importFrom rlang .data
plot_select_multiple <- function(results_df, select_multiple_cols, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  results_df <- healthyr::break_apart_cis(results_df, result_cols = names(results_df)) %>%
    dplyr::filter(.data$variable %in% select_multiple_cols)

  g <- ggplot2::ggplot(data = results_df, mapping = ggplot2::aes(x = stats::reorder(.data$variable, as.numeric(.data$point.estimate)), y = as.numeric(.data$point.estimate), fill = grDevices::rgb(238, 88, 89, maxColorValue = 255) )) + ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_errorbar(ggplot2::aes(x = .data$variable, ymin = as.numeric(.data$lower_ci), ymax = as.numeric(.data$upper_ci)), width = 0.3, colour = "black", alpha = 0.9, size = 0.9)+
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none") + ggplot2::labs(x = "Variable", y = "Proportion (%)") + ggplot2::coord_flip()

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
