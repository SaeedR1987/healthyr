#' Plot Age Distribution in Months
#'
#' This function creates a histogram of child ages under 5 years of age.
#'
#' @param df Inputs a dataframe that has been processed and standardized by
#' the healthyr::format_nut_health_indicators function, and has an age_months
#' column.
#' @param by_group Inputs a character value specifying a column to facet the
#' the results by.
#' @param file_path Inputs an optional character value specifying the file path location to save
#' an image of the plot. Recommended to specify as a .jpg or .png file.
#' @param wdth Inputs an optional numeric value for the width of the image.
#' @param hght Inputs an optional numeric value fo the height of the image.
#' @param title_name Inputs an optional character value for the title of the plot.
#'
#'
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{plot_age_months_distribution(df, by_group = "enum")}
#' @importFrom rlang .data
plot_age_months_distribution <- function(df, by_group = NULL, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  if( !(c("age_months") %in% colnames(df))) {stop("There is no age_months column in your dataframe. Please check your inputs.")}

  if(max(df$age_months>59)) {
    print("Ages >59 months detected, removed for this graph.")
    df <- df %>% dplyr::filter(.data$age_months <60)
  }

  if(is.null(by_group)) {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$age_months)) + ggplot2::geom_histogram(binwidth = 1) + ggplot2::scale_x_continuous(minor_breaks = seq(0,60,by=1), breaks = seq(0,60,by=12), limits = c(0,60))
  } else {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$age_months)) + ggplot2::geom_histogram(binwidth = 1) + ggplot2::scale_x_continuous(minor_breaks = seq(0,60,by=1), breaks = seq(0,60,by=12), limits = c(0,60)) + ggplot2::facet_wrap(~get(by_group), ncol = 1)
  }

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)
}
