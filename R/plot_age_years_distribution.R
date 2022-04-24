#' Plot Age Distribution in Years
#'
#' This functions creates a histogram of reported ages in years.
#'
#' @param df Inputs a dataframe that has been processed and standardized by the
#' healthyr::format_nut_health_indicators and has an age_years column.
#' @param by_group Inputs an optional character value specifying a column by which
#' to facet the plot.
#' @param min_age Inputs a numeric value for the minimum age in years to include in the plot.
#' @param max_age Inputs a numeric value for the maximum age in years to include in the plot.
#' @param breaks Inputs a numeric value for how many years between ticks to plot.
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
#' \dontrun{plot_age_years_distribution(df, by_group = "enum", min_age = 5, max_age = 100)}
#' @importFrom rlang .data
plot_age_years_distribution <- function(df, by_group = NULL, min_age = NULL, max_age = NULL, breaks = NULL, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  if( !(c("age_years") %in% colnames(df))) {stop("There is no age_years column in your dataframe. Please check your inputs.")}

    if(is.null(min_age)) {
      min_age <- 0
      print("No minimum age specified. Defaulting to 0 years.")}
    if(is.null(max_age)) {
      max_age <- 5
      print("No maximum age specified. Defaulting to 5 years.")
    }
  if(is.null(breaks)) {
    breaks <- 1
  }

    df <- df %>% dplyr::filter(.data$age_years >= min_age & .data$age_years <= max_age)

    if(is.null(by_group)) {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = get("age_years") )) + ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age,max_age,by=), breaks = seq(min_age,max_age,by=breaks), limits = c(min_age,max_age))
    } else {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = get("age_years"))) + ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age,max_age,by=1), breaks = seq(min_age,max_age,by=breaks), limits = c(min_age,max_age)) + ggplot2::facet_wrap(~get(by_group), ncol = 1)
    }

    if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

    if(is.null(wdth)) {wdth <- 5}
    if(is.null(hght)) {hght <- 5}

    if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
