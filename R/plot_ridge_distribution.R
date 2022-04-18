#' Plot Ridge Distributions
#'
#' Creates a distribution plot of several numeric variables side by side for comparison.
#'
#' @param df Inputs a dataframe.
#' @param numeric_cols Inputs a character vector specifying column names of the numeric
#' variables to plot.
#' @param name_groups Inputs an optional character value specifying the names of the groups.
#' @param name_units Inputs an optional character value specifying the names of the units.
#' @param grouping Inputs an optional character value specifying the name of a column by which
#' to facet the plot.
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
#' \dontrun{plot_ridge_distribution(df, numeric_cols = c("fcs_cereal", "fcs_dairy", ...),
#' name_groups = "Food Groups", name_units = "Days", grouping = "enum")}
#' @importFrom rlang .data
plot_ridge_distribution <- function(df, numeric_cols, name_groups = NULL, name_units = NULL, grouping = NULL,
                                     file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  a <- 0
  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
    a <- 1
  }
  if(is.null(name_groups)) {name_groups <- "Groups"}
  if(is.null(name_units)) {name_units <- "Units"}

  df <- df %>% dplyr::select(grouping, numeric_cols) %>%
    tidyr::gather(key = !!name_groups, value = !!name_units, numeric_cols)

  g <- ggplot2::ggplot(df, ggplot2::aes(x = get(name_units), y = get(name_groups), fill = get(name_groups))) +
    ggridges::geom_density_ridges() +
    ggridges::theme_ridges() + ggplot2::xlab(name_units) + ggplot2::ylab(name_groups) +
    ggplot2::theme(legend.position = "none", legend.title = ggplot2::element_text(name_groups))

  if(a == 0) {
    g <- g + ggplot2::facet_wrap(~get(grouping))
  }

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}

