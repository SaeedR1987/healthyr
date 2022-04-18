#' Plot Correlogram
#'
#' This function wraps the GGally package to facilitate making correlation plots
#' to visually assess the relationships between several numeric variables.
#'
#' @param df Inputs a dataframe.
#' @param numeric_cols Inputs a character vector of specifying the column names
#' of the numeric variables to use.
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
#' \dontrun{plot_correlogram(df, numeric_cols = c("fcs_score", "rcsi_score", "hhs_score"))}
plot_correlogram <- function(df, numeric_cols, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  g <- GGally::ggpairs(data = df, columns = numeric_cols)

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)
}
