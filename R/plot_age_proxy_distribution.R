#' Plot Age Proxy Distribution
#'
#' This function plots a stacked bar graph showing the distribution of
#' children under 2 years of age to over 2 years of age.
#'
#' @param df Inputs a dataframe that has been processed and standardized
#' by the healthyr::format_nut_health_indicators function.
#' @param by_group Inputs an optional character value specifying the column
#' by which to facet the resulting plot.
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
#' \dontrun{plot_age_proxy_distribution(df, by_group = "enum", )}
#' @importFrom rlang .data
plot_age_proxy_distribution <- function(df, by_group = NULL, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  if( !(c("age_proxy") %in% colnames(df))) {stop("There is no age_proxy column in your dataframe. Please check your inputs.")}

  if(is.null(by_group)) {
    df2 <- df %>% dplyr::group_by(.data$age_proxy) %>% dplyr::mutate(n = dplyr::n())
    g <- ggplot2::ggplot(data = df2, ggplot2::aes(x = .data$age_proxy, y = .data$n)) + ggplot2::geom_bar(stat = "identity")
  } else {
    df2 <- df %>% dplyr::group_by(.data$age_proxy, get(by_group)) %>% dplyr::mutate(n = dplyr::n())
    g <- ggplot2::ggplot(data = df2, ggplot2::aes(fill = .data$age_proxy, y = .data$n, x = get(by_group))) + ggplot2::geom_bar(position = "fill", stat = "identity")
  }

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}


  return(g)

}
