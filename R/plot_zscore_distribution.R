#' Plot Z-Score Distribution
#'
#' Plot the distribution of an anthropometric z-score in order to visualize assess the distribution
#' against a normal Gaussian curve.
#'
#' @param df Inputs a dataframe with anthropometric z-scores that has already been processed
#' and standardized by the healthyr::format_nut_health_indicators function.
#' @param index Inputs a character value for which anthropometric index to plot. Use 'wfhz' for weight-for-height,
#' 'hfaz' for height-for-age, 'wfaz' for weight-for-age, or 'mfaz' for MUAC-for-age.
#' @param flags Inputs a character value of 'yes' or 'no'. Yes specifies to include flagged values in the plot. No specifies
#' to exclude flagged values in the plot.
#' @param grouping Inputs an optional character value to facet the plot by.
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
#' \dontrun{plot_zscore_distribution(df, index = "wfhz", file_path = "myplots/wfhz_plot.jpg",
#' wdth = 10, hght = 8, title_name = "Distribution of WFH Z-scores, County X")}
#' @importFrom rlang .data
#' @importFrom stats density
plot_zscore_distribution <- function(df, index, flags, grouping = NULL, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {
  options(warn = -1)
  anthro_cols <- c("wfhz_noflag", "hfaz_noflag", "wfaz_noflag", "mfaz_noflag")
  valid_indexes <- c("wfhz", "hfaz", "wfaz", "mfaz")

  if(length(setdiff(index, valid_indexes))!= 0) {stop("No valid index was specified. Valid inputs include 'wfhz', 'hfaz', 'wfaz', or 'mfaz'.")}

  if(is.null(flags)) {flags <- "yes"} else if(length(setdiff(flags, c("yes", "no")))!= 0) {stop("Please include a valid input for the flags argument. Use 'yes' to include flagged values in the plot. Use 'no' to exclude flagged values from the plot.")}

  if(index == "wfhz") {
    if(flags == "yes") {var <- "wfhz"} else {var <- "wfhz_noflag"}
  } else if(index == "hfaz") {
    if(flags == "yes") {var <- "hfaz"} else {var <- "hfaz_noflag"}
  } else if(index == "wfaz") {
    if(flags == "yes") {var <- "wfaz"} else {var <- "wfaz_noflag"}
  } else if(index == "mfaz") {
    if(flags == "yes") {var <- "mfaz"} else {var <- "mfaz_noflag"}
  }

  if(length(setdiff(var, colnames(df)))!=0) {stop("The specific anthropometric column indicated is not in your dataframe. Please check your input.")}

  if(is.null(grouping)) {

    data_norm <- as.data.frame(graphics::curve(stats::dnorm(x, mean = 0, sd = 1), from = -6, to = 6))

    g <- ggplot2::ggplot(df, ggplot2::aes(get(var))) +
      ggplot2::geom_histogram(ggplot2::aes(x=get(var), y=ggplot2::after_stat(density)), bins=100, fill="#d3d3d3", color="gray", alpha = 0.8) +
      ggplot2::geom_density(color="blue", size = 1) +  ggplot2::xlim(c(-6, 5)) +
      ggplot2::geom_line(data = data_norm, ggplot2::aes(x = .data$x, y = .data$y), color = "darkred", size = 1.2) +
      ggplot2::geom_vline(xintercept = c(-3, 3), color = "red", size = 0.5) +
      ggplot2::geom_vline(xintercept = c(-2, 2), color = "orange") + ggplot2::xlab(index) + ggplot2::theme_minimal()

  }

  if(!is.null(grouping)) {

    data_norm <- as.data.frame(graphics::curve(stats::dnorm(x, mean = 0, sd = 1), from = -6, to = 6))

    df <- df %>% dplyr::rename(group_var = {{grouping}})

    g <- ggplot2::ggplot(df, ggplot2::aes(x = get(var), color = as.factor(get("group_var")))) +
      ggplot2::geom_density(size = 1) +  ggplot2::xlim(c(-6, 5)) +
      ggplot2::geom_line(data = data_norm, ggplot2::aes(x = .data$x, y = .data$y), color = "darkred", size = 1.2) +
      ggplot2::geom_vline(xintercept = c(-3, 3), color = "red", size = 0.5) +
      ggplot2::geom_vline(xintercept = c(-2, 2), color = "orange") + ggplot2::xlab(index) + ggplot2::theme_minimal() + ggplot2::labs(color=paste0(grouping))

  }

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}
  options(warn = 0)

  return(g)

}
