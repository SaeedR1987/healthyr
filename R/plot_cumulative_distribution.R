#' Plot Cumulative Distribution
#'
#' Plots a cumulatative distribution of MUAC or an anthropometric index.
#'
#' @param df A data frame with MUAC data, each row is 1 child.
#' @param index Inputs a character value for which anthropometric index to plot. Use 'wfhz' for weight-for-height,
#' 'hfaz' for height-for-age, 'wfaz' for weight-for-age, 'mfaz' for MUAC-for-age, or 'muac' for MUAC.
#' @param flags Inputs a character value of 'yes' or 'no'. Yes specifies to include flagged values in the plot. No specifies
#' to exclude flagged values in the plot.
#' @param grouping Inputs an optional character value to facet the plot by.#' @return Returns a ggplot cumulative distribution plot of MUAC measurements.
#' @param file_path Inputs an optional character value specifying the file path location to save
#' an image of the plot. Recommended to specify as a .jpg or .png file.
#' @param wdth Inputs an optional numeric value for the width of the image.
#' @param hght Inputs an optional numeric value fo the height of the image.
#' @param title_name Inputs an optional character value for the title of the plot.
#' @export
#'
#' @examples
#' \dontrun{plot_cumulative_distribution(df, index = "muac", flags = "yes", grouping = "enum")}
#' @importFrom rlang .data
plot_cumulative_distribution <- function(df, index, flags, grouping = NULL, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  options(warn = -1)
  anthro_cols <- c("wfhz_noflag", "hfaz_noflag", "wfaz_noflag", "mfaz_noflag", "muac_noflag")
  valid_indexes <- c("wfhz", "hfaz", "wfaz", "mfaz", "muac")

  if(length(setdiff(index, valid_indexes))!= 0) {stop("No valid index was specified. Valid inputs include 'wfhz', 'hfaz', 'wfaz', or 'mfaz'.")}

  if(is.null(flags)) {flags <- "yes"} else if(length(setdiff(flags, c("yes", "no")))!= 0) {stop("Please include a valid input for the flags argument. Use 'yes' to include flagged values in the plot. Use 'no' to exclude flagged values from the plot.")}

  if(index == "wfhz") {
    if(flags == "yes") {index <- "wfhz"} else {index <- "wfhz_noflag"}
  } else if(index == "hfaz") {
    if(flags == "yes") {index <- "hfaz"} else {index <- "hfaz_noflag"}
  } else if(index == "wfaz") {
    if(flags == "yes") {index <- "wfaz"} else {index <- "wfaz_noflag"}
  } else if(index == "mfaz") {
    if(flags == "yes") {index <- "mfaz"} else {index <- "mfaz_noflag"}
  } else if(index == "muac") {
    if(flags == "yes") {index <- "muac"} else {index <- "muac_noflag"}
  }

  df <- df %>% dplyr::rename(indx = {{index}})

  if(index == "muac" | index == "muac_noflag") {
    minval <- 6.0
    maxval <- 22.0
    brkval <- 0.5
    severe <- 11.5
    moderate <- 12.5
  } else {
    minval <- (-6)
    maxval <- 6
    brkval <- 0.5
    severe <- (-3)
    moderate <- (-2)
  }

  g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$indx, color = "Overall")) + ggplot2::stat_ecdf(geom = "step") + ggplot2::geom_vline(xintercept = severe) + ggplot2::geom_vline(xintercept = moderate)

  if(!missing(grouping)) {

    g <- g + ggplot2::stat_ecdf(geom = "step", ggplot2::aes(x = .data$indx, color = as.factor(get(grouping)), group = get(grouping))) + ggplot2::geom_vline(xintercept = severe) + ggplot2::geom_vline(xintercept = moderate)
    g <- g + ggplot2::labs(color=paste0(grouping))

  }

  values <- df %>% dplyr::select(grouping) %>% t %>% c %>% unique()
  colors <- c("red", "blue", "green", "orange", "purple", "dodgerblue", "magenta", "cyan", "deeppink", "mediumblue", "darkgreen")

  colors <- colors[1:length(values)]
  colors <- c(colors, "black")

  if(missing(grouping)) {
    colors <- "black"
    }

  g <- g + ggplot2::xlim(c(minval,maxval)) + ggplot2::theme_minimal() + ggplot2::xlab(index) + ggplot2::ylab("% Cumulative Proportion")
  g <- g + ggplot2::scale_x_continuous(breaks = seq(minval, maxval, by = brkval))
  g <- g + ggplot2::scale_color_manual(values = colors)

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}
  options(warn = 0)

  return(g)
}
