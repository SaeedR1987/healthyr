#' Plot Variables by Group
#'
#' This function creates bar charts showing counts of responses by group.
#'
#' @param df Inputs a dataframe.
#' @param vars Inputs a character vector of column names to plot by group.
#' @param grouping Inputs a character value specifying the column by which to
#' facet the plot.
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
#' \dontrun{plot_vars_by_group(df, vars = c("availability", "accessibility", "quality"),
#' grouping = "unmet_need")}
#' @importFrom rlang .data
plot_vars_by_group <- function(df, vars, grouping, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  df[vars] <- lapply(df[vars], as.numeric)

  df <- df %>%
    dplyr::select(vars, grouping) %>%
    tidyr::gather(key = "variable", value = "value", vars) %>%
    #filter(value != "0") %>%
    dplyr::group_by(.data$variable, !!rlang::sym(grouping)) %>%
    dplyr::summarize(value = sum(.data$value, na.rm = TRUE)) %>%
    #dplyr::group_by(response) %>%
    #dplyr::filter(value == max(value)) %>%
    dplyr::mutate(variable = haven::as_factor(.data$variable),
           domain = stringr::str_remove(string = .data$variable, pattern = "health6_barrier."),
    )

  g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = stats::reorder(.data$variable, +.data$value), y = .data$value, fill = grDevices::rgb(238, 88, 89, maxColorValue = 255))) + ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::xlab("Variables") + ggplot2::ylab("Count of Responses") + ggplot2::facet_wrap(~get(grouping)) + ggplot2::theme(legend.position = "none")

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
