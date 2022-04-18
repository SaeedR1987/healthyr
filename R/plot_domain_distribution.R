#' Plot Select Multiple Across Domains
#'
#' This functions plots a count distribution of responses for a select multiple question
#' and shows their responses distributed against some domain categorization of those
#' response. E.g A health barriers question with several response options, however those
#' response options can be grouped by barriers related to availability, accessibility, or
#' quality of services. Each response option should only belong to one domain category.
#'
#' @param df Inputs a dataframe.
#' @param domain_vars Inputs a character vector of column names for domain categories. Each domain
#' should have its own column coded as 1/0.
#' @param response_vars Inputs a character vector of column names for response categories for
#' the select multiple question coded as 1/0.
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
#' \dontrun{plot_domain_distribution(df, domain_vars = c("availability",
#' "accessibility", "quality"), response_vars = c("barrier.distance",
#' "barrier.nostaff", "barrier.costtransport", "barrier.rudestaff"))}
#' @importFrom rlang .data
plot_domain_distribution <- function(df, domain_vars, response_vars, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  df[response_vars] <- lapply(df[response_vars], as.numeric)

  df <- df %>%
    dplyr::select(domain_vars, response_vars) %>%
    tidyr::gather(key = "domain", value = "value", domain_vars) %>%
    tidyr::gather(key = "response", value = "value2", response_vars) %>%
    dplyr::filter(.data$value != "0") %>%
    dplyr::group_by(.data$domain, .data$response) %>%
    dplyr::summarize(value2 = sum(.data$value2, na.rm = TRUE)) %>%
    dplyr::group_by(.data$response) %>%
    dplyr::filter(.data$value2 == max(.data$value2)) %>%
    dplyr::mutate(domain = haven::as_factor(.data$domain),
           domain = stringr::str_remove(string = .data$domain, pattern = "health6_barrier."),
           response = haven::as_factor(.data$response)) #%>%
  # mutate(response = fct_reorder(.f = response, .x = value2, .desc = TRUE))

  g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = stats::reorder(.data$response, +.data$value2), y = .data$value2, fill = .data$domain)) + ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::xlab("response") + ggplot2::ylab("count")

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
