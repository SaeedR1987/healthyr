#' Create Value Map
#' Creates a map of the categorical/factor variables in the dataset.
#'
#' @param df A dataframe object.
#'
#' @return Returns a dataframe with mapped variables and values in a condensed
#' format, and also adds a "new_value" column for recoding with other functions.
#' @export
#'
#' @examples
create_value_map <- function(df) {

  defaultW <- getOption("warn")
  options(warn = -1)

  df_map <- healthyr::select_non_numeric_cols(df)
  df_map <- healthyr::select_non_date_cols(df_map)
  df_map <- healthyr::select_non_other_columns(df_map)

  n <- nrow(df_map)

  map <- df_map %>%
    tidyr::gather(key = "variable", value = "value", na.rm = T, convert = T, factor_key = F) %>%
    dplyr::group_by(.data$variable, .data$value) %>%
    dplyr::summarise(n_values = dplyr::n_distinct(.data$value)) %>% dplyr::ungroup()

  options(warn = defaultW)

  return(as.data.frame(
    map %>%
      dplyr::mutate(n_values = NULL,
                    new_value = ""))
           )

}
