#' Format Results Long Helper function
#'
#' This function takes the output of srvyr summary analysis that uses vartype = "ci", and reformats the results
#' so the point estimate and confidence intervals are presented in a single cell together.
#'
#' @param df Inputs a dataframe that contains aggregated results from a srvyr summary. There should be columns included
#' for point.estimate, lower_ci, and upper_ci.
#' @param vars Inputs a character vector specifying the names of the columns to be reshaped, including the point.estimate and lower_ci and upper_ci columns.
#' Should not include the grouping column.
#' @param not_proportions Inputs a character vector specifying the names of the columns that are not proportions, as proporiton results
#' will be multiplied by 100 within the function, and confidence intervals limited to 0 and 100.
#'
#' @return Returns a dataframe of the reshaped results with point.estimates and confidence intervals in the same cells,
#' and one column for each variable included.
#' @export
#'
#' @examples
#' \dontrun{format_results_long(df = myresults, vars = c("variable1",
#' "variable1_low", "variable1_upp", "variable2", "variable2_low", "variable2_upp"),
#' not_proportions = c("avg_hh_size", "num_children_under5"))}
#'
#' @importFrom rlang .data
format_results_long <- function(df, vars, not_proportions) {

  not_prop_low <- paste0(not_proportions, "_low")
  not_prop_upp <- paste0(not_proportions, "_upp")

  df <- df %>%
    tidyr::gather(key = "key", value = "value", vars) %>%
    dplyr::mutate(value = ifelse( !(.data$key %in% not_proportions) & !(.data$key %in% not_prop_low) & !(.data$key %in% not_prop_upp) &  .data$value < 0, 0, .data$value),
           value = ifelse( !(.data$key %in% not_proportions) & !(.data$key %in% not_prop_low) & !(.data$key %in% not_prop_upp) &  .data$value > 1, 1, .data$value)) %>%
    dplyr::mutate(spread_key = stringr::str_extract(.data$key, '_low|_upp'),
           spread_key = ifelse(is.na(.data$spread_key), "value",
                               ifelse(.data$spread_key == "_low", "lower_ci",
                                      ifelse(.data$spread_key == "_upp", "upper_ci", NA))),
           key = stringr::str_remove(string = .data$key, pattern = "_low"),
           key = stringr::str_remove(string = .data$key, pattern = "_upp")) %>%
    tidyr::spread(key = .data$spread_key, value = .data$value) %>%
    dplyr::mutate(value = ifelse( !(.data$key %in% not_proportions) & !(.data$key %in% not_prop_low) & !(.data$key %in% not_prop_upp),
                           paste0(round(.data$value, 3)*100, " [", round(.data$lower_ci, 3)*100, "-", round(.data$upper_ci, 3)*100, "]"),
                           paste0(round(.data$value, 3), " [", round(.data$lower_ci, 3), "-", round(.data$upper_ci, 3), "]")),
           lower_ci = NULL, upper_ci = NULL) %>%
    tidyr::spread(key = .data$key, value = .data$value)

  return(df)

}
