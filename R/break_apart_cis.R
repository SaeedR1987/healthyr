#' Break Apart CIs
#'
#' Helper function to facilitate reformatting results from healthyr::analyse_survey_results
#' so they can be more easily used to plot results. It is intended to be called within several of the healthyr
#' plotting functions, and not genererally used directly.
#'
#' @param df Inputs a dataframe that has been returned from healthyr::analyse_survey_results.
#' @param result_cols Inputs a character vector of the results column names.
#'
#' @return Returns a dataframe that has reshaped long
#' @export
#'
#' @examples
#' \dontrun{break_apart_cis(df, result_cols = c("result_col1", "result_col2", ...))}
#' @importFrom rlang .data
break_apart_cis <- function(df, result_cols) {

  df <- df %>%
    tidyr::gather(key = "variable", value = "value", result_cols)

  df <- df %>% tidyr::separate(col = .data$value, into = c("point.estimate", "ci"), sep = " ")

  df <- df %>%
    dplyr::mutate(ci = stringr::str_remove(string = .data$ci, pattern = "\\["),
           ci = stringr::str_remove(string = .data$ci, pattern = "\\]"),
    )

  df <- df %>% tidyr::separate(col = .data$ci, into = c("lower_ci", "upper_ci"), sep = "-")

  return(df)
}
