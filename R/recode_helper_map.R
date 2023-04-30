
#' Recode Helper Map
#'
#' Helper function to update the map dataframe based on user console inputs
#' for the recoding steps
#'
#' @param df The dataframe
#' @param column A character value of the column name to recode.
#' @param old_val The old value to recode.
#' @param new_val The new value to recode.
#'
#' @return Returns a re-coded dataframe for this single variable.
#' @export
#'
#' @examples
recode_helper_map <- function(df, column, old_val, new_val) {

  df <- df %>% dplyr::mutate_at(
    dplyr::vars("new_value"),
    list(as.character))

  if(!is.na(old_val)){

    if(new_val == "NA") {

      df <- df %>%
        dplyr::mutate_at(
          dplyr::vars("new_value"),
          list(~dplyr::case_when(.data$value == old_val & .data$new_variable == column ~ NA_character_,
                                TRUE ~ .)))

    } else {

      df <- df %>%
        dplyr::mutate_at(
          dplyr::vars("new_value"),
          list(~dplyr::case_when(.data$value == old_val & .data$new_variable == column ~ new_val,
                                TRUE ~ .)))
    }
  }

  return(df)

}
