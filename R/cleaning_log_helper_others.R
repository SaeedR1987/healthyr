
#' Cleaning Log Helper for Other Variables
#'
#' Helper function for the create cleaning log flags function in healthyr. This will create
#' cleaning log records for each other reported value in the dataset, for all columns ending
#' in "_other".
#'
#' @param df Inputs the dataframe. Must have columns ending in _other.
#' @param uuid Inputs a character value of the column with UUID, or other household identifier for cleaning purposes.
#'
#' @return Returns a REACH style cleaning log with all the 'other' responses that need to be checked.
#' @export
#'
#' @examples
#' \dontrun{cleaning_log_helper_others(df = mydata, uuid = "_uuid")}
cleaning_log_helper_others <- function(df, uuid) {

  cols <- grep(pattern = "_other", x = colnames(df), value = TRUE)

  cols2 <- stringr::str_remove(string = cols, pattern = "_other")

  description = "Other values, check if should be recoded."

  df <- df %>% dplyr::mutate(n = dplyr::row_number())

  df <- df %>% dplyr::rename(uuid = {{uuid}}) %>%
    dplyr::select(.data$n, uuid, cols, cols2) %>%
    tidyr::gather(key = "question.name",value = "old.value", c(cols,cols2) ) %>%
    # dplyr::rename(drop = {{flag}}) %>% dplyr::filter(drop != 0) %>%
    dplyr::mutate(drop = NULL, issue = paste0("flag_", .data$question.name)) %>%
    dplyr::mutate(new.value = "", feedback = "", changed = "") %>%
    dplyr::select(.data$uuid, .data$question.name, .data$issue, .data$feedback, .data$changed, .data$old.value, .data$new.value) %>%
    dplyr::mutate(description = description) %>%
    dplyr::filter(!is.na(.data$old.value))

  return(df)

}
