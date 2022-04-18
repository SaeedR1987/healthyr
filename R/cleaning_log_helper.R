#' Cleaning Log Helper
#'
#' Function to facilitate creation of cleaning log segments for single types of flags.
#'
#' @param df Inputs a dataframe with flags, and selection of columns with values to clean if record is flagged
#' @param uuid Inputs a character value of the uuid column name.
#' @param flag Inputs a character value of the column specifying flagged records for a single type of issue.
#' Flag columns must be coded as a 1 for flagged, or 0 if no flag or issue.
#' @param cols Inputs a character vector of column names that should be included in the exported cleaning log.
#' @param description Inputs a character, or brief custom description of the type of flag. To be included in the cleaning log.
#'
#' @return Returns a REACH style cleaning log listing all flags of the specified type, and variables/values to be checked or cleaned
#' because of that flag for all flagged records.
#' @export
#'
#' @examples
#' \dontrun{cl <- cleaning_log_helper(df = df, uuid = "uuid_col", flag = "flag_wgss_age",
#' cols = c("age_years", wgss_vars), description = "Washington Group Short Set values
#' are present for a person under-5 years of age. This indicator set is not applicable for
#' this age group.")}
#' @importFrom rlang .data
cleaning_log_helper <- function(df, uuid, flag, cols, description = NULL) {

  if(is.null(description)) {description = ""}

  df <- df %>% dplyr::mutate(n = dplyr::row_number())

  df <- df %>%
    dplyr::rename(uuid = {{uuid}}) %>%
    dplyr::select(.data$n, flag, uuid, cols) %>%
    tidyr::gather(key = "question.name", value = "old.value", cols) %>%
    dplyr::rename(drop = {{flag}}) %>%
    dplyr::filter(drop != 0) %>%
    dplyr::mutate(drop = NULL, issue = .data$flag) %>%
    dplyr::mutate(new.value = "", feedback = "", changed = "") %>%
    dplyr::select(.data$uuid, .data$question.name, .data$issue, .data$feedback, .data$changed, .data$old.value, .data$new.value) %>%
    dplyr::mutate(description = description)

  return(df)

}
