
#' Summarize Value by Group
#'
#' Creates a summary table of the number of times a specified value appears in each dataframe column, by group.
#' For summarizing missing values, use instead healthyr::summarize_NA_by_group
#'
#' @param df Inputs a dataframe
#' @param grouping Inputs a character value of the column name by which to group the results.
#' @param val Inputs a character value of the value that you want to summarize across groups and columns.
#'
#' @return Returns a dataframe of a summary table counting the number of occurences of the value in each column, by group.
#' @export
#'
#' @examples
#' \dontrun{summarize_value_by_group(df = mydata, grouping = "enum", val = "dontknow")}
summarize_value_by_group <- function(df, grouping, val) {

  cols <- colnames(df)
  cols <- setdiff(cols, grouping)

  df <- df %>%
    rename(group = {{grouping}}) %>%
    gather(key = "column.name", value = "value", cols) %>%
    mutate(count = ifelse(is.na(value), NA, ifelse(value == val, 1, 0))) %>%
    group_by(group, column.name) %>%
    summarize(n = sum(count, na.rm = TRUE)) %>%
    spread(key = group, value = n)


  return(df)

}
