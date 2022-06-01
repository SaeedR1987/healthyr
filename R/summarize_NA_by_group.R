
#' Summarize NA by Group
#'
#' Creates a summary table of the number of NA values in each dataframe column, by group.
#'
#' @param df Inputs a dataframe
#' @param grouping Inputs a character value of the column name by which to group the results.
#'
#' @return Returns a data frame of a summary table, with group values as columns, and dataframe columns as rows, and
#' cell values as the count of NA per column.
#' @export
#'
#' @examples
#' \dontrun{summarize_NA_by_group(df = mydata, grouping = "date_data_collection")}
summarize_NA_by_group <- function(df, grouping) {

  cols <- colnames(df)
  cols <- setdiff(cols, grouping)

  df <- df %>%
    rename(group = {{grouping}}) %>%
    gather(key = "column.name", value = "value", cols) %>%
    mutate(count = ifelse(is.na(value), 1, 0)) %>%
    group_by(group, column.name) %>%
    summarize(n = sum(count, na.rm = TRUE)) %>%
    spread(key = group, value = n)

  # df2 <-

  return(df)

}
