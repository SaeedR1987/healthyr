
#' Select Non Other Columns
#'
#' Select all columns that do not end in "_other" or "_autre"
#'
#' @param df A dataframe object
#'
#' @return A dataframe object removing any columns with the suffixes "_other" or "_autre".
#' @export
#'
#' @examples
select_non_other_columns <- function(df) {

  nms <- names(df)
  nms2 <- nms
  b <- c("", "test")

  cols <- grep(pattern = "_other", x = colnames(df), value = TRUE)
  cols2 <- grep(pattern = "_autre", x = colnames(df), value = TRUE)

  cols <- c(cols, cols2)

  c <- setdiff(names(df), cols)

  return(df[c])

}
