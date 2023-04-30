

#' Select Non-Numeric Columns
#' Helper function to select only non-numeric columns in a dataset.
#' It will also exclude character columns that have only numeric values.#'
#'
#' @param df A dataframe object.
#'
#' @return Returns a dataframe object with only non-numeric columns selected.
#' @export
#'
#' @examples
select_non_numeric_cols <- function(df) {
#
  nms <- names(df)
  nms2 <- nms
  b <- c("", "test")

  for(i in 1:length(nms)) {

    a <- as.vector(df[[i]])

    if(!all(varhandle::check.numeric(v = as.character(a) ))) {

      b <- c(b, nms[[i]])

    }

  }

  c <- intersect(names(df), b)

  return(df[c])
}
