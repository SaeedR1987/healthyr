#' Select Non-Date Columns
#'
#' Helper function to select only non-Date columns. will test each column for
#' Date properties or formatting, and if present will remove that column.
#'
#' @param df A dataframe object.
#'
#' @return Returns a dataframe object without any date columns.
#' @export
#'
#' @examples
select_non_date_cols <- function(df) {

  nms <- names(df)
  b <- c("", "test")

  for(i in 1:length(nms)) {

    a <- as.vector(df[[i]])

    if(inherits(x = df[[i]], 'Date')) {
      # if class is Date, then column is added to b for later removal
      b <- c(b, nms[[i]])

    } else if(inherits(x = df[[i]], 'POSIXct')) {
      # if class is POSIXct, then column is added to b for later removal
      b <- c(b, nms[[i]])

    } else if(inherits(x = df[[i]], 'character')) {

      q <- sum(is.na(df[[i]]))

      r <- lubridate::parse_date_time(df[[i]], orders = c("ymd", "mdy", "dmy", "ydm", "dmY", "Ymd", "mdY", "Ydm"))

      if(length(df[[i]]) == sum(is.na(r))) {
        # if the number of NA values is equal to the length of the vector, it means no values parsed correctly and its just a normal character vector.

      } else if(q != sum(is.na(r))) {
        # if the number of NA values before parsing is different than the number of NA values after parsing, then NAs were introduced.
        stop(paste0(column(names("Column named ", df[[i]], " appears to have some Date values, but not all have parsed. Please check your input."))))
      }
      else if(sum(is.na(r)) == q) {
        # if the number of NAs is the same before and after parsing, then parsing dates happened correctly and column is added to b for later removal
        b <- c(b, nms[[i]])

      }

    }

  }

  c <- setdiff(names(df), b)

  return(df[c])

}

