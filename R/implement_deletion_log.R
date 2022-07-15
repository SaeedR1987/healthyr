

#' Implement Deletion Log
#'
#' Will delete records from a dataframe according to a REACH style deletion log.
#'
#' @param df Inputs the dataframe with records to be deleted.
#' @param deletion_log Inputs the REACH style deletion log, indicating which records to be deleted. Must have a
#' 'uuid' named column.
#' @param uuid_col Inputs a character value specifying the column name of the dataframe with the unique identifier which matches the
#' deletion log uuid column.
#'
#' @return Returns a dataframe without the deleted records.
#' @export
#'
#' @examples
#' \dontrun{implement_deletion_log(df = mydata, deletion_log = my_deletion_log, uuid_col = "my_hh_id")}
implement_deletion_log <- function(df, deletion_log, uuid_col) {

  if(c("uuid") %in% colnames(deletion_log)) {} else {stop("You must have a column named 'uuid' in the deletion log specifying the record id to be deleted. ")}

  df <- df %>% dplyr::filter(!(!!rlang::sym(uuid_col) %in% deletion_log$uuid))

  return(df)

}
