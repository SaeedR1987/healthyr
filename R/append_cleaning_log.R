
#' Append Cleaning Log
#'
#' Function to read a new and old REACH style cleaning log, and will return a combined cleaning log of only unique issues.
#' It will preferentially keep cleaning log lines from the old, working cleaning log file, adding in any new issues from
#' the new cleaning log file.
#'
#' @param old_cl Inputs a dataframe of the current, working cleaning log file.
#' @param new_cl Inputs a dataframe of a new, automated cleaning log file, to be added to the working cleaning log.
#'
#' @return Returns a combined cleaning log file.
#' @export
#'
#' @examples
#' \dontrun{append_deletion_log(old_cl = myworkingcl, new_cl = mynewcl)}
append_cleaning_log <- function(old_cl, new_cl) {
#
  combined_cl <- rbind(new_cl, old_cl)
  # check for similar columns with uuid, question.name, issue, description, old.value
  cl <- combined_cl[ !duplicated(combined_cl[,c("uuid", "question.name", "issue", "old.value", "description")], fromLast=T)]

  return(cl)
}
