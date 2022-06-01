

#' Check Which Analysis Variables
#'
#' A helper function to identify which standard indicators can be checked with
#' the dashboard based on the data frame input.
#'
#' @param df Inputs a dataframe of a standardized anthropometric, FSL, mortality or WG-SS dataset
#' @param anthro_grouping_var Inputs an optional character value specifying the grouping by which to make SMART flags
#'
#' @return Returns a vector with values specifying different technical indicators.
#' @export
#'
#' @examples
#' \dontrun{}
check_which_analysis_vars <- function(df, anthro_grouping_var = NULL) {

  # Set list of varnames to search for
  # vars_to_check <- c("")
  # check which varnames are in the dataset

  # list_analysis_vars <- intersect(vars_to_check, colnames(df))

  # return list of varnames that can be analyzed

  print(!exists("analysis_vars"))

  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    anthro_grouping_var <- "group"
  }

  # adding flags if they haven't been included
  if(c("wfhz") %in% colnames(df)) {
    if(c("wfhz_noflag") %in% colnames(df)) {
      if(!is.null(anthro_grouping_var)) {
        df <- flag_anthro_issues(df, grouping = anthro_grouping_var)
      } else {df <- flag_anthro_issues(df)}
    }

    if(!exists("analysis_vars")) {analysis_vars <- c("wfhz")} else {analysis_vars <- c(analysis_vars, "wfhz")}
  }
  if(c("hfaz") %in% colnames(df)) {
    if(c("hfaz_noflag") %in% colnames(df)) {
      if(!is.null(anthro_grouping_var)) {
        df <- flag_anthro_issues(df, grouping = anthro_grouping_var)
      } else {df <- flag_anthro_issues(df)}
    }

    if(!exists("analysis_vars")) {analysis_vars <- c("hfaz")} else {analysis_vars <- c(analysis_vars, "hfaz")}

  }
  if(c("wfaz") %in% colnames(df)) {
    if(c("wfaz_noflag") %in% colnames(df)) {
      if(!is.null(anthro_grouping_var)) {
        df <- flag_anthro_issues(df, grouping = anthro_grouping_var)
      } else {df <- flag_anthro_issues(df)}
    }

    if(!exists("analysis_vars")) {analysis_vars <- c("wfaz")} else {analysis_vars <- c(analysis_vars, "wfaz")}

  }
  if(c("mfaz") %in% colnames(df)) {
    if(c("mfaz_noflag") %in% colnames(df)) {
      if(!is.null(anthro_grouping_var)) {
        df <- flag_anthro_issues(df, grouping = anthro_grouping_var)
      } else {df <- flag_anthro_issues(df)}
    }
    if(!exists("analysis_vars")) {analysis_vars <- c("mfaz")} else {analysis_vars <- c(analysis_vars, "mfaz")}

  }
  if(c("muac") %in% colnames(df)) {
    if(c("muac_noflag") %in% colnames(df)) {
      if(!is.null(anthro_grouping_var)) {
        df <- flag_anthro_issues(df, grouping = anthro_grouping_var)
      } else {df <- flag_anthro_issues(df)}
    }

    if(!exists("analysis_vars")) {analysis_vars <- c("muac")} else {analysis_vars <- c(analysis_vars, "muac")}

  }
  if(length(setdiff(c("person_time", "death"), colnames(df))) == 0) {

    if(!exists("analysis_vars")) {analysis_vars <- c("mortality")} else {analysis_vars <- c(analysis_vars, "mortality")}

  }
  if( c("fcs_score") %in% colnames(df) | c("hhs_score") %in% colnames(df)  | c("rcsi_score") %in% colnames(df) ) {

    if(!exists("analysis_vars")) {analysis_vars <- c("fsl")} else {analysis_vars <- c(analysis_vars, "fsl")}

  }

  print(analysis_vars)


  return(analysis_vars)


}
