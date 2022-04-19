#' Flag Summary Table
#'
#' This function identifies all columns with 'flag' included in the name, and will summarize these flagged variables
#' by the grouping variable specified. This would commonly be used to assess data quality issues across data collection
#' teams, administrative areas, or clusters. Flag columns must be coded as 1s and 0s, with one indicating a data quality issue
#' for that record.
#'
#' @param df Inputs a dataframe that has been standardized with the format_nut_health_indicators function, and flagged
#' flagged with the flag_nut_health_issues function. It will also recognize flags from the flag_anthro_issues,
#' flag_mortality_issues, and flag_iycf_issues functions
#' @param grouping Inputs a character value identifying the column by which to summarize the number of flags reported.
#' For example, it may specify a column for team id, administrative area, or cluster. Leaving this blank will
#' assume no grouping and just summarize the results overall.
#'
#' @return Returns a dataframe, a summary table of the number of flags reported by grouping.
#' @export
#'
#' @examples
#' \dontrun{flag_summary_table(df = mydata, grouping = "enumerator")}
flag_summary_table <- function(df, grouping = NULL) {

  if(is.null(grouping)) {

    df1 <- df %>%
      dplyr::select(c(dplyr::contains('flag_'), dplyr::contains('_flag'))) %>%
      dplyr::summarise_all(sum, na.rm = TRUE)

    df1 <- as.data.frame(t(df1))
    colnames(df1) <- "all"

    return(df1)

  } else if(!is.null(grouping)) {

    df1 <- df %>%
      dplyr::select(c(grouping, dplyr::contains('flag_'), dplyr::contains('_flag'))) %>%
      dplyr::rename(group = {{grouping}}) %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise_all(sum, na.rm = TRUE)

    df1 <- as.data.frame(t(df1))
    colnames(df1) <- as.factor(df1[1,])
    df1 <- df1[-1,]

  }

  return(df1)

}
