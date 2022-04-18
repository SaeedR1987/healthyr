#' Flag Mortality Issues
#'
#' To flag individual households which may have unusual demographic and mortality reporting, for purposes of
#' data cleaning and quality monitoring.
#'
#' @param df Inputs a dataframe that has been formatted by the format_mortality_current_census function, which
#' is intended to reformat data collected using the current census method via the SMART methodology.
#'
#' @return Returns dataframe, the mortality dataset with additional columns flagging records with potential
#' data quality issues.
#' @export
#'
#' @examples
#' \dontrun{flag_mortality_issues(proc_mortality1)}
#'
#' @importFrom rlang .data
flag_mortality_issues <- function(df) {

  if(!(c("hh_id") %in% names(df))) {stop("Must have variable hh_id for flagging. Please check your input.")}
  # Unusual births, lefts, joins

  hh_summary <- df %>%
    dplyr::group_by(.data$hh_id) %>%
    dplyr::summarize(flag_births = ifelse(sum(!is.na(.data$birth))>1, 1, 0),
                     flag_deaths = ifelse(sum(!is.na(.data$death))>1, 1, 0)
                     # flag_prop_joins = ifelse((sum(!is.na(join)) / sum(!is.na(hh_id))) > 0.75, 1, 0),
                     # flag_prop_lefts = ifelse((sum(!is.na(left)) / sum(!is.na(hh_id))) > 0.75, 1, 0)
                     #
    )

  df <- merge(df, hh_summary, by = "hh_id", all.x = TRUE)

  df <- df %>%
    dplyr::mutate(flag_birth = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years > 0 & !is.na(.data$birth), 1, 0)),
           flag_join_left = ifelse(!is.na(.data$join) & !is.na(.data$left), 1, 0),
           flag_missing_sex = ifelse(is.na(.data$sex), 1, 0),
           flag_missing_age_years = ifelse(is.na(.data$age_years), 1, 0),
           flag_missing_date_dc = ifelse(is.na(.data$date_dc), 1, 0 ),
           flag_missing_cluster = ifelse(is.na(.data$cluster), 1, 0),
           flag_missing_enum = ifelse(is.na(.data$enum), 1, 0),
           flag_missing_death_cause = ifelse(!is.na(.data$death) & is.na(.data$death_cause), 1, 0),
           flag_missing_death_location = ifelse(!is.na(.data$death) & is.na(.data$death_location), 1, 0))

  return(df)

}
