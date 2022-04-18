
#' SMART Survey Processed Mortality Dataset
#'
#' A dataset containing demographic and moratlity data from a SMART Survey that has been
#' merged and standardized by the healthyr functions.
#'
#' @format A data frame with 3780 rows and 47 variables:
#' \describe{
#'   \item{hh_id}{Unique household survey identifier}
#'   \item{date_dc}{Date of data collection, the original input into healthyr function.}
#'   \item{date_recall}{Date of recall event associated with mortality survey, the original input into healthyr function}
#'   \item{enum}{The survey team or enumerator ID}
#'   \item{admin2}{The administrative name of the county where the survey took place.}
#'   \item{cluster}{The cluster id}
#'   \item{sex}{Sex of the individual}
#'   \item{age_years}{Age of the individual in years}
#'   \item{join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{death_cause}{Categorical responses for causes of death.
#'   Typically categorized into illness/disease related deaths (1), injury/trauma deaths (2), or other causes (97).}
#'   \item{death_location}{Categorical variable for location of death. Typically categorized into
#'   died at current location (1), died during migration (2), died at previous residence (3), or other location (97).}
#'   \item{date_dc_date}{Date of data collection, as a date type variable}
#'   \item{date_dc_month}{Month of data collection}
#'   \item{date_dc_day}{Day of the month of data collection}
#'   \item{date_dc_year}{Year of data collection}
#'   \item{date_dc_char}{Date of data collection, as a character type variable}
#'   \item{date_recall_date}{Date of recall event, as a date type variable}
#'   \item{date_recall_month}{Month of recall event}
#'   \item{date_recall_day}{Day of the month of recall event}
#'   \item{date_recall_year}{Year of recall event}
#'   \item{person_time}{Number of days observed for individual, from the date of the recall event to the
#'   date of data collection.}
#'   \item{under_5}{Yes (1)/No(NA) if the individual is a child under 5 years of age}
#'   \item{under_5_pt}{Number of days observed for under 5 year old child, from the date of the recall
#'   event to the date of data collection.}
#'   \item{join_under5}{Yes (1) / No(NA) if the child under 5 joined the household since the start of the recall period.}
#'   \item{left_under5}{Yes (1) / No(NA) if the child under 5 left the household since the start of the recall period.}
#'   \item{birth_under5}{Yes (1) / No(NA) if the child under 5 was born into the household since the start of the recall period.}
#'   \item{death_under5}{Yes (1) / No(NA) if the  child under 5 died during the recall period}
#'   \item{age_0to2}{Yes (1) / No(NA) if the individual is less than 2 years of age.}
#'   \item{age_2to5}{Yes (1) / No(NA) if the individual is greater than or equal to 2 and less than 5 years of age.}
#'   \item{age_5to10}{Yes (1) / No(NA) if the individual is greater than or equal to 5 and less than 10 years of age.}
#'   \item{age_0to5}{Yes (1) / No(NA) if the individual is less than 5 years of age.}
#'   \item{age_5plus}{Yes (1) / No(NA) if the individual is greater than or equal to 5 years of age.}
#'   \item{age_group}{Categorical variable grouping individuals into 5 year age groupings.}
#'   \item{flag_births}{Yes (1) / No(0) Data quality flag if multiple births reported within one household.
#'   If flagged, will mark all household members for review.}
#'   \item{flag_deaths}{Yes (1) / No(0) Data quality flag if multiple deaths reported within one household.
#'   If flagged, will mark all household members for review.}
#'   \item{flag_birth}{Yes (1) / No(0) Data quality flag if a child was reported born since the recall event,
#'   but has an age of 1 completed year or greater. Usually recall periods are only about 3 months long.}
#'   \item{flag_join_left}{Yes (1) / No(0) Data quality flag if a person joined and left a household during the
#'   recall period.}
#'   \item{flag_missing_sex}{Yes (1) / No(0) Data quality flag if the sex of the individual is missing/NA.}
#'   \item{flag_missing_age_years}{Yes (1) / No(0) Data quality flag if the age_years of the individual is
#'   missing/NA.}
#'   \item{flag_missing_date_dc}{Yes (1) / No(0) Data quality flag if the date_dc is missing/NA.}
#'   \item{flag_missing_cluster}{Yes (1) / No(0) Data quality flag if the cluster is missing/NA.}
#'   \item{flag_missing_enum}{Yes (1) / No(0) Data quality flag if the enumerator or team id is missing/NA.}
#'   \item{flag_missing_death_cause}{Yes (1) / No(0) Data quality flag if the cause of death is missing/NA from a
#'   reported death.}
#'   \item{flag_missing_death_location}{Yes (1) / No(0) Data quality flag if the location of death is missing/NA
#'   from a reported death.}
#'
#' }
#'
#' @source SMART Survey August 2019
#'
#' @examples
#' data(proc_mortality1)
"proc_mortality1"
