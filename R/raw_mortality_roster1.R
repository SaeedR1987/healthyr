#' SMART Survey Current Household Roster Data
#'
#' A dataset containing current household roster data from a SMART Survey. Not processed by the healthyr functions.
#'
#' @format A data frame with 3608 rows and 11 variables:
#' \describe{
#'   \item{KEY}{Unique household survey identifier}
#'   \item{sex_roster}{Sex of the individual}
#'   \item{age_years}{Age of the individual in years}
#'   \item{joined}{Yes/No if the person joined the household after the recall event}
#'   \item{birth}{Yes/No if the child was born into the household after the recall event}
#'   \item{left_roster}{Yes/No if the individual left the household after the recall event}
#'   \item{died_roster}{Yes/No if the individual died since the recall event}
#'   \item{county}{The administrative name of the county where the survey took place}
#'   \item{today}{The date of data collection}
#'   \item{enum}{The survey team or enumerator ID}
#'   \item{cluster_id}{The cluster id}
#' }
#'
#' @source SMART Survey August 2019
#'
#' @examples
#' data(raw_mortality_roster1)
"raw_mortality_roster1"
