
#' SMART Survey Household Leavers Data
#'
#' A dataset containing household leavers data from a SMART Survey. Not processed by the healthyr functions.
#'
#' @format A data frame with 153 rows and 11 variables:
#' \describe{
#'   \item{KEY}{Unique household survey identifier}
#'   \item{sex_left}{Sex of the individual}
#'   \item{age_left}{Age of the individual in years}
#'   \item{join_left}{Yes/No if the person joined the household after the recall event}
#'   \item{birth_left}{Yes/No if the child was born into the household after the recall event}
#'   \item{left_left}{Yes/No if the individual left the household after the recall event}
#'   \item{died_left}{Yes/No if the individual died since the recall event}
#'   \item{county}{The administrative name of the county where the survey took place}
#'   \item{today}{The date of data collection}
#'   \item{enum}{The survey team or enumerator ID}
#'   \item{cluster_id}{The cluster id}
#' }
#'
#' @source SMART Survey August 2019
#'
#' @examples
#' data(raw_mortality_left1)
"raw_mortality_left1"
