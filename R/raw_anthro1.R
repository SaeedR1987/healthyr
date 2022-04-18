
#' SMART Survey Anthropometric Data - Unprocessed
#'
#' A dataset containing household anthropometric data for children under-5 years of age.
#' Not processed by the healthyr functions.
#'
#' @format A data frame with 651 rows and 16 variables:
#' \describe{
#'   \item{KEY}{Unique household survey identifier.}
#'   \item{child_sex}{Sex of the child}
#'   \item{child_present}{Yes (1)/No (0) if the child is present at the time of the household interview.}
#'   \item{dob_known}{Yes (1)/No (0) if the date of birth of the child is known.}
#'   \item{birthdate}{Date of birth of the child, if known}
#'   \item{age}{Age in months of the child, from dat of birth or estimate from a local events calendar.}
#'   \item{weight}{Weight in kilogram of the child}
#'   \item{measure}{If the child was measured by Length (1) or by Height (2)}
#'   \item{height}{The length or height of the child in cm.}
#'   \item{muac}{The mid-upper arm circumference of the child in cm.}
#'   \item{edema}{Yes (1)/No (0) if the child had bilateral pitting oedema.}
#'   \item{edema_confirm}{Yes (1)/No (0) if the team supervisor confirmed the bilateral putting oedema.
#'   This variable should be used for oedema in analysis.}
#'   \item{county}{The administrative name of the county where the survey took place.}
#'   \item{today}{The date of data collection.}
#'   \item{enum}{The survey team or enumerator ID.}
#'   \item{cluster_id}{The cluster id.}
#'
#' }
#'
#' @source SMART Survey August 2019
#'
#' @examples
#' data(raw_anthro1)
"raw_anthro1"
