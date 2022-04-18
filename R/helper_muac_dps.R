#' Helper MUAC DPS
#'
#' A helper function for returning MUAC digit preference scores within a
#' dplyr call.
#'
#' @param muac A vector of MUAC measurements.
#'
#' @return Returns a vector of digit preference scores.
#' @export
#'
#' @examples
#' \dontrun{helper_muac_dps(muac)}
#'
helper_muac_dps <- function(muac) {

  vect <- nipnTK::digitPreference(muac)[1]
  return(vect)
}
