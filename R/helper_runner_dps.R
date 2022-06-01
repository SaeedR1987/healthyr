

#' Helper Runner DPS
#'
#' Helper function for plot_date_runner function. Calculates digit preference
#' scores and feeds them into the runner::runner function.
#'
#' @param x A numeric vector
#'
#' @return A vector of digit preference scores
#' @export
#'
#' @examples
#' \dontrun{helper_runner_dps(muac)}
helper_runner_dps <- function(x) {

  x <- as.numeric(format(round(x, 1), nsmall = 1))

  vect <- nipnTK::digitPreference(x/10)[1]

  return(vect)


}
