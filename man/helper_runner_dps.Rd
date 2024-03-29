% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_runner_dps.R
\name{helper_runner_dps}
\alias{helper_runner_dps}
\title{Helper Runner DPS}
\usage{
helper_runner_dps(x)
}
\arguments{
\item{x}{A numeric vector}
}
\value{
A vector of digit preference scores
}
\description{
Helper function for plot_date_runner function. Calculates digit preference
scores and feeds them into the runner::runner function.
}
\examples{
\dontrun{helper_runner_dps(muac)}
}
