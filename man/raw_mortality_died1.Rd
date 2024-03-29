% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raw_mortality_died1.R
\docType{data}
\name{raw_mortality_died1}
\alias{raw_mortality_died1}
\title{SMART Survey Household Deaths Data}
\format{
A data frame with 19 rows and 14 variables:
\describe{
\item{KEY}{Unique household survey identifier}
\item{sex_died}{Sex of the individual}
\item{age_died}{Age of the individual in years}
\item{join_died}{Yes/No if the person joined the household after the recall event}
\item{birth_died}{Yes/No if the child was born into the household after the recall event}
\item{died_left}{Yes/No if the individual left the household after the recall event}
\item{died_died}{Yes/No if the individual died since the recall event}
\item{death_cause}{Categorical responses for causes of death.
Typically categorized into illness/disease related deaths (1), injury/trauma deaths (2), or other causes (97).}
\item{death_location}{Categorical variable for location of death. Typically categorized into
died at current location (1), died during migration (2), died at previous residence (3), or other location (97).}
\item{county}{The administrative name of the county where the survey took place.}
\item{today}{The date of data collection}
\item{enum}{The survey team or enumerator ID}
\item{cluster_id}{The cluster id}
}
}
\source{
SMART Survey August 2019
}
\usage{
raw_mortality_died1
}
\description{
A dataset containing household deaths data from a SMART Survey. Not processed by the healthyr functions.
}
\examples{
data(raw_mortality_died1)
}
\keyword{datasets}
