% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flag_summary_table.R
\name{flag_summary_table}
\alias{flag_summary_table}
\title{Flag Summary Table}
\usage{
flag_summary_table(df, grouping = NULL)
}
\arguments{
\item{df}{Inputs a dataframe that has been standardized with the format_nut_health_indicators function, and flagged
flagged with the flag_nut_health_issues function. It will also recognize flags from the flag_anthro_issues,
flag_mortality_issues, and flag_iycf_issues functions}

\item{grouping}{Inputs a character value identifying the column by which to summarize the number of flags reported.
For example, it may specify a column for team id, administrative area, or cluster. Leaving this blank will
assume no grouping and just summarize the results overall.}
}
\value{
Returns a dataframe, a summary table of the number of flags reported by grouping.
}
\description{
This function identifies all columns with 'flag' included in the name, and will summarize these flagged variables
by the grouping variable specified. This would commonly be used to assess data quality issues across data collection
teams, administrative areas, or clusters. Flag columns must be coded as 1s and 0s, with one indicating a data quality issue
for that record.
}
\examples{
\dontrun{flag_summary_table(df = mydata, grouping = "enumerator")}
}
