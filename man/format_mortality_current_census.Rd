% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_mortality_current_census.R
\name{format_mortality_current_census}
\alias{format_mortality_current_census}
\title{Format Mortality Current Census}
\usage{
format_mortality_current_census(
  df_roster,
  file_path = NULL,
  date_dc_roster,
  enum_roster,
  cluster_roster,
  admin1_roster = NULL,
  admin2_roster = NULL,
  hh_id_roster,
  sex_roster,
  age_roster,
  joined_roster,
  birth_roster,
  birthdate_roster = NULL,
  joined_date_roster = NULL,
  df_left,
  date_dc_left,
  enum_left,
  cluster_left,
  admin1_left = NULL,
  admin2_left = NULL,
  hh_id_left,
  sex_left,
  age_left,
  birth_left,
  joined_left,
  joined_date_left = NULL,
  left_date_left = NULL,
  birthdate_left = NULL,
  df_died,
  date_dc_died,
  enum_died,
  cluster_died,
  admin1_died = NULL,
  admin2_died = NULL,
  hh_id_died,
  sex_died,
  age_died,
  birth_died,
  joined_died,
  death_cause,
  death_location,
  date_death = NULL,
  joined_date_died = NULL,
  birthdate_died = NULL,
  date_recall_event
)
}
\arguments{
\item{df_roster}{Inputs a dataframe of the current household demographic roster.}

\item{file_path}{Optional input to include a file path to export an xlsx file of the formatted mortality data}

\item{date_dc_roster}{Inputs a character value specifying the column in df_roster for date of data collection}

\item{enum_roster}{Inputs a character value specifying the column in df_roster for enumerator or team id}

\item{cluster_roster}{Inputs a character value specifying the column in df_roster for cluster number or id}

\item{admin1_roster}{Inputs a character value specifying the column in df_roster for admin1}

\item{admin2_roster}{Inputs a character value specifying the column in df_roster for admin2}

\item{hh_id_roster}{Inputs a character value specifying the column in df_roster for unique household id. Can use
the uuid for this as well.}

\item{sex_roster}{Inputs a character value specifying the column in df_roster for sex of the individual}

\item{age_roster}{Inputs a character value specifying the column in df_roster for age in years of the individual}

\item{joined_roster}{Inputs a character value specifying the column in df_roster for if the individual joined since
the start of the recall period.}

\item{birth_roster}{Inputs a character value specifying the column in df_roster for if the individual was born into
the household since the start of the recall period.}

\item{birthdate_roster}{Optional input of a character value specifying date of birth of a current roster member}

\item{joined_date_roster}{Optional input of a character value specifying date a current roster member joined the household}

\item{df_left}{Inputs a dataframe of the roster of individuals who have left the household during the recall period.}

\item{date_dc_left}{Inputs a character value specifying the column in df_left for date of data collection}

\item{enum_left}{Inputs a character value specifying the column in df_left for enumerator or team id}

\item{cluster_left}{Inputs a character value specifying the column in df_left for cluster number or id}

\item{admin1_left}{Inputs a character value specifying the column in df_left for admin1}

\item{admin2_left}{Inputs a character value specifying the column in df_left for admin2}

\item{hh_id_left}{Inputs a character value specifying the column in df_left for unique household id. Can use
the uuid for this as well.}

\item{sex_left}{Inputs a character value specifying the column in df_left for sex of the individual}

\item{age_left}{Inputs a character value specifying the column in df_left for age in years of the individual}

\item{birth_left}{Inputs a character value specifying the column in df_left for if the individual was born into the household
since the start of the recall period.}

\item{joined_left}{Inputs a character value specifying the column in df_left for if the individual joined the household
since the start of the recall period.}

\item{joined_date_left}{Optional input of a character value specifying date a left household had joined the household}

\item{left_date_left}{Optional input of a character value specifying date a left household member had left the household}

\item{birthdate_left}{Optional input of a character value specifying date a left household member was born, if born after the recall event}

\item{df_died}{Inputs a dataframe of the roster of individuals who have died from the household during the recall period.}

\item{date_dc_died}{Inputs a character value specifying the column in df_died for date of data collection}

\item{enum_died}{Inputs a character value specifying the column in df_died for enumerator or team id}

\item{cluster_died}{Inputs a character value specifying the column in df_died for cluster number or id}

\item{admin1_died}{Inputs a character value specifying the column in df_died for admin1}

\item{admin2_died}{Inputs a character value specifying the column in df_died for admin2}

\item{hh_id_died}{Inputs a character value specifying the column in df_died for unique household id. Can use
the uuid for this as well.}

\item{sex_died}{Inputs a character value specifying the column in df_died for the sex of the deceased individual.}

\item{age_died}{Inputs a character value specifying the column in df_died for the age in years of deceased individual at time of death.}

\item{birth_died}{Inputs a character value specifying the column in df_died for if the deceased individual was born into the
household since the start of the recall period}

\item{joined_died}{Inputs a character value specifying the column in df_died for if the deceased individual joined the household
since the start of the recall period.}

\item{death_cause}{Inputs a character value specifying the column in df_died for the reported cause of death.}

\item{death_location}{Inputs a character value specifying the column in df_died for the reported location of death.}

\item{date_death}{Optional input of a character value specifying date of death of a deceased household member.}

\item{joined_date_died}{Optional input of a character value specifying date that a deceased household member had joined the household.}

\item{birthdate_died}{Optional input of a character value specifying date of birth of a deceased household member.}

\item{date_recall_event}{Inputs a character value with the date of the recall event. Person time observed will be determined based
on the difference in time between this date and the date of data collection. While this function will check the formatting, it is
recommended to input the date in a format such as "DD/MM/YYYY".}
}
\value{
Returns a single merged and standardized dataframe of the demographic and mortality data, in a long format where each
row is an individual, whether they are a current household member, left individual, or deceased individual.
}
\description{
This function aims to reshape demographic and mortality data into a standard format. Is intended to be used with mortality
data that has been collected in-line with the SMART mortality module, which uses the current census method for mortality
data collection.
}
\examples{
\dontrun{ df_aweil_mortality <- format_mortality_current_census(
df_roster = raw_mortality_roster1, #' date_dc_roster = "today", enum_roster = "enum",
cluster_roster = "cluster_id", admin1_roster = "state", admin2_roster = "county",
hh_id_roster = "KEY", sex_roster = "sex_roster", age_roster = "age_years",
joined_roster = "joined", birth_roster = "birth", df_left = raw_mortality_left1,
date_dc_left = "today", enum_left = "enum", cluster_left = "cluster_id",
admin1_left = "state", admin2_left = "county", hh_id_left = "KEY", sex_left = "sex_left",
age_left = "age_left", birth_left = "birth_left", joined_left = "join_left",
df_died = raw_mortality_died1, date_dc_died = "today", enum_died = "enum",
cluster_died = "cluster_id", admin1_died = "state", admin2_died = "county",
hh_id_died = "KEY", sex_died = "sex_died", age_died = "age_died",
birth_died = "birth_died", joined_died = "join_died", death_cause = "death_cause",
death_location = "death_location", date_recall_event = "21/04/2019")}

}
