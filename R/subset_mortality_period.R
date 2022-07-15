
#' Subset Mortality Period
#'
#' Intended to be used on a standardized mortality dataset which additionally captured dates of
#' different demographic events such as joins, lefts, births and deaths. It will dynamically subset the data
#' between a specified start and end of the recall period, and either include or exclude people, and calculate
#' their estimated person time observed.
#'
#' @param df Inputs a dataframe that has already been standardized by the format_current_census_mortality function.
#' @param start_date Inputs a character value of a date of the start of the recall period, in a format like YYYY-MM-DD.
#' @param end_date Inputs a character value of a date of the end of the recall period, in a format like YYYY-MM-DD.
#'
#' @return Returns a subsetted mortality dataset.
#' @export
#'
#' @examples
#' \dontrun{subset_mortality_period(df = mymortalitydata, start_date = "2020-04-24", end_date = "2021-05-15")}
subset_mortality_period <- function(df, start_date, end_date) {

  # check start and end date inputs

  if(is.null(start_date) | is.null(end_date)) {stop("You must include a start_date, and end_date for subsetting the mortality data. Please include in a YYYY-MM-DD format.")}

  start_date <- format(lubridate::parse_date_time(start_date, orders = "ymd", tz = ""), "%Y-%m-%d")
  end_date <- format(lubridate::parse_date_time(end_date, orders = "ymd", tz = ""), "%Y-%m-%d")

  # filter records

  if(c("date_join_date") %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(date_join_date = lubridate::as_date(lubridate::parse_date_time(.data$date_join_date, orders = "ymd", tz = ""))) %>%
      filter( is.na(date_join_date) | lubridate::as_date(.data$date_join_date) <= format(lubridate::parse_date_time(end_date, orders = "ymd", tz = ""), "%Y-%m-%d"))
  }
  if(c("date_birth_date") %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(date_birth_date = lubridate::as_date(lubridate::parse_date_time(.data$date_birth_date, orders = "ymd", tz = ""))) %>%
      filter( is.na(date_birth_date) | lubridate::as_date(.data$date_birth_date) <= format(lubridate::parse_date_time(end_date, orders = "ymd", tz = ""), "%Y-%m-%d"))
  }
  if(c("date_left_date") %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(date_left_date = lubridate::as_date(lubridate::parse_date_time(.data$date_left_date, orders = "ymd", tz = ""))) %>%
      filter( is.na(date_left_date) | lubridate::as_date(.data$date_left_date) > format(lubridate::parse_date_time(start_date, orders = "ymd", tz = ""), "%Y-%m-%d"))
  }
  if(c("date_death_date") %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(date_death_date = lubridate::as_date(lubridate::parse_date_time(.data$date_death_date, orders = "ymd", tz = ""))) %>%
      filter( is.na(date_death_date) | lubridate::as_date(.data$date_death_date) > format(lubridate::parse_date_time(start_date, orders = "ymd", tz = ""), "%Y-%m-%d"))
  }

  # Re-classify demographic events based on the new recall periods

  df <- df %>%
    dplyr::mutate(age_years = as.numeric(.data$age_years),
                  date_dc_date = lubridate::as_date(lubridate::parse_date_time(end_date, orders = "ymd", tz = "")),
                  date_recall_date = lubridate::as_date(lubridate::parse_date_time(start_date, orders = "ymd", tz = "")),

                  date_dc_date = as.Date(date_dc_date),
                  date_recall_date = as.Date(date_recall_date),
                  date_join_date = as.Date(date_join_date),
                  date_left_date = as.Date(date_left_date),
                  date_birth_date = as.Date(date_birth_date),
                  date_death_date = as.Date(date_death_date),

                  # date_dc_date2 = date_dc_date,
                  # date_recall_date2 = date_recall_date,
                  # date_join_date2 = date_join_date,
                  # date_left_date2 = date_left_date,
                  # date_birth_date2 = date_birth_date,
                  # date_death_date2 = date_death_date,

                  join = ifelse(.data$date_recall_date - .data$date_join_date > 0, NA, join),
                  left = ifelse(.data$date_left_date - .data$date_dc_date > 0, NA, left),
                  birth = ifelse(.data$date_recall_date - .data$date_birth_date > 0, NA, birth),
                  death = ifelse(.data$date_death_date - .data$date_dc_date > 0, NA, death),
                  # date_left_date = ifelse(is.na(.data$left), "", lubridate::as_date(lubridate::parse_date_time(date_left_date, orders = "ymd", tz = "")))
                  # test = .data$date_dc_date - .data$date_left_date,

                  # birth = ifelse(as.Date(.data$date_recall_date, origin = "1970-01-01") - as.Date(.data$date_birth_date, origin = "1970-01-01") < 0, NA, birth),
                  # death = ifelse(as.Date(.data$date_death_date, origin = "1970-01-01") - as.Date(.data$date_dc_date, origin = "1970-01-01") < 0, NA, death),

                  # diff_join = as.numeric(.data$date_dc_date - .data$date_join_date),
                  # remove_join = ifelse(diff_join < 0, 1, 0),
                  # date_join_date = ifelse(remove_join == 1, NA, as.Date(date_join_date, origin = "1970-01-01")),
                  # date_join_date = ifelse(as.Date(.data$date_dc_date, origin = "1970-01-01") - as.Date(.data$date_join_date, origin = "1970-01-01") < 0, NA, as.Date(.data$date_join_date, origin = "1970-01-01")),
                  # date_join_day = ifelse(.data$date_join_date2 < .data$date_recall_date2, NA, date_join_day),
                  # date_join_month = ifelse(.data$date_join_date2 < .data$date_recall_date2, NA, date_join_month),
                  # date_join_year = ifelse(.data$date_join_date2 < .data$date_recall_date2, NA, date_join_year)
                  )
  # %>%
    # dplyr::mutate(left = ifelse(.data$date_left_date2 > .data$date_dc_date2, NA, left),
    #               date_left_date = ifelse(.data$date_left_date2 > .data$date_dc_date2, NA, date_left_date),
    #               date_left_day = ifelse(.data$date_left_date2 > .data$date_dc_date2, NA, date_left_day),
    #               date_left_month = ifelse(.data$date_left_date2 > .data$date_dc_date2, NA, date_left_month),
    #               date_left_year = ifelse(.data$date_left_date2 > .data$date_dc_date2, NA, date_left_year)) %>%
    # dplyr::mutate(birth = ifelse(.data$date_birth_date2 < .data$date_recall_date2, NA, birth),
    #               date_birth_date = ifelse(.data$date_birth_date2 < .data$date_recall_date2, NA, date_birth_date),
    #               date_birth_day = ifelse(.data$date_birth_date2 < .data$date_recall_date2, NA, date_birth_day),
    #               date_birth_month = ifelse(.data$date_birth_date2 < .data$date_recall_date2, NA, date_birth_month),
    #               date_birth_year = ifelse(.data$date_birth_date2 < .data$date_recall_date2, NA, date_birth_year)) %>%
    # dplyr::mutate(death = ifelse(.data$date_death_date2 > .data$date_dc_date2, NA, death),
    #               date_death_date = ifelse(.data$date_death_date2 > .data$date_dc_date2, NA, date_death_date),
    #               date_death_day = ifelse(.data$date_death_date2 > .data$date_dc_date2, NA, date_death_day),
    #               date_death_month = ifelse(.data$date_death_date2 > .data$date_dc_date2, NA, date_death_month),
    #               date_death_year = ifelse(.data$date_death_date2 > .data$date_dc_date2, NA, date_death_year)
                  # )

  # recalculate person time with new start and end date

  df <- df %>%
    dplyr::mutate(date_join_date = lubridate::as_date(lubridate::parse_date_time(date_join_date, orders = "ymd", tz = "")),
                  date_left_date = lubridate::as_date(lubridate::parse_date_time(date_left_date, orders = "ymd", tz = "")),
                  date_birth_date = lubridate::as_date(lubridate::parse_date_time(date_birth_date, orders = "ymd", tz = "")),
                  date_death_date = lubridate::as_date(lubridate::parse_date_time(date_death_date, orders = "ymd", tz = "")),
                  under_5 = ifelse(is.na(.data$age_years), NA, ifelse(as.numeric(.data$age_years) < 5, 1, NA)),
                  under_5_pt = ifelse(is.na(.data$under_5), NA, ifelse(.data$under_5 == 1, .data$person_time, NA)))

  df <- df %>%
    dplyr::mutate(
      age_years = as.numeric(.data$age_years),


      # default person time calculations
      person_time = as.numeric(.data$date_dc_date - .data$date_recall_date),

      person_time = ifelse(is.na(.data$date_join_date), .data$person_time,
                           ifelse(!is.na(.data$date_death_date) & !is.na(death) & !is.na(join), as.numeric(.data$date_death_date - .data$date_join_date),
                                  ifelse(!is.na(.data$date_left_date) & !is.na(.data$left) & !is.na(join), as.numeric(.data$date_left_date - .data$date_join_date),
                                        ifelse(!is.na(join), as.numeric(.data$date_dc_date - .data$date_join_date), .data$person_time)))),

      # leaver person time calculations - join_left situaiton taken care above, so it defaults to person_time here
      person_time = ifelse(is.na(.data$date_left_date), .data$person_time,
                           ifelse(!is.na(.data$date_join_date) & !is.na(join), .data$person_time,
                                  ifelse(!is.na(.data$left), as.numeric(.data$date_left_date - .data$date_recall_date), .data$person_time))),

      # # birth person time calculations
      person_time = ifelse(is.na(.data$date_birth_date), .data$person_time,
                           ifelse( .data$date_birth_date < .data$date_recall_date, .data$person_time,
                                   ifelse(!is.na(.data$date_death_date)  & !is.na(death) & !is.na(birth), as.numeric(.data$date_death_date - .data$date_birth_date),
                                          ifelse(!is.na(.data$date_left_date) & !is.na(.data$left) & !is.na(birth), as.numeric(.data$date_left_date - .data$date_birth_date),
                                                ifelse(!is.na(birth), as.numeric(.data$date_dc_date - .data$date_birth_date), .data$person_time))))),
      #
      # # death person time calculations
      person_time = ifelse(is.na(.data$date_death_date), .data$person_time,
                           ifelse(!is.na(.data$date_join_date) & !is.na(join), .data$person_time,
                                  ifelse(!is.na(.data$date_birth_date) & !is.na(birth), .data$person_time,
                                        ifelse(!is.na(death), as.numeric(.data$date_death_date - .data$date_recall_date), .data$person_time))))  ,

      )


  print("date_recall_date, and date_dc_date columns have been modified to reflect the specified recall period.")


  return(df)

}
