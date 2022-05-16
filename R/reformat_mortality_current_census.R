#' Reformatting Mortality Data for Current Census Survey Method
#'
#' This function is intended as a helper function and to be called from within the format_mortality_current_census function.
#' It will check the formatting of each of the variables, and if needed prompt the user for recoding certain variables.
#'
#' @param df Inputs a dataframe where the column names have been standardized already from the format_mortality_current_census function.
#'
#' @return Returns a reformatted standardized long mortality dataset.
#' @export
#'
#' @examples
#' \dontrun{reformat_mortality_current_census(df)}
#'
#' @importFrom rlang .data
reformat_mortality_current_census <- function(df) {

  # reformatting sex variable

  if(c("sex") %in% colnames(df)) {

    sex_codes <- unique(df$sex)
    ideal_codes <- c("1", "2")
    sex_recodes <- c("1", "2", "NA")

    if(length(setdiff(sex_codes, ideal_codes))==0) {
      print("Good - Sex coded as 1/2 for male/female")
    } else {

      for(i in 1:length(sex_codes)) {
        a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : SEX \n How is '",sex_codes[[i]], "' coded? Please input either\n '1' for male, \n '2' for 'female' or \n 'NA' for missing. " )))
        while(length(setdiff(a, sex_recodes))==1) {
          a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : SEX \n Invalid input. ", "How is '", sex_codes[[i]], "' coded? Please input either\n '1' for male, \n '2' for 'female' or \n 'NA' for missing. ")))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", sex_codes[[i]]))

        if(!is.na(sex_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(sex = ifelse(.data$sex == sex_codes[[i]], NA, .data$sex))
          } else {df <- df %>% dplyr::mutate(sex = ifelse(.data$sex == sex_codes[[i]], a, .data$sex))}
        }

      }
    }

  }

  # Checking Date of Data Collection

  if(c("date_dc") %in% names(df)) {

    date_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_dc)) %>% dplyr::select(.data$date_dc) %>% t %>% c %>% unique

    print(paste0("Example of Date of Data collection values: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))
    a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n What is the date format for the DATE OF DATA COLLECTION column? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, date_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. \n ", "\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n How is DATE OF DATA COLLECTION formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console

    if(is.character(df$date_dc)) {df <- df %>% dplyr::mutate(date_dc = ifelse(.data$date_dc == "", NA, .data$date_dc))}

    df <- df %>%
      dplyr::mutate(date_dc_date = lubridate::parse_date_time(.data$date_dc, orders = a)) %>%
      dplyr::mutate(date_dc_month = lubridate::month(.data$date_dc_date),
             date_dc_day = lubridate::day(.data$date_dc_date),
             date_dc_year = lubridate::year(.data$date_dc_date)) %>%
      dplyr::mutate(date_dc_char = paste(.data$date_dc_month, .data$date_dc_day, .data$date_dc_year, sep = "/"),
             date_dc_char = ifelse(is.na(.data$date_dc_char), NA, ifelse(.data$date_dc_char == "NA/NA/NA", NA, .data$date_dc_char)))

  }

  # Checking Recall Event Date

  if(c("date_recall") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_recall)) %>% dplyr::select(.data$date_recall) %>% t %>% c %>% unique

    print(paste0("Example of RECALL DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))
    a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : RECALL DATE \n What is the date format for the RECALL DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, dob_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "\n RE-FORMATTING VARIABLE : RECALL DATE \n How is RECALL DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console
    if(is.character(df$date_recall)) {df <- df %>% dplyr::mutate(date_recall = ifelse(.data$date_recall == "", NA, .data$date_recall))}


    df <- df %>%
      dplyr::mutate(date_recall_date = lubridate::parse_date_time(.data$date_recall, orders = a)) %>%
      dplyr::mutate(date_recall_month = lubridate::month(.data$date_recall_date),
             date_recall_day = lubridate::day(.data$date_recall_date),
             date_recall_year = lubridate::year(.data$date_recall_date)) %>%
      dplyr::mutate(date_recall = paste(.data$date_recall_month, .data$date_recall_day, .data$date_recall_year, sep = "/"),
             date_recall = ifelse(is.na(.data$date_recall), NA, ifelse(.data$date_recall == "NA/NA/NA", NA, .data$date_recall)))

  }

  # Checking Date Join

  if(c("date_join") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_join)) %>% dplyr::select(.data$date_join) %>% t %>% c %>% unique

    print(paste0("Example of JOIN DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))
    a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : JOIN DATE \n What is the date format for the JOIN DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, dob_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "\n RE-FORMATTING VARIABLE : JOIN DATE \n How is JOIN DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console
    if(is.character(df$date_join)) {df <- df %>% dplyr::mutate(date_join = ifelse(.data$date_join == "", NA, .data$date_join))}


    df <- df %>%
      dplyr::mutate(date_join_date = lubridate::parse_date_time(.data$date_join, orders = a)) %>%
      dplyr::mutate(date_join_month = lubridate::month(.data$date_join_date),
                    date_join_day = lubridate::day(.data$date_join_date),
                    date_join_year = lubridate::year(.data$date_join_date)) %>%
      dplyr::mutate(date_join = paste(.data$date_join_month, .data$date_join_day, .data$date_join_year, sep = "/"),
                    date_join = ifelse(is.na(.data$date_join), NA, ifelse(.data$date_join == "NA/NA/NA", NA, .data$date_join)))

  }

  # Checking Date Left

  if(c("date_left") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_left)) %>% dplyr::select(.data$date_left) %>% t %>% c %>% unique

    print(paste0("Example of LEFT DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))
    a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : LEFT DATE \n What is the date format for the LEFT DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, dob_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "\n RE-FORMATTING VARIABLE : LEFT DATE \n How is LEFT DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console
    if(is.character(df$date_left)) {df <- df %>% dplyr::mutate(date_left = ifelse(.data$date_left == "", NA, .data$date_left))}


    df <- df %>%
      dplyr::mutate(date_left_date = lubridate::parse_date_time(.data$date_left, orders = a)) %>%
      dplyr::mutate(date_left_month = lubridate::month(.data$date_left_date),
                    date_left_day = lubridate::day(.data$date_left_date),
                    date_left_year = lubridate::year(.data$date_left_date)) %>%
      dplyr::mutate(date_left = paste(.data$date_left_month, .data$date_left_day, .data$date_left_year, sep = "/"),
                    date_left = ifelse(is.na(.data$date_left), NA, ifelse(.data$date_left == "NA/NA/NA", NA, .data$date_left)))

  }

  # Checking Date Birth

  if(c("date_birth") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_birth)) %>% dplyr::select(.data$date_birth) %>% t %>% c %>% unique

    print(paste0("Example of BIRTH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))
    a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : BIRTH DATE \n What is the date format for the BIRTH DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, dob_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "\n RE-FORMATTING VARIABLE : BIRTH DATE \n How is BIRTH DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console
    if(is.character(df$date_birth)) {df <- df %>% dplyr::mutate(date_birth = ifelse(.data$date_birth == "", NA, .data$date_birth))}


    df <- df %>%
      dplyr::mutate(date_birth_date = lubridate::parse_date_time(.data$date_birth, orders = a)) %>%
      dplyr::mutate(date_birth_month = lubridate::month(.data$date_birth_date),
                    date_birth_day = lubridate::day(.data$date_birth_date),
                    date_birth_year = lubridate::year(.data$date_birth_date)) %>%
      dplyr::mutate(date_birth = paste(.data$date_birth_month, .data$date_birth_day, .data$date_birth_year, sep = "/"),
                    date_birth = ifelse(is.na(.data$date_birth), NA, ifelse(.data$date_birth == "NA/NA/NA", NA, .data$date_birth)))

  }

  # Checking Date Death

  if(c("date_death") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_death)) %>% dplyr::select(.data$date_death) %>% t %>% c %>% unique

    print(paste0("Example of DEATH DATE value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))
    a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : DEATH DATE \n What is the date format for the DEATH DATE formatted? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, dob_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "\n RE-FORMATTING VARIABLE : DEATH DATE \n How is DEATH DATE formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console
    if(is.character(df$date_death)) {df <- df %>% dplyr::mutate(date_death = ifelse(.data$date_death == "", NA, .data$date_death))}


    df <- df %>%
      dplyr::mutate(date_death_date = lubridate::parse_date_time(.data$date_death, orders = a)) %>%
      dplyr::mutate(date_death_month = lubridate::month(.data$date_death_date),
                    date_death_day = lubridate::day(.data$date_death_date),
                    date_death_year = lubridate::year(.data$date_death_date)) %>%
      dplyr::mutate(date_death = paste(.data$date_death_month, .data$date_death_day, .data$date_death_year, sep = "/"),
                    date_death = ifelse(is.na(.data$date_death), NA, ifelse(.data$date_death == "NA/NA/NA", NA, .data$date_death)))

  }

  # Checking Joined, Left, Birth, Dead

  demographic_vars <- c("join", "left", "birth", "death")

  list_to_check <- intersect(demographic_vars, colnames(df))
  df[list_to_check] <- lapply(df[list_to_check], as.character)
  list_to_check_codes <- df %>% dplyr::select(list_to_check) %>% t %>% c %>% unique

  ideal_codes <- c("1")
  demo_recodes <- c("1", "NA")

  if(length(setdiff(list_to_check_codes, ideal_codes))==0) {
    print("Good - The demographic variables are already coded as 1/0 for yes/no")
  } else {

    print(cat(paste0("\n Please define how each value is coded for the demographic variable (Yes/No) columns in the data.")))
    for(i in 1:length(list_to_check_codes)) {
      a <- readline(cat(paste0("\n RE-FORMATTING VARIABLES : BIRTHS, JOINS, LEFTS, DEATHS \n How is '",list_to_check_codes[[i]], "' coded? Please input either \n '1' for yes, or \n 'NA' for no, missing or any other values." )))
      while(length(setdiff(a, demo_recodes))==1) {
        a <- readline(cat(paste0("Invalid input. \n ", "\n RE-FORMATTING VARIABLES : BIRTHS, JOINS, LEFTS, DEATHS \n How is '", list_to_check_codes[[i]], "' coded? Please input either \n '1' for yes, or \n 'NA' for no, missing or any other values." )))
      }
      cat("\014") #clears the console

      print(paste("The input ", a, " is replacing", list_to_check_codes[[i]]))

      if(!is.na(list_to_check_codes[[i]])){

        if(a == "NA") {

          df <- df %>% dplyr::mutate(dplyr::across(list_to_check, ~ (ifelse(. == list_to_check_codes[[i]], NA, .))))

        } else {

          df <- df %>% dplyr::mutate(dplyr::across(list_to_check, ~(ifelse(. == list_to_check_codes[[i]], a, .))))

        }
      }
    }
  }

  # Checking Cause of Death Coding

  cause_codes <- unique(df$death_cause)
  ideal_codes <- c("1", "2", "3")
  cause_recodes <- c("1", "2", "3", "NA")

  if(length(setdiff(cause_codes, ideal_codes))==0) {
    print("Good - Cause of Death coded as 1/2/3 for unknown/injury/illness")
  } else {
    print(cat(paste0("\n Please define how each value is coded for Cause of Death in the data,")))
    for(i in 1:length(cause_codes)) {
      a <- readline(cat(paste0("\n RE-FORMATTING VARIABLES : CAUSE OF DEATH \n How is '",cause_codes[[i]], "' coded? Please input either: \n '1' for unknown, \n '2' for injury/trauma, \n '3' for illness or \n 'NA' for missing. \n" )))
      while(length(setdiff(a, cause_recodes))==1) {
        a <- readline(paste0("Invalid input. \n ", "\n RE-FORMATTING VARIABLES : CAUSE OF DEATH \n How is '", cause_codes[[i]], "' coded? Please choose either: \n '1' for unknown, \n '2' for injury/trauma, \n '3' for illness or \n 'NA' for missing. \n"))
      }
      cat("\014") #clears the console

      print(paste("The input ", a, " is replacing", cause_codes[[i]]))

      if(!is.na(cause_codes[[i]])){
        if(a == "NA") {df <- df %>% dplyr::mutate(death_cause = ifelse(.data$death_cause == cause_codes[[i]], NA, .data$death_cause))
        } else {df <- df %>% dplyr::mutate(death_cause = ifelse(.data$death_cause == cause_codes[[i]], a, .data$death_cause))}
      }

    }
  }

  # Checking Location of Death Coding

  location_codes <- unique(df$death_location)
  ideal_codes <- c("1", "2", "3", "4")
  location_recodes <- c("1", "2", "3", "4", "NA")

  if(length(setdiff(location_codes, ideal_codes))==0) {
    print("Good - Cause of Death coded as 1/2/3/4 for unknown/injury/illness")
  } else {
    print(cat(paste0("\n Please define how each value is coded for Location of Death in the data,")))
    for(i in 1:length(location_codes)) {
      a <- readline(cat(paste0("\n RE-FORMATTING VARIABLES : LOCATION OF DEATH \n How is '",location_codes[[i]], "' coded? Please input either: \n '1' for Died in Current Location, \n '2' for Died During Migration, \n '3' for Died in place of last residence, \n '4' Other location of death or \n 'NA' for missing. \n" )))
      while(length(setdiff(a, location_recodes))==1) {
        a <- readline(cat(paste0("Invalid input. \n ", "\n RE-FORMATTING VARIABLES : LOCATION OF DEATH \n How is '", location_codes[[i]], "' coded? Please choose either: \n '1' for Died in Current Location, \n '2' for Died During Migration, \n '3' for Died in place of last residence, \n '4' Other location of death or \n 'NA' for missing. \n")))
      }
      cat("\014") #clears the console

      print(paste("The input ", a, " is replacing", location_codes[[i]]))

      if(!is.na(location_codes[[i]])){
        if(a == "NA") {df <- df %>% dplyr::mutate(death_location = ifelse(.data$death_location == location_codes[[i]], NA, .data$death_location))
        } else {df <- df %>% dplyr::mutate(death_location = ifelse(.data$death_location == location_codes[[i]], a, .data$death_location))}
      }

    }
  }

  return(df)

}
