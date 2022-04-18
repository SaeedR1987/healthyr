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
