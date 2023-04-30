
#' Reformat Nutrition Health Indicators
#'
#' #' Function to reformat and standardize the values of columns for nutrition and health analysis. This function is intended to be a helper function
#' and to be called from within the format_nut_health_indicators function after column names and preliminary checks have been made. This
#' function will check the values for each relevant column and in some cases prompt the user for recoding inputs. This function covers anthropometric columns,
#' Washington Group Short Set indicators, infant and young child feeding indicators, food security outcome indicators, and a few other MSNA relevant health indicators.
#'
#' @param df Inputs a data frame directly from the format_nut_health_indicators function.
#' @param health_barriers Inputs a character vector of column names for self-reported health barriers.
#' @param lcs_variables Inputs a character vector of column names for livelihood coping strategies.
#' @param livelihood_variables Inputs a character vector of column names specifying all the livelihood activities
#' for which income is received. These should be numerical columns with estimated income per livelihood activity.
#' As per MSNA core indicator bank 2023.
#' @param value_map Inputs a value map which is generated from other heatlhyr functions to facilitate recoding.
#'
#' @return Returns a dataframe with standardized values for specific columns, that can then be used to calculate composite or other indicators.
#' @export
#'
#' @examples
#' \dontrun{reformat_nut_health_indicators(df = myhealthdata, health_barriers =
#' c("healthbarrier_colname_1", "healthbarrier_colname_2", ...), lcs_variables =
#' c("lcs_colname1", "lcs_colname2", ...))}
#'
#' @importFrom rlang .data
reformat_nut_health_indicators <- function(df,
                                           health_barriers = NULL,
                                           lcs_variables = NULL,
                                           livelihood_variables = NULL,
                                           value_map) {

  # reformatting sex

  if(c("sex") %in% colnames(df)) {

    sex_codes <- unique(df$sex)
    ideal_codes <- c("1", "2")
    sex_recodes <- c("1", "2", "NA")



    if(length(setdiff(sex_codes, ideal_codes))==0) {
      print("Good - Sex coded as 1/2 for male/female")
    } else {

      for(i in 1:length(sex_codes)) {
        a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : SEX \n How is '",sex_codes[[i]], "'? Please input either\n '1' for male, \n '2' for 'female' or \n 'NA' for missing. " )))
        while(length(setdiff(a, sex_recodes))==1) {
          a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : SEX \n Invalid input. ", "How is '", sex_codes[[i]], "' coded? Please input either\n '1' for male, \n '2' for 'female' or \n 'NA' for missing. ")))
        }

        df <- healthyr::recode_helper_data(df = df, column = "sex", old_val = sex_codes[[i]], new_val = a)
        value_map <- healthyr::recode_helper_map(df = value_map, column = "sex", old_val = sex_codes[[i]], new_val = a)

      }
    }

  }

  # reformatting date vars

  if(c("date_dc") %in% names(df)) {

    date_recodes <- c("mdy", "dmy", "ymd", "ydm")

    print(cat(paste0("\n RE-FORMATTING VARIABLE : DATE OF DATA COLLECTION \n")))

    unique_dates <- df %>% dplyr::filter(!is.na(.data$date_dc)) %>% dplyr::select(.data$date_dc) %>% t %>% c %>% unique

    print(paste0("Example of DATE OF DATA COLLECTION value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))

    a <- readline(cat(paste0("What is the date format for the DATE OF DATA COLLECTION column? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, date_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "How is DATE OF DATA COLLECTION formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console

    if(is.character(df$date_dc)) {df <- df %>% dplyr::mutate(date_dc = ifelse(.data$date_dc == "", NA, .data$date_dc))}

    df <- df %>%
      dplyr::mutate(date_dc_date = lubridate::as_date(lubridate::parse_date_time(.data$date_dc, orders = a))) %>%
      dplyr::mutate(date_dc_month = lubridate::month(.data$date_dc_date),
             date_dc_day = lubridate::day(.data$date_dc_date),
             date_dc_year = lubridate::year(.data$date_dc_date)) %>%
      dplyr::mutate(date_dc_char = paste(.data$date_dc_month, .data$date_dc_day, .data$date_dc_year, sep = "/"),
             date_dc_char = ifelse(is.na(.data$date_dc_char), NA, ifelse(.data$date_dc_char == "NA/NA/NA", NA, .data$date_dc_char)))

  }

  if(c("dob") %in% names(df)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")

    print(cat(paste0("\n RE-FORMATTING VARIABLE : DATE OF BIRTH \n")))

    unique_dates <- df %>% dplyr::filter(!is.na(.data$dob)) %>% dplyr::select(.data$dob) %>% t %>% c %>% unique

    print(paste0("Example of DATE OF DATA COLLECTION value: ", if(length(unique_dates)>0) {unique_dates[[1]]}, " ", if(length(unique_dates)>1) {unique_dates[[2]]}, " ",if(length(unique_dates)>2) {unique_dates[[3]]}))

    a <- readline(cat(paste0("What is the date format for the DATE OF BIRTH column? Please input : \n 'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month." )))
    while(length(setdiff(a, dob_recodes))==1) {
      a <- readline(cat(paste0("Invalid input. ", "How is DATE OF BIRTH formatted? Please input : \n  'mdy' for months-day-year, \n 'dmy' for day-months-year \n 'ymd' for year-month-day \n 'ydm' for year-day-month.")))
    }

    cat("\014") #clears the console

    if(is.character(df$dob)) {df <- df %>% dplyr::mutate(dob = ifelse(.data$dob == "", NA, .data$dob))}

    df <- df %>%
      dplyr::mutate(dob_date = lubridate::as_date(lubridate::parse_date_time(.data$dob, orders = a))) %>%
      dplyr::mutate(dob_month = lubridate::month(.data$dob_date),
             dob_day = lubridate::day(.data$dob_date),
             dob_year = lubridate::year(.data$dob_date)) %>%
      dplyr::mutate(dob_char = paste(.data$dob_month, .data$dob_day, .data$dob_year, sep = "/"),
             dob_char = ifelse(is.na(.data$dob_char), NA, ifelse(.data$dob_char == "NA/NA/NA", NA, .data$dob_char)))

  }

  # reformatting ages ####

  if(length(setdiff(c("dob", "date_dc"), colnames(df)))==0) {

    if(c("age_days") %in% colnames(df)) {
      df <- df %>%
        dplyr::mutate(age_days = ifelse(is.na(.data$date_dc), .data$age_days, .data$date_dc_date - .data$dob_date))

    } else {
      df <- df %>%
        dplyr::mutate(age_days = ifelse(is.na(.data$date_dc), NA, .data$date_dc_date - .data$dob_date))
    }

  }

  if(c("age_days") %in% names(df)) {

    if(!all(varhandle::check.numeric(df$age_days))) {stop("There are non-numeric values in the age_days variable. Please check your input.")} else {df <- df %>% dplyr::mutate(age_days = ifelse(is.na(.data$age_days), NA,  as.numeric(.data$age_days)))}

    if(c("age_months") %in% names(df)) {

      if(!all(varhandle::check.numeric(df$age_months))) {stop("There are non-numeric values in the age_months variable. Please check your input.")} else {df <- df %>% dplyr::mutate(age_months = ifelse(is.na(.data$age_months), NA,  as.numeric(.data$age_months)))}

      df <- df %>%
        dplyr::mutate(age_days = ifelse(is.na(.data$age_days), floor(.data$age_months*(365.25/12)), .data$age_days),
               age_months = ifelse(is.na(.data$age_months), floor(.data$age_days*(12/365.25)), .data$age_months))

    } else {

      df <- df %>%
        dplyr::mutate(age_months = ifelse(is.na(.data$age_days), NA, .data$age_days*(12/365.25)))

    }

  }

  if(c("age_months") %in% names(df)) {

    if(!all(varhandle::check.numeric(df$age_months))) {stop("There are non-numeric values in the age_months variable. Please check your input.")} else {df <- df %>% dplyr::mutate(age_months = ifelse(is.na(.data$age_months), NA,  as.numeric(.data$age_months)))}

    df <- df %>%
      dplyr::mutate(age_months = ifelse(is.na(.data$age_months), NA, ifelse(.data$age_months - floor(.data$age_months) >=0.96, as.numeric(ceiling(.data$age_months)), as.numeric(floor(.data$age_months)))))

    if(c("age_years") %in% names(df)) {

      df <- df %>%
        dplyr::mutate(age_years = ifelse(is.na(.data$age_months), .data$age_years, floor((.data$age_months/12))))

    } else {

      df <- df %>%
        dplyr::mutate(age_years = ifelse(is.na(.data$age_months), NA, floor((.data$age_months/12))))

    }

    if(c("age_days") %in% names(df)) {} else {
      df <- df %>% dplyr::mutate(age_days = ifelse(is.na(.data$age_months), NA, floor(.data$age_months*(365.25/12))))
    }

    if(c("age_proxy") %in% names(df)) {

      df <- df %>%
        dplyr::mutate(age_proxy = ifelse(is.na(.data$age_months), .data$age_proxy,
                                  ifelse(.data$age_months <6, "under_6months",
                                         ifelse(.data$age_months<24, "between_6to23months",
                                                ifelse(.data$age_months<60, "between_24to59months", "greater_59months")))))

    } else {

      df <- df %>%
        dplyr::mutate(age_proxy = ifelse(is.na(.data$age_months), NA,
                                  ifelse(.data$age_months <6, "under_6months",
                                         ifelse(.data$age_months<24, "between_6to23months",
                                                ifelse(.data$age_months<60, "between_24to59months", "greater_59months"))))
        )

    }



  }

  if(c("age_years") %in% names(df)) {

    if(!all(varhandle::check.numeric(df$age_years))) {stop("There are non-numeric values in the age_years variable. Please check your input.")} else {df <- df %>% dplyr::mutate(age_years = ifelse(is.na(.data$age_years), NA,  as.numeric(.data$age_years)))}

    if(c("age_months") %in% colnames(df)) {

    } else {
      df <- df %>% dplyr::mutate(age_months = ifelse(is.na(.data$age_years), NA, .data$age_years*12))
    }

    df$age_group <- cut(as.numeric(df$age_years),
                        breaks = c(-1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84, Inf),
                        labels = c("0-4", "5-9", "10-14", "15-19",
                                   "20-24", "25-29", "30-34", "35-39","40-44", "45-49", "50-54", "55-59",
                                   "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

  }

  # reformatting anthropometric indicators ####

  if(c("muac") %in% names(df)) {

    if(!all(varhandle::check.numeric(df$muac))) {stop("There are non-numeric values in the muac variable. Please check your input.")} else {df <- df %>% dplyr::mutate(muac = ifelse(is.na(.data$muac), NA,  as.numeric(.data$muac)))}

    if(mean(df$muac, na.rm = TRUE) > 50) {
      print(cat(paste0("\n MUAC is in millimeters Converting to centimeters \n \n \n ")))

      df <- df %>% dplyr::mutate(muac = ifelse(is.na(.data$muac), NA, .data$muac/10))
    } else {

      df <- df %>% dplyr::mutate(muac = ifelse(is.na(.data$muac), NA, .data$muac))

    }




  }
  if(c("weight") %in% names(df)) {

    if(!all(varhandle::check.numeric(df$weight))) {stop("There are non-numeric values in the weight variable. Please check your input.")} else {df <- df %>% dplyr::mutate(weight = ifelse(is.na(.data$weight), NA,  as.numeric(.data$weight)))}

    if(!(mean(df$weight, na.rm = TRUE) <100 & mean(df$weight, na.rm = TRUE)>2)) {stop("The average of your weight values is either <2kg or >100kg, which is likely. Please review the weight input.")}


  }
  if(c("height") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$height))) {stop("There are non-numeric values in the height variable. Please check your input.")} else {df <- df %>% dplyr::mutate(height = ifelse(is.na(.data$height), NA,  as.numeric(.data$height)))}
    if(mean(df$height, na.rm = TRUE)>50 & mean(df$height, na.rm = TRUE) <200) {
    } else {
      stop("The average of your length or height values is either <50 or >200, which is likely not in centimeters. Please review the lenght/height input. ")
    }

  }
  if(c("oedema") %in% names(df)) {

    df$oedema <- as.character(df$oedema)
    oedema_codes <- unique(df$oedema)
    ideal_oedema_codes <- c("y", "n")
    oedema_recodes <- c("y", "n", "NA")

    if(length(setdiff(oedema_codes, ideal_oedema_codes))==0) {
      print("Good - oedema is correctly coded as y/n.")
    } else {

      for(i in 1:length(oedema_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : OEDEMA \n")))
        d <- readline(cat(paste0("\n How is ", oedema_codes[[i]], " for oedema coded? Please choose from \n 'y' for yes oedema, \n 'n' for no oedema, or \n 'NA' for missing. \n")))
        while(length(setdiff(d, oedema_recodes))==1) {
          d <- readline(paste0("Invalid input. ", "How is '", oedema_codes[[i]], "' coded? Please choose from \n 'y' for yes oedema, \n 'n' for no oedema, or \n 'NA' for missing. \n"))
        }

        cat("\014") #clears the console

        if(!is.na(oedema_codes[[i]])){
          if(d == "NA") {df <- df %>% dplyr::mutate(oedema = ifelse(is.na(oedema_codes[[i]]), NA, ifelse(.data$oedema == oedema_codes[[i]], NA, .data$oedema)))
          } else {df <- df %>% dplyr::mutate(oedema = ifelse(is.na(.data$oedema), NA, ifelse(.data$oedema == oedema_codes[[i]], d, .data$oedema)))}
        }
      }



    }



  }

  if(c("oedema") %in% names(df)) {

    df <- df %>% dplyr::mutate(oedema = ifelse(is.na(.data$oedema), "n", .data$oedema))

    }



  # reformatting Washingtong Group Short Set indicators ####

  if(c("wgss_type_interview") %in% colnames(df)) {

    ideal_codes <- c("direct", "proxy")
    wgss_type_recodes <- c("direct", "proxy", "NA")

    df$wgss_type_interview <- as.character(df$wgss_type_interview)
    wgss_type_codes <- unique(df$wgss_type_interview)

    if(length(setdiff(wgss_type_codes, ideal_codes))==0) {
      print("Good - WGSS_type_interview is already coded properly.")
    } else {
      print((paste0("WGSS_type_interview is checking whether the WGSS questions were asked directly to the person, or through a proxy person who reported on their behalf. ")))
      print(cat(paste0("\n Please define how each value is coded for WGSS_type_interview in the data. \n")))

      for(i in 1:length(wgss_type_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : DIRECT or PROXY INTERVIEW for Washingtong Group Questions \n")))

        a <- readline(cat(paste0("How is '",wgss_type_codes[[i]], "' coded? Please input either \n 'direct' for direct interviews, \n 'proxy' for proxy interviews, \n 'dontknow' dont know or no responses, or \n 'NA' for missing." )))
        while(length(setdiff(a, wgss_type_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", wgss_type_codes[[i]], "' for WGSS_type_interview coded? Please input either 'direct' for direct interviews, 'proxy' for proxy interviews, 'dontknow' dont know or no responses, or 'NA' for missing." )))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", wgss_type_codes[[i]]))

        if(!is.na(wgss_type_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(wgss_type_interview = ifelse(.data$wgss_type_interview == wgss_type_codes[[i]], NA, .data$wgss_type_interview))
          } else {df <- df %>% dplyr::mutate(wgss_type_interview = ifelse(.data$wgss_type_interview == wgss_type_codes[[i]], a, .data$wgss_type_interview))}
        }
      }
    }
  }

  wgss_vars <- c("wgss1_seeing", "wgss2_hearing", "wgss3_walking", "wgss4_remembering", "wgss5_selfcare", "wgss6_communicating")

  if(length(setdiff(wgss_vars, names(df)))==0) {

    df[wgss_vars] <- lapply(df[wgss_vars], as.character)
    wgss_codes <- df %>% dplyr::select(wgss_vars) %>% t %>% c %>% unique
    ideal_codes <- c("1", "2", "3", "4", "9")
    wgss_recodes <- c("1", "2", "3", "4", "9", "NA")

    if(length(setdiff(wgss_codes, ideal_codes))==0) {
      print("Good - WGSS is already coded properly.")
    } else {
      print((paste0("WGSS asks if the person had no difficulty, some difficulty, a lot of difficulty, or cannot do at all with regards to 6 different domains.")))

      for(i in 1:length(wgss_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : Washington Group Short Set Questions \n")))

        a <- readline(cat(paste0("How is '",wgss_codes[[i]], "' coded? Please input either \n '1' for no dificulty, \n '2' for some difficulty, \n '3  for a lot of difficulty, \n '4' for cannot do at all, \n '9' for dont know or no response, or \n 'NA' for missing." )))
        while(length(setdiff(a, wgss_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", wgss_codes[[i]], "' for coded? Please input either \n '1' for no dificulty, \n '2' for some difficulty, \n '3  for a lot of difficulty, \n '4' for cannot do at all, \n '9' for dont know or no response, or \n 'NA' for missing." )))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", wgss_codes[[i]]))

        if(!is.na(wgss_codes[[i]])){

          if(a == "NA") {

            df <- df %>% dplyr::mutate(dplyr::across(wgss_vars, ~ (ifelse(. == wgss_codes[[i]], NA, .))))

          } else {

            df <- df %>% dplyr::mutate(dplyr::across(wgss_vars, ~(ifelse(. == wgss_codes[[i]], a, .))))

          }
        }
      }
    }

  }

  # reformatting inputs for IYCF 2021 indicators ####

  ebf_vars <- c("iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  swt_bv_vars_yn <- c("iycf_6c_swt", "iycf_6d_swt", "iycf_6h_swt", "iycf_6j_swt")
  bf_liquids_foods_yn <- c("iycf_1" ,"iycf_3", "iycf_4", "iycf_5", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                           "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r",
                           swt_bv_vars_yn)
  mdd_vars <- c("iycf_4", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o")
  mmf_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_8")
  mmff_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_7a_num")
  swt_bv_vars <- c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt")
  flesh_foods_vars <- c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m")
  unhealthy_food_vars <- c("iycf_7p", "iycf_7q")
  zero_veg_fruit_vars <- c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h")

  ### reformatting IYCF Caregiver ###

  if(c("iycf_caregiver") %in% colnames(df)) {

    caregiver_codes <- unique(df$iycf_caregiver)
    ideal_codes <- c("1", "0")
    caregiver_recodes <- c("1", "0", "NA")

    if(length(setdiff(caregiver_codes, ideal_codes))==0) {
      print("Good - IYCF Caregiver is coded as 1/0 for yes/no")
    } else {

      for(i in 1:length(caregiver_codes)) {
        a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : IYCF Caregiver \n How is '",caregiver_codes[[i]], "'? Please input either \n '1' for yes they were interviewed, \n '0' for not interviewed or \n 'NA' for missing. " )))
        while(length(setdiff(a, caregiver_recodes))==1) {
          a <- readline(cat(paste0("\n RE-FORMATTING VARIABLE : IYCF Caregiver \n Invalid input. ", "How is '", caregiver_codes[[i]], "' coded? Please input either \n '1' for yes they were interviewed, \n '0' for not interviewed or \n 'NA' for missing. ")))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", caregiver_codes[[i]]))

        if(!is.na(caregiver_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(iycf_caregiver = ifelse(.data$iycf_caregiver == caregiver_codes[[i]], NA, .data$iycf_caregiver))
          } else {df <- df %>% dplyr::mutate(iycf_caregiver = ifelse(.data$iycf_caregiver == caregiver_codes[[i]], a, .data$iycf_caregiver))}
        }

      }
    }

  }

  ### reformatting IYCF 2  ####

  if(c("iycf_2") %in% colnames(df)) {

    ideal_ei_codes <- c("1", "2", "3", "4", "9")
    iycf_ei_recodes <- c("1", "2", "3", "4", "9", "NA")
    iycf_ei_codes <- unique(df$iycf_2)

    if(length(setdiff(iycf_ei_codes, ideal_ei_codes))==0) {
      print("Good - IYCF_2 is coded as 1/2/3/4/9 for immediately/within first hour/within first day/after first day/dont know no response.")
    } else {
      print((paste0("IYCF_2 asks how soon after birth the child was put to the breast.")))
      for(i in 1:length(iycf_ei_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : IYCF 2 - Early Initiation of Breastfeeding \n")))

        a <- readline(cat(paste0("Is '",iycf_ei_codes[[i]], "' coded as immediately, within first hour, within first day, or after first day? Please input either \n '1' for immediately, \n '2' within the first hour, \n '3' within the first day, \n '4' for after the first day, \n '9' for no response or don't know, or \n 'NA' for missing. \n" )))
        while(length(setdiff(a, iycf_ei_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", iycf_ei_codes[[i]], "' for IYCF_2 coded? Please input either \n '1' for immediately, \n '2' within the first hour, \n '3' within the first day, \n '4' for after the first day, \n '9' for no response or don't know, or \n 'NA' for missing. \n" )))
        }

        cat("\014") #clears the console
        print(cat(paste("The input ", a, " is replacing", iycf_ei_codes[[i]], " \n ")))

        if(!is.na(iycf_ei_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(iycf_2 = ifelse(.data$iycf_2 == iycf_ei_codes[[i]], NA, .data$iycf_2))
          } else {df <- df %>% dplyr::mutate(iycf_2 = ifelse(.data$iycf_2 == iycf_ei_codes[[i]], a, .data$iycf_2))}
        }
      }
    }
  }

  ### reformatting IYCF 6b Num - Number of times infant formula ####

  if(c("iycf_6b_num") %in% colnames(df)) {

    if(!all(varhandle::check.numeric(df$iycf_6b_num))) {stop("There are non-numeric values in the iycf_6b_num variable. Please check your input.")} else {df <- df %>% dplyr::mutate(iycf_6b_num = ifelse(is.na(.data$iycf_6b_num), NA,  as.numeric(.data$iycf_6b_num)))}

  }

  ### reformatting IYCF 6c Num - Number of times milk ####

  if(c("iycf_6c_num") %in% colnames(df)) {
    if(!all(varhandle::check.numeric(df$iycf_6c_num))) {stop("There are non-numeric values in the iycf_6c_num variable. Please check your input.")} else {df <- df %>% dplyr::mutate(iycf_6c_num = ifelse(is.na(.data$iycf_6c_num), NA,  as.numeric(.data$iycf_6c_num)))}

  }

  ### reformatting IYCF 6d Num - Number of times milk ####

  if(c("iycf_6d_num") %in% colnames(df)) {
    if(!all(varhandle::check.numeric(df$iycf_6d_num))) {stop("There are non-numeric values in the iycf_6d_num variable. Please check your input.")} else {df <- df %>% dplyr::mutate(iycf_6d_num = ifelse(is.na(.data$iycf_6d_num), NA,  as.numeric(.data$iycf_6d_num)))}

  }

  ### reformatting IYCF 7a Num - Number times yoghurt (not drink) ####

  if(c("iycf_7a_num") %in% colnames(df)) {
    if(!all(varhandle::check.numeric(df$iycf_7a_num))) {stop("There are non-numeric values in the iycf_7a_num variable. Please check your input.")} else {df <- df %>% dplyr::mutate(iycf_7a_num = ifelse(is.na(.data$iycf_7a_num), NA,  as.numeric(.data$iycf_7a_num)))}

  }

  ### reformatting IYCF 8 - Number times ate solid, semi-solid or soft foods yesterday ####

  if(c("iycf_8") %in% colnames(df)) {
    if(!all(varhandle::check.numeric(df$iycf_8))) {stop("There are non-numeric values in the iycf_8 variable. Please check your input.")} else {df <- df %>% dplyr::mutate(iycf_8 = ifelse(is.na(.data$iycf_8), NA,  as.numeric(.data$iycf_8)))}

  }

  ### Checking IYCF Liquids and Foods ####

  list_to_check <- intersect(bf_liquids_foods_yn, colnames(df))

  if(length(list_to_check) > 0) {

    df[list_to_check] <- lapply(df[list_to_check], as.character)
    list_to_check_codes <- df %>% dplyr::select(list_to_check) %>% t %>% c %>% unique

    ideal_codes <- c("1", "2", "9")
    iycf_yn_recodes <- c("1", "2", "9", "NA")

    if(length(setdiff(list_to_check_codes, ideal_codes))==0) {
      print("Good - IYCF Liquid and Food variables are already coded as 1/2/9 for yes/no/don't know")
    } else {

      print(cat(paste0("\n RE-FORMATTING VARIABLE : IYCF Yes/No Questions \n")))

      print(cat(paste0("\n Please define how each value is coded for the IYCF Liquid and Food (Yes/No) questions in the data. \n")))
      for(i in 1:length(list_to_check_codes)) {
        a <- readline(cat(paste0("Is '",list_to_check_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either \n '1' for yes, \n '2' for no', \n '9' for no response/don't know, or \n 'NA' for missing." )))
        while(length(setdiff(a, iycf_yn_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", list_to_check_codes[[i]], "' for IYCF Liquids and Foods coded? Please input either \n '1' for yes, \n '2' for no', \n '9' for no response/don't know, or \n 'NA' for missing." )))
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

  }

  ### reformatting Household hunger scale questions ####

  hhs_vars <- c("hhs_nofoodhh_1", "hhs_nofoodhh_1a", "hhs_sleephungry_2", "hhs_sleephungry_2a", "hhs_alldaynight_3", "hhs_alldaynight_3a")
  hhs_vars_yn <- c("hhs_nofoodhh_1", "hhs_sleephungry_2", "hhs_alldaynight_3")
  hhs_vars_freq <- c("hhs_nofoodhh_1a", "hhs_sleephungry_2a", "hhs_alldaynight_3a")

  if(length(setdiff(hhs_vars_yn, names(df)))==0) {

    list_to_check_hhs_yn <- intersect(hhs_vars_yn, colnames(df))
    df[list_to_check_hhs_yn] <- lapply(df[list_to_check_hhs_yn], as.character)
    list_to_check_yn_codes <- df %>% dplyr::select(list_to_check_hhs_yn) %>% t %>% c %>% unique

    ideal_codes_yn <- c("1", "2", "9")
    yn_recodes <- c("1", "2", "9", "NA")

    if(length(setdiff(list_to_check_hhs_yn, ideal_codes_yn))==0) {
      print("Good - HHS Y/N vars are already coded as 1/2/9 for yes/no/don't know")
    } else {

      print(cat(paste0("\n Please define how each value is coded for the HHS Y/N questions in the data")))
      for(i in 1:length(list_to_check_yn_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : Household Hunger Scale yes/no questions \n")))
        a <- readline(cat(paste0("How is '",list_to_check_yn_codes[[i]], "' coded? Please input either \n '1' for yes, \n '2' for no', \n '9' for no response/don't know, or \n 'NA' for missing." )))
        while(length(setdiff(a, yn_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", list_to_check_yn_codes[[i]], "'? Please input either \n '1' for yes, \n '2' for no', \n '9' for no response/don't know, or \n 'NA' for missing." )))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", list_to_check_yn_codes[[i]]))

        if(!is.na(list_to_check_yn_codes[[i]])){

          if(a == "NA") {

            df <- df %>% dplyr::mutate(dplyr::across(list_to_check_hhs_yn, ~ (ifelse(. == list_to_check_yn_codes[[i]], NA, .))))

          } else {

            df <- df %>% dplyr::mutate(dplyr::across(list_to_check_hhs_yn, ~(ifelse(. == list_to_check_yn_codes[[i]], a, .))))

          }
        }
      }
    }
  }

  if(length(setdiff(hhs_vars_freq, names(df)))==0) {

    list_to_check_hhs_freq <- intersect(hhs_vars_freq, colnames(df))
    df[list_to_check_hhs_freq] <- lapply(df[list_to_check_hhs_freq], as.character)
    list_to_check_freq_codes <- df %>% dplyr::select(list_to_check_hhs_freq) %>% t %>% c %>% unique

    ideal_codes_freq <- c("1", "2", "3")
    freq_recodes <- c("1", "2", "3", "NA")

    if(length(setdiff(list_to_check_hhs_freq, ideal_codes_freq))==0) {
      print("Good - HHS Freq vars are already coded as 1/2/3 for Rarely, Sometimes, Often")
    } else {

      print(cat(paste0("\n Please define how each value is coded for the HHS Freq questions in the data")))
      for(i in 1:length(list_to_check_freq_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : Household Hunger Scale frequency questions \n")))
        a <- readline(cat(paste0("How is '",list_to_check_freq_codes[[i]], "' coded? Please input either \n '1' for Rarely (1-2 times), \n '2' for Sometimes (3-10 times), \n '3' for Often (>10 times), or \n 'NA' for missing." )))
        while(length(setdiff(a, freq_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", list_to_check_freq_codes[[i]], "'? Please input either \n '1' for Rarely (1-2 times), \n '2' for Sometimes (3-10 times), \n '3' for Often (>10 times), or \n 'NA' for missing." )))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", list_to_check_freq_codes[[i]]))

        if(!is.na(list_to_check_freq_codes[[i]])){

          if(a == "NA") {

            df <- df %>% dplyr::mutate(dplyr::across(list_to_check_hhs_freq, ~ (ifelse(. == list_to_check_freq_codes[[i]], NA, .))))

          } else {

            df <- df %>% dplyr::mutate(dplyr::across(list_to_check_hhs_freq, ~(ifelse(. == list_to_check_freq_codes[[i]], a, .))))

          }
        }
      }
    }

  }

  ### reformatting HDDS ####

  hdds_vars <- c("hdds_cereals", "hdds_tubers", "hdds_veg", "hdds_fruit", "hdds_meat", "hdds_eggs", "hdds_fish", "hdds_legumes",
                 "hdds_dairy", "hdds_oils", "hdds_sugars", "hdds_condiments")

  list_to_check <- intersect(hdds_vars, colnames(df))

  if(length(list_to_check) > 0) {

    list_to_check_hdds <- intersect(hdds_vars, colnames(df))
    df[list_to_check_hdds] <- lapply(df[list_to_check_hdds], as.character)
    list_to_check_hdds_codes <- df %>% dplyr::select(list_to_check_hdds) %>% t %>% c %>% unique

    ideal_codes <- c("1", "0")
    hdds_recodes <- c("1", "0", "NA")

    if(length(setdiff(list_to_check_hdds_codes, ideal_codes))==0) {
      print("Good - HDDS vars are already coded as 1/0 for yes consumed, or not consumed the previous day.")
    } else {

      print(cat(paste0("\n Please define how each value is coded for the HHS Freq questions in the data")))
      for(i in 1:length(list_to_check_hdds_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : Household Dietary Diversity Score (HDDS) \n")))
        a <- readline(cat(paste0("How is '",list_to_check_hdds_codes[[i]], "' coded? Please input either \n '1' for yes, consumed yesterday, \n '0' for no not consumed, or \n  'NA' for missing." )))
        while(length(setdiff(a, hdds_recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", list_to_check_hdds_codes[[i]], "'? Please input either \n '1' for yes, consumed yesterday, \n '0' for no not consumed, or \n  'NA' for missing." )))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", list_to_check_hdds_codes[[i]]))

        if(!is.na(list_to_check_hdds_codes[[i]])){

          if(a == "NA") {

            df <- df %>% dplyr::mutate(dplyr::across(list_to_check_hdds, ~ (ifelse(. == list_to_check_hdds_codes[[i]], NA, .))))

          } else {

            df <- df %>% dplyr::mutate(dplyr::across(list_to_check_hdds, ~(ifelse(. == list_to_check_hdds_codes[[i]], a, .))))

          }
        }
      }
    }





  }

  ### reformatting livelihoods ####

  if(!is.null(livelihood_variables)) {

    df <- df %>%
      dplyr::mutate(livelihood_salaried = "0",
                    livelihood_casual = "0",
                    livelihood_business = "0",
                    livelihood_own_prod = "0",
                    livelihood_govt_benefits = "0",
                    livelihood_rent = "0",
                    livelihood_remittances = "0",
                    livelihood_loans_family = "0",
                    livelihood_loans_community = "0",
                    livelihood_hum_assistance = "0",
                    livelihood_other = "0",
                    livelihood_crops = "0",
                    livelihood_livestock = "0")

    df[livelihood_variables] <- lapply(df[livelihood_variables], as.numeric)
    livelihood_recodes <- c("1", "2","3","4", "5", "6", "7", "8", "9", "10", "11", "12")

    for (z in 1:length(livelihood_variables)) {

      a <- readline(cat(paste0("Which livelihood category does <<",livelihood_variables[[z]], ">> correspond to? Please choose either:
                               \n '1' for Salaried
                               \n '2' for Casual Labour,
                               \n '3' for Business or Trade,
                               \n '4' for Own production (agriculture, livestock, fishing, food processing, home manufacture, etc.),
                               \n '5' for Government social benefits or assistance,
                               \n '6' for Income from Rent,
                               \n '7' for Remittances,
                               \n '8' for Loans or support from family and friends (not remittances),
                               \n '9' for Loans, support or charitable donations from from community members,
                               \n '10' for Humanitarian Assistance,
                               \n '11' for Other
                               \n '12' for Not Applicable.")))

      while(length(setdiff(a, livelihood_recodes))==1) {
        a <- readline(cat(paste0("Which livelihood category does <<",livelihood_variables[[z]], ">> correspond to? Please choose either:
                               \n '1' for Salaried
                               \n '2' for Casual Labour,
                               \n '3' for Business or Trade,
                               \n '4' for Own production (agriculture, livestock, fishing, food processing, home manufacture, etc.),
                               \n '5' for Government social benefits or assistance,
                               \n '6' for Income from Rent,
                               \n '7' for Remittances,
                               \n '8' for Loans or support from family and friends (not remittances),
                               \n '9' for Loans, support or charitable donations from from community members,
                               \n '10' for Humanitarian Assistance,
                               \n '11' for Other
                               \n '12' for Not Applicable.")))
      }

      if(a == "1") {df <- df %>% dplyr::mutate(livelihood_salaried = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_salaried))}
      if(a == "2") {df <- df %>% dplyr::mutate(livelihood_casual = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_casual))}
      if(a == "3") {df <- df %>% dplyr::mutate(livelihood_business = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_business))}
      # Own production coding, and additional coding for agricultural or livestock
      if(a == "4") {

        # Updates Livelihood Own Production
        df <- df %>% dplyr::mutate(livelihood_own_prod = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_own_prod))

        yn_recodes <- c("1", "2", "3")

        # Checks Additionally if it is agriculture related, and makes additional columns to use for data quality flags.
        k <- readline(cat(paste0("Is <<",livelihood_variables[[z]], ">> specific to agriculture or crops? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))

        while(length(setdiff(k, yn_recodes))==1) {
          k <- readline(cat(paste0("Invalid input, try again. Is <<",livelihood_variables[[z]], ">> specific to agriculture or crops? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))
        }

        if(k == "1") {df <- df %>% dplyr::mutate(livelihood_crops = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_crops))}

        # Checks Additionally if it is agriculture or livestock related, and makes additional columns to use for data quality flags.

        k <- readline(cat(paste0("Is <<",livelihood_variables[[z]], ">> specific to raising and selling livestock, or products from livestock (milk, meat, etc.)? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))

        while(length(setdiff(k, yn_recodes))==1) {
          k <- readline(cat(paste0("Invalid input, try again. Is <<",livelihood_variables[[z]], ">> specific to raising and selling livestock, or products from livestock (milk, meat, etc.)? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))
        }

        if(k == "1") {df <- df %>% dplyr::mutate(livelihood_livestock = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_livestock))}

        }
      if(a == "5") {df <- df %>% dplyr::mutate(livelihood_govt_benefits = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_govt_benefits))}
      if(a == "6") {df <- df %>% dplyr::mutate(livelihood_rent = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_rent))}
      if(a == "7") {df <- df %>% dplyr::mutate(livelihood_remittances = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_remittances))}
      if(a == "8") {df <- df %>% dplyr::mutate(livelihood_loans_family = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_loans_family))}
      if(a == "9") {df <- df %>% dplyr::mutate(livelihood_loans_community = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_loans_community))}
      if(a == "10") {df <- df %>% dplyr::mutate(livelihood_hum_assistance = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_hum_assistance))}
      if(a == "11") {df <- df %>% dplyr::mutate(livelihood_other = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_other))}
      # if(a == "12") {df <- df %>% dplyr::mutate(livelihood_salaried = ifelse(!!rlang::sym(livelihood_variables[[z]]) > 0 & !!rlang::sym(livelihood_variables[[z]]) != -999, "1", .data$livelihood_salaried))}

    }

  }

  ### reformatting LCS ####

  if(!is.null(lcs_variables)) {

    df <- df %>%
      dplyr::mutate(lcs_stress = "0",
             lcs_crisis = "0",
             lcs_emergency = "0",
             lcs_other = "0",
             lcs_stress_yes = "0",
             lcs_crisis_yes = "0",
             lcs_emergency_yes = "0",
             lcs_other_yes = "0",
             lcs_stress_exhaust = "0",
             lcs_crisis_exhaust = "0",
             lcs_emergency_exhaust = "0",
             lcs_other_exhaust = "0",
             lcs_livestock_yn = "0",
             lcs_agriculture_yn = "0"
      )

    # for recoding all barriers, assumes they are all coded the same way
    df[lcs_variables] <- lapply(df[lcs_variables], as.character)
    lcs_codes <- df %>% dplyr::select(lcs_variables) %>% t %>% c %>% unique
    ideal_codes <- c("1", "0")
    lcs_recodes <- c("1", "2", "3", "4", "NA")

    if(length(setdiff(lcs_codes, ideal_codes))==0) {
      print("Good - livelihood coping strategies are coded as 1/0 for yes/no")
    } else {
      for(i in 1:length(lcs_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLES : LIVELIHOOD COPING STRATEGIES \n")))
        a <- readline(cat(paste0("How is '",lcs_codes[[i]], "' coded? Please input either \n '1' for yes did in last 30 days, \n '2' for no didn't need to, \n '3' No couldnt because exhausted in last 12 months, \n '4' for Not Applicable, \n '9' for Don't know or no response, or \n 'NA' for missing." )))
        while(length(setdiff(a, lcs_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", lcs_codes[[i]], "' coded? Please input either \n '1' for yes did in last 30 days, \n '2' for no didn't need to, \n '3' No couldnt because exhausted in last 12 months, \n '4' for Not Applicable, \n '9' for Don't know or no response, or \n 'NA' for missing."))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", lcs_codes[[i]]))

        if(!is.na(lcs_codes[[i]])){
          if(a == "NA") {

            df <- df %>% dplyr::mutate(dplyr::across(lcs_variables, ~ (ifelse(. == lcs_codes[[i]], NA, .))))

          } else {
            df <- df %>% dplyr::mutate(dplyr::across(lcs_variables, ~(ifelse(. == lcs_codes[[i]], a, .))))

          }
        }

      }
    }

    lcs_severity_recodes <- c("1", "2", "3","9")
    # for assigning domains for each variable

    for (z in 1:length(lcs_variables)) {

      a <- readline(cat(paste0("Which severity of livelihood coping does <<",lcs_variables[[z]], ">> correspond to? Please choose either: \n '1' for Stress \n '2' for Crisis, \n '3' for Emergency, or \n '9' for Not Applicable." )))
      while(length(setdiff(a, lcs_severity_recodes))==1) {
        a <- readline(paste0("Invalid input. ", "Which severity of livelihood coping is <<", lcs_variables[[z]], ">>? Please choose either: \n '1' for Stress \n '2' for Crisis, \n '3' for Emergency, or \n '9' for Not Applicable."))
      }

      if(a == "1") {df <- df %>% dplyr::mutate(lcs_stress = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_stress))}
      if(a == "2") {df <- df %>% dplyr::mutate(lcs_crisis = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_crisis))}
      if(a == "3") {df <- df %>% dplyr::mutate(lcs_emergency = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_emergency))}
      if(a == "9") {df <- df %>% dplyr::mutate(lcs_other = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_other))}

      if(a == "1") {df <- df %>% dplyr::mutate(lcs_stress = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_stress))}
      if(a == "2") {df <- df %>% dplyr::mutate(lcs_crisis = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_crisis))}
      if(a == "3") {df <- df %>% dplyr::mutate(lcs_emergency = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_emergency))}
      if(a == "9") {df <- df %>% dplyr::mutate(lcs_other = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_other))}

      if(a == "1") {df <- df %>% dplyr::mutate(lcs_stress_yes = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_stress_yes))}
      if(a == "2") {df <- df %>% dplyr::mutate(lcs_crisis_yes = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_crisis_yes))}
      if(a == "3") {df <- df %>% dplyr::mutate(lcs_emergency_yes = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_emergency_yes))}
      if(a == "9") {df <- df %>% dplyr::mutate(lcs_other_yes = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_other_yes))}

      if(a == "1") {df <- df %>% dplyr::mutate(lcs_stress_exhaust = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_stress_exhaust))}
      if(a == "2") {df <- df %>% dplyr::mutate(lcs_crisis_exhaust = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_crisis_exhaust))}
      if(a == "3") {df <- df %>% dplyr::mutate(lcs_emergency_exhaust = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_emergency_exhaust))}
      if(a == "9") {df <- df %>% dplyr::mutate(lcs_other_exhaust = ifelse(!!rlang::sym(lcs_variables[[z]]) == "3", "1", .data$lcs_other_exhaust))}

      # Check if it is an agriculture or livestock dependent coping strategy
      # Selling more livestock than usual, selling last female cattle, etc.

      yn_recodes <- c("1", "2", "3")

      a <- readline(cat(paste0("Is the livelihood coping strategy <<",lcs_variables[[z]], ">> dependent on LIVESTOCK? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))

      while(length(setdiff(a, yn_recodes))==1) {
        a <- readline(cat(paste0("Invalid Input, try again. Is the livelihood coping strategy <<",lcs_variables[[z]], ">> dependent on LIVESTOCK? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))
      }
      if(a == "1") {df <- df %>% dplyr::mutate(lcs_livestock_yn = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_livestock_yn))}

      a <- readline(cat(paste0("Is the livelihood coping strategy <<",lcs_variables[[z]], ">> dependent on AGRICULTURE, FARMING, CROPS? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))

      # checking if agriculture dependent
      while(length(setdiff(a, yn_recodes))==1) {
        a <- readline(cat(paste0("Invalid Input, try again. Is the livelihood coping strategy <<",lcs_variables[[z]], ">> dependent on AGRICULTURE, FARMING, CROPS? Please choose either:
                               \n '1' for YES
                               \n '2' for NO
                               \n '3' for DONT KNOW")))
      }
      if(a == "1") {df <- df %>% dplyr::mutate(lcs_agriculture_yn = ifelse(!!rlang::sym(lcs_variables[[z]]) == "1", "1", .data$lcs_agriculture_yn))}

    }

  }

  # reformatting delivery_location ####

  if(c("delivery_location") %in% names(df)) {

    df <- df %>% dplyr::mutate(delivery_facility = "")

    health7_codes <- unique(df$delivery_location)
    ideal_codes <- c("1", "2", "9")
    health7_recodes <- c("1", "2", "9", "NA")

    if(length(setdiff(health7_codes, ideal_codes))==0) {
      print("Good - delivery_location is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("delivery_location asks yes/no if this specific woman 15-49 years of age delivered at an appropriate health facility.")))
      print(cat(paste0("\n Please define how each value is coded for delivery_location in the data")))
      for(i in 1:length(health7_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : PREGNANCY DELIVERY LOCATION \n")))
        a <- readline(paste0("Is '",health7_codes[[i]], "' coded as yes, no, or no response/don't know? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        while(length(setdiff(a, health7_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", health7_codes[[i]], "' for health7_pregnancy_ind_yn coded? Please input either '1' for yes, '2' for no', '9' for no response/don't know, or 'NA' for missing." ))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", health7_codes[[i]]))

        if(!is.na(health7_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(delivery_facility = ifelse(.data$delivery_location == health7_codes[[i]], NA, .data$delivery_facility))
          } else {df <- df %>% dplyr::mutate(delivery_facility = ifelse(.data$delivery_location == health7_codes[[i]], a, .data$delivery_facility))}
        }
      }
    }

  }

  # reformatting birth_assistant ####

  if(c("birth_assistant") %in% colnames(df)) {

    df <- df %>% dplyr::mutate(skilled_birth_assistant = "")

    health7_codes <- unique(df$birth_assistant)
    ideal_codes <- c("1", "2", "9")
    health7_recodes <- c("1", "2", "9", "NA")

    if(length(setdiff(health7_codes, ideal_codes))==0) {
      print("Good - birth_assistant is coded as 1/2/9 for yes/no/don't know")
    } else {
      print((paste0("birth_assistant asks who attended or assisted with the woman's most recent birth.")))
      print(cat(paste0("\n Please define how each value is coded for birth_assistant in the data.")))
      for(i in 1:length(health7_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : SKILLED BIRTH ATTENDANCE \n")))

        a <- readline(cat(paste0("How is '", health7_codes[[i]], "' coded? Please input either \n '1' for skilled birth attendant, \n '2' for any other person, \n '9' for no response/don't know, or \n 'NA' for missing." )))
        while(length(setdiff(a, health7_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", health7_codes[[i]], "' coded? Please input either \n '1' for skilled birth attendant, \n '2' for any other person, \n '9' for no response/don't know, or \n 'NA' for missing." ))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", health7_codes[[i]]))

        if(!is.na(health7_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(skilled_birth_assistant = ifelse(.data$birth_assistant == health7_codes[[i]], NA, .data$skilled_birth_assistant))
          } else {df <- df %>% dplyr::mutate(skilled_birth_assistant = ifelse(.data$birth_assistant == health7_codes[[i]], a, .data$skilled_birth_assistant))}
        }
      }
    }

  }

  # reformatting health barriers ####

  if(!is.null(health_barriers)) {

    df <- df %>%
      dplyr::mutate(health_barrier.none = 0,
             health_barrier.phys_access = 0,
             health_barrier.insecurity = 0,
             health_barrier.fin_access = 0,
             health_barrier.legal_access = 0,
             health_barrier.availability = 0,
             health_barrier.quality = 0,
             health_barrier.cultural = 0,
             health_barrier.other = 0,
             health_barrier.dknr = 0)

    # for recoding all barriers, assumes they are all coded the same way
    df[health_barriers] <- lapply(df[health_barriers], as.character)
    barrier_codes <- df %>% dplyr::select(health_barriers) %>% t %>% c %>% unique
    ideal_codes <- c("1", "0")
    barrier_recodes <- c("1", "0", "NA")

    if(length(setdiff(barrier_codes, ideal_codes))==0) {
      print("Good - health barriers are coded as 1/0 for yes/no")
    } else {
      print(cat(paste0("\n Please define how each value is coded for health barriers in the data. \n")))
      for(i in 1:length(barrier_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLES : HEALTH BARRIERS \n")))
        a <- readline(cat(paste0("How is '",barrier_codes[[i]], "' coded? Please input either \n '1' for yes, \n '0' for no, or \n 'NA' for missing. " )))
        while(length(setdiff(a, barrier_recodes))==1) {
          a <- readline(paste0("Invalid input. ", "How is '", barrier_codes[[i]], "' for health barriers coded? Please input either \n '1' for yes, \n '0' for no, or \n 'NA' for missing. "))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", barrier_codes[[i]]))

        if(!is.na(barrier_codes[[i]])){
          if(a == "NA") {

            df <- df %>% dplyr::mutate(dplyr::across(health_barriers, ~ (ifelse(. == barrier_codes[[i]], NA, .))))

          } else {
            df <- df %>% dplyr::mutate(dplyr::across(health_barriers, ~(ifelse(. == barrier_codes[[i]], a, .))))

          }
        }

      }
    }

    barrier_domain_recodes <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    # for assigning domains for each variable
    for (z in 1:length(health_barriers)) {

      a <- readline(cat(paste0("Which domain of health barriers does <<",health_barriers[[z]], ">> correspond to? Please choose either: \n '0' for no barriers \n '1' for Physical Accessibility, \n '2' for Insecurity, \n '3' for Financial Accessibility, \n '4' for Legal Access \n '5' for 'Availability', \n '6' for Quality, \n '7' for Cultural, \n '8' for Other, \n '9' for Don't know/No responses." )))
      while(length(setdiff(a, barrier_domain_recodes))==1) {
        a <- readline(paste0("Invalid input. ", "Which health barrier domain is <<", health_barriers[[z]], ">>? Please choose either: \n '0' for no barriers \n '1' for Physical Accessibility, \n '2' for Insecurity, \n '3' for Financial Accessibility, \n '4' for Legal Access \n '5' for 'Availability', \n '6' for Quality, \n '7' for Cultural, \n '8' for Other, \n '9' for Don't know/No responses."))
      }

      if(a == "0") {df <- df %>% dplyr::mutate(health_barrier.none = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.none))}
      if(a == "1") {df <- df %>% dplyr::mutate(health_barrier.phys_access = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.phys_access))}
      if(a == "2") {df <- df %>% dplyr::mutate(health_barrier.insecurity = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.insecurity))}
      if(a == "3") {df <- df %>% dplyr::mutate(health_barrier.fin_access = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.fin_access))}
      if(a == "4") {df <- df %>% dplyr::mutate(health_barrier.legal_access = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.legal_access))}
      if(a == "5") {df <- df %>% dplyr::mutate(health_barrier.availability = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.availability))}
      if(a == "6") {df <- df %>% dplyr::mutate(health_barrier.quality = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.quality))}
      if(a == "7") {df <- df %>% dplyr::mutate(health_barrier.cultural = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.cultural))}
      if(a == "8") {df <- df %>% dplyr::mutate(health_barrier.other = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.other))}
      if(a == "9") {df <- df %>% dplyr::mutate(health_barrier.dknr = ifelse(!!rlang::sym(health_barriers[[z]]) == "1", "1", .data$health_barrier.dknr))}
    }

  }

  # reformatting vaccination indicators

  if(c("penta") %in% colnames(df)) {

    penta_codes <- unique(df$penta)
    df$penta <- as.character(df$penta)
    ideal_codes <- c("1", "2", "3", "9")
    recodes <- c("1", "2", "3", "9", "NA")

    if(length(setdiff(penta_codes, ideal_codes))==0) {
      print("Good - penta coded as 1/2/3/9 already.")
    } else {
      print(cat(paste0("\n Please define how each value is coded for penta in the data, \n")))
      for(i in 1:length(penta_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : PENTAVALENT VACCINATION \n")))
        a <- readline(cat(paste0("How is '",penta_codes[[i]], "' coded? Please input either \n '1' yes by caregiver recall, \n '2' yes by vaccination card, \n '3' for not vaccinated, \n '9' for don't know or no response, or \n 'NA' for missing. \n" )))
        while(length(setdiff(a, recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", penta_codes[[i]], "' for penta coded? Please choose either \n '1' yes by caregiver recall, \n '2' yes by vaccination card, \n '3' for not vaccinated, \n '9' for don't know or no response, or \n 'NA' for missing. \n")))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", penta_codes[[i]]))

        if(!is.na(penta_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(penta = ifelse(.data$penta == penta_codes[[i]], NA, .data$penta))
          } else {df <- df %>% dplyr::mutate(penta = ifelse(.data$penta == penta_codes[[i]], a, .data$penta))}
        }

      }
    }

  }

  if(c("measles") %in% colnames(df)) {

    measles_codes <- unique(df$measles)
    df$measles <- as.character(df$measles)
    ideal_codes <- c("1", "2", "3", "9")
    recodes <- c("1", "2", "3", "9", "NA")

    if(length(setdiff(measles_codes, ideal_codes))==0) {
      print("Good - measles coded as 1/2/3/9 already.")
    } else {
      print(cat(paste0("\n Please define how each value is coded for measles in the data, \n")))
      for(i in 1:length(measles_codes)) {
        print(cat(paste0("\n RE-FORMATTING VARIABLE : MEASLES VACCINATION \n")))

        a <- readline(cat(paste0("How is '",measles_codes[[i]], "' coded? Please input either \n '1' yes by caregiver recall, \n '2' yes by vaccination card, \n '3' for not vaccinated, \n '9' for don't know or no response, or \n 'NA' for missing. \n" )))
        while(length(setdiff(a, recodes))==1) {
          a <- readline(cat(paste0("Invalid input. ", "How is '", measles_codes[[i]], "' for measles coded? Please choose either \n '1' yes by caregiver recall, \n '2' yes by vaccination card, \n '3' for not vaccinated, \n '9' for don't know or no response, or \n 'NA' for missing. \n")))
        }

        cat("\014") #clears the console
        print(paste("The input ", a, " is replacing", measles_codes[[i]]))

        if(!is.na(measles_codes[[i]])){
          if(a == "NA") {df <- df %>% dplyr::mutate(measles = ifelse(.data$measles == measles_codes[[i]], NA, .data$measles))
          } else {df <- df %>% dplyr::mutate(measles = ifelse(.data$measles == measles_codes[[i]], a, .data$measles))}
        }

      }
    }

  }


  return(list(df, value_map))

}
