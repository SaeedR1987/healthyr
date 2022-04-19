#' Format Mortality Current Census
#'
#' This function aims to reshape demographic and mortality data into a standard format. Is intended to be used with mortality
#' data that has been collected in-line with the SMART mortality module, which uses the current census method for mortality
#' data collection.
#'
#' @param df_roster Inputs a dataframe of the current household demographic roster.
#' @param file_path Optional input to include a file path to export an xlsx file of the formatted mortality data
#' @param date_dc_roster Inputs a character value specifying the column in df_roster for date of data collection
#' @param enum_roster Inputs a character value specifying the column in df_roster for enumerator or team id
#' @param cluster_roster Inputs a character value specifying the column in df_roster for cluster number or id
#' @param admin1_roster Inputs a character value specifying the column in df_roster for admin1
#' @param admin2_roster Inputs a character value specifying the column in df_roster for admin2
#' @param hh_id_roster Inputs a character value specifying the column in df_roster for unique household id. Can use
#' the uuid for this as well.
#' @param sex_roster Inputs a character value specifying the column in df_roster for sex of the individual
#' @param age_roster Inputs a character value specifying the column in df_roster for age in years of the individual
#' @param joined_roster Inputs a character value specifying the column in df_roster for if the individual joined since
#' the start of the recall period.
#' @param birth_roster Inputs a character value specifying the column in df_roster for if the individual was born into
#' the household since the start of the recall period.
#' @param df_left Inputs a dataframe of the roster of individuals who have left the household during the recall period.
#' @param date_dc_left Inputs a character value specifying the column in df_left for date of data collection
#' @param enum_left Inputs a character value specifying the column in df_left for enumerator or team id
#' @param cluster_left Inputs a character value specifying the column in df_left for cluster number or id
#' @param admin1_left Inputs a character value specifying the column in df_left for admin1
#' @param admin2_left Inputs a character value specifying the column in df_left for admin2
#' @param hh_id_left Inputs a character value specifying the column in df_left for unique household id. Can use
#' the uuid for this as well.
#' @param sex_left Inputs a character value specifying the column in df_left for sex of the individual
#' @param age_left Inputs a character value specifying the column in df_left for age in years of the individual
#' @param birth_left Inputs a character value specifying the column in df_left for if the individual was born into the household
#' since the start of the recall period.
#' @param joined_left Inputs a character value specifying the column in df_left for if the individual joined the household
#' since the start of the recall period.
#' @param df_died Inputs a dataframe of the roster of individuals who have died from the household during the recall period.
#' @param date_dc_died Inputs a character value specifying the column in df_died for date of data collection
#' @param enum_died Inputs a character value specifying the column in df_died for enumerator or team id
#' @param cluster_died Inputs a character value specifying the column in df_died for cluster number or id
#' @param admin1_died Inputs a character value specifying the column in df_died for admin1
#' @param admin2_died Inputs a character value specifying the column in df_died for admin2
#' @param hh_id_died Inputs a character value specifying the column in df_died for unique household id. Can use
#' the uuid for this as well.
#' @param sex_died Inputs a character value specifying the column in df_died for the sex of the deceased individual.
#' @param age_died Inputs a character value specifying the column in df_died for the age in years of deceased individual at time of death.
#' @param birth_died Inputs a character value specifying the column in df_died for if the deceased individual was born into the
#' household since the start of the recall period
#' @param joined_died Inputs a character value specifying the column in df_died for if the deceased individual joined the household
#' since the start of the recall period.
#' @param death_cause Inputs a character value specifying the column in df_died for the reported cause of death.
#' @param death_location Inputs a character value specifying the column in df_died for the reported location of death.
#' @param date_recall_event Inputs a character value with the date of the recall event. Person time observed will be determined based
#' on the difference in time between this date and the date of data collection. While this function will check the formatting, it is
#' recommended to input the date in a format such as "DD/MM/YYYY".
#'
#' @return Returns a single merged and standardized dataframe of the demographic and mortality data, in a long format where each
#' row is an individual, whether they are a current household member, left individual, or deceased individual.
#' @export
#'
#' @examples
#' \dontrun{ df_aweil_mortality <- format_mortality_current_census(
#' df_roster = raw_mortality_roster1, #' date_dc_roster = "today", enum_roster = "enum",
#' cluster_roster = "cluster_id", admin1_roster = "state", admin2_roster = "county",
#' hh_id_roster = "KEY", sex_roster = "sex_roster", age_roster = "age_years",
#' joined_roster = "joined", birth_roster = "birth", df_left = raw_mortality_left1,
#' date_dc_left = "today", enum_left = "enum", cluster_left = "cluster_id",
#' admin1_left = "state", admin2_left = "county", hh_id_left = "KEY", sex_left = "sex_left",
#' age_left = "age_left", birth_left = "birth_left", joined_left = "join_left",
#' df_died = raw_mortality_died1, date_dc_died = "today", enum_died = "enum",
#' cluster_died = "cluster_id", admin1_died = "state", admin2_died = "county",
#' hh_id_died = "KEY", sex_died = "sex_died", age_died = "age_died",
#' birth_died = "birth_died", joined_died = "join_died", death_cause = "death_cause",
#' death_location = "death_location", date_recall_event = "21/04/2019")}
#'
#' @importFrom rlang .data

format_mortality_current_census <- function(df_roster, file_path = NULL,  date_dc_roster, enum_roster, cluster_roster, admin1_roster = NULL, admin2_roster = NULL, hh_id_roster, sex_roster, age_roster, joined_roster, birth_roster,
                                            df_left, date_dc_left, enum_left, cluster_left, admin1_left = NULL, admin2_left = NULL, hh_id_left, sex_left, age_left, birth_left, joined_left,
                                            df_died, date_dc_died, enum_died, cluster_died, admin1_died = NULL, admin2_died = NULL, hh_id_died, sex_died, age_died, birth_died, joined_died, death_cause, death_location,
                                            date_recall_event) {

  if(!methods::hasArg(date_recall_event)) {stop("A date for recall event is required. Please input a character date with a format like dd/mm/yyyy. E.g 28/12/2020. Please check your input.")}

  df_roster <- df_roster %>%
    dplyr::rename(date_dc = {{date_dc_roster}},
           enum = {{enum_roster}},
           cluster = {{cluster_roster}},
           admin1 = {{admin1_roster}},
           admin2 = {{admin2_roster}},
           hh_id = {{hh_id_roster}},
           sex = {{sex_roster}},
           age_years = {{age_roster}},
           join = {{joined_roster}},
           birth = {{birth_roster}}) %>%
    dplyr::mutate(date_recall = date_recall_event)


  df_left <- df_left %>%
    dplyr::rename(date_dc = {{date_dc_left}},
           enum = {{enum_left}},
           cluster = {{cluster_left}},
           admin1 = {{admin1_left}},
           admin2 = {{admin2_left}},
           hh_id = {{hh_id_left}},
           sex = {{sex_left}},
           age_years = {{age_left}},
           join = {{joined_left}},
           birth = {{birth_left}}) %>%
    dplyr::mutate(date_recall = date_recall_event)


  df_died <- df_died %>%
    dplyr::rename(date_dc = {{date_dc_died}},
           enum = {{enum_died}},
           cluster = {{cluster_died}},
           admin1 = {{admin1_died}},
           admin2 = {{admin2_died}},
           hh_id = {{hh_id_died}},
           sex = {{sex_died}},
           age_years = {{age_died}},
           join = {{joined_died}},
           birth = {{birth_died}},
           death_cause = {{death_cause}},
           death_location = {{death_location}}) %>%
    dplyr::mutate(date_recall = date_recall_event)

  # If no cluster variable, make a blank column
  if(!(c("cluster") %in% names(df_roster))) {
    df_roster <- df_roster %>% dplyr::mutate(cluster = "")
  }
  if(!(c("cluster") %in% names(df_left))) {
    df_left <- df_left %>% dplyr::mutate(cluster = "")
  }
  if(!(c("cluster") %in% names(df_died))) {
    df_died <- df_died %>% dplyr::mutate(cluster = "")
  }

  req_roster <- c("date_dc", "enum", "cluster", "sex", "age_years", "birth")
  req_left <- c("sex", "age_years", "birth")
  req_died <- c("sex", "age_years", "birth", "death_cause", "death_location")
  additional_cols <- c("join", "left", "death", "death_cause", "death_location")

  if(length(setdiff(req_roster, colnames(df_roster)))==0) {print("Sex, Age and Births available for current roster.")} else {stop("Missing minimum information (SEX, AGE, Births) for current household roster. Please check input.")}
  if(length(setdiff(req_left, colnames(df_left)))==0) {print("Sex, Age and Births available for Left people.")} else {stop("Missing minimum information (SEX, AGE, Births) for left people roster. Please check input.")}
  if(length(setdiff(req_died, colnames(df_died)))==0) {print("Sex, Age, Births, Cause and Location of Death available for Deceased people.")} else {stop("Missing minimum information (SEX, AGE, Births, Cause of Death, Location of Death) for death roster. Please check input.")}

  # Adding missing columns to current roster data
  if(length(setdiff(additional_cols, colnames(df_roster)))>0) {

    cols_to_add <- setdiff(additional_cols, colnames(df_roster))

    if(length(setdiff(c("join"), cols_to_add))==0) {df_roster <- df_roster %>% dplyr::mutate(join = "")}
    if(length(setdiff(c("left"), cols_to_add))==0) {df_roster <- df_roster %>% dplyr::mutate(left = "")}
    if(length(setdiff(c("death"), cols_to_add))==0) {df_roster <- df_roster %>% dplyr::mutate(death = "")}
    if(length(setdiff(c("death_cause"), cols_to_add))==0) {df_roster <- df_roster %>% dplyr::mutate(death_cause = "")}
    if(length(setdiff(c("death_location"), cols_to_add))==0) {df_roster <- df_roster %>% dplyr::mutate(death_location = "")}

  }


  # Adding missing columns to left roster data
  if(length(setdiff(additional_cols, colnames(df_left)))>0) {

    cols_to_add <- setdiff(additional_cols, colnames(df_left))

    if(length(setdiff(c("join"), cols_to_add))==0) {df_left <- df_left %>% dplyr::mutate(join = "")}
    if(length(setdiff(c("left"), cols_to_add))==0) {df_left <- df_left %>% dplyr::mutate(left = "1")}
    if(length(setdiff(c("death"), cols_to_add))==0) {df_left <- df_left %>% dplyr::mutate(death = "")}
    if(length(setdiff(c("death_cause"), cols_to_add))==0) {df_left <- df_left %>% dplyr::mutate(death_cause = "")}
    if(length(setdiff(c("death_location"), cols_to_add))==0) {df_left <- df_left %>% dplyr::mutate(death_location = "")}

  }


  # Adding missing columns to died roster data
  if(length(setdiff(additional_cols, colnames(df_died)))>0) {

    cols_to_add <- setdiff(additional_cols, colnames(df_died))

    if(length(setdiff(c("join"), cols_to_add))==0) {df_died <- df_died %>% dplyr::mutate(join = "")}
    if(length(setdiff(c("left"), cols_to_add))==0) {df_died <- df_died %>% dplyr::mutate(left = "")}
    if(length(setdiff(c("death"), cols_to_add))==0) {df_died <- df_died %>% dplyr::mutate(death = "1")}

  }

  # adjusting col order if admin are included or not

  if(is.null(admin1_roster)) {

    if(is.null(admin2_roster)) {

      col_order <- c("date_dc", "date_recall", "enum", "cluster", "hh_id", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")
    } else {
      col_order <- c("date_dc", "date_recall", "enum", "admin2", "cluster", "hh_id", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")

    }

  } else {

    if(is.null(admin2_roster)) {
      col_order <- c("date_dc", "date_recall", "enum", "admin1", "cluster", "hh_id", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")

    } else {
      col_order <- c("date_dc", "date_recall", "enum", "admin1", "admin2", "cluster", "hh_id", "sex", "age_years", "join", "left", "birth", "death", "death_cause", "death_location")

    }

  }

  df_roster <- df_roster %>% dplyr::select(col_order) %>% dplyr::mutate(age_years = as.character(.data$age_years))
  df_left <- df_left %>% dplyr::select(col_order)%>% dplyr::mutate(age_years = as.character(.data$age_years))
  df_died <- df_died %>% dplyr::select(col_order)%>% dplyr::mutate(age_years = as.character(.data$age_years))

  df_roster <- lapply(df_roster, as.character)
  df_left <- lapply(df_left, as.character)
  df_died <- lapply(df_died, as.character)

  df_mortality <- dplyr::bind_rows(df_roster, df_left)
  df_mortality <- dplyr::bind_rows(df_mortality, df_died)

  df_mortality <- healthyr::reformat_mortality_current_census(df_mortality)

  df_mortality <- df_mortality %>%
    dplyr::mutate(
      age_years = as.numeric(.data$age_years),
      #death = ifelse(is.na(death), 0, 1),
      person_time = .data$date_dc_date - .data$date_recall_date,
      person_time = as.numeric(.data$person_time),
      person_time = ifelse(!is.na(.data$join) | !is.na(.data$left) | !is.na(.data$birth) | !is.na(.data$death), .data$person_time*0.5, .data$person_time),
      under_5 = ifelse(is.na(.data$age_years), NA, ifelse(as.numeric(.data$age_years) < 5, 1, NA)),
      under_5_pt = ifelse(is.na(.data$under_5), NA, ifelse(.data$under_5 == 1, .data$person_time, NA)),
      join_under5 = ifelse(is.na(.data$under_5), NA, .data$join),
      left_under5 = ifelse(is.na(.data$under_5), NA, .data$left),
      birth_under5 = ifelse(is.na(.data$under_5), NA, .data$birth),
      death_under5 = ifelse(is.na(.data$under_5), NA, .data$death),
      age_0to2 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years >= 0 & .data$age_years < 2, 1, NA)),
      age_2to5 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years >= 2 & .data$age_years < 5, 1, NA)),
      age_5to10 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years >= 5 & .data$age_years < 10, 1, NA)),
      age_0to5 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years >= 0 & .data$age_years < 5, 1, NA)),
      age_5plus = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years >= 5 & .data$age_years < 200, 1, NA)),
    )

  df_mortality$age_group <- cut(as.numeric(df_mortality$age_years),
                                breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84, Inf),
                                labels = c("0-4", "5-9", "10-14", "15-19",
                                           "20-24", "25-29", "30-34", "35-39","40-44", "45-49", "50-54", "55-59",
                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))

  df_mortality <- healthyr::flag_mortality_issues(df = df_mortality)

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df_mortality, file_path)}

  return(df_mortality)

}
