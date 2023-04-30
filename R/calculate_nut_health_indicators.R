#' Calculate Nutrition Health Composite Indicators and Indices
#'
#' Function to create new columns for calculated nutrition and health indicators. This function is intended to be a helper function
#' and to be called from within the format_nut_health_indicators function after column names and values have been standardized. This
#' function will calculate anthropometric indicis, Washington Group Short Set indicators, infant and young child feeding indicators,
#' food security outcome indicators, and a few other MSNA relevant health indicators.
#'
#' @param df Inputs a data frame that has already been processed and checked by the format_nut_health_indicators and reformat_nut_health_indicators functions.
#' @param monthly_expenditures Inputs a character vector of column names for columns of various household expenses in the previous 30 days.
#' @param period_expenditures Inputs a character vector of column names for columns of various household expenses in a previous, recall period, specified in the num_period_months paramater.
#' @param num_period_months Inputs a whole integer for the number of months of the period expenditures reported.
#'
#' @return Returns a data frame with additional columns for new nutrition and health indicators.
#' @export
#'
#' @examples
#' \dontrun{calculate_nut_health_indicators(df)}
#'
#' #' @importFrom rlang .data
calculate_nut_health_indicators <- function(df, monthly_expenditures = NULL, period_expenditures = NULL, num_period_months = NULL) {

  # Calculating Anthropometric Indices

  wfh_vars <- c("weight", "height", "age_months")

  if(length(setdiff(wfh_vars, colnames(df)))==0) {

    age_codes <- c("1", "2")
    a <- readline(cat(paste0("Calculate Weight-For-Height z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months." )))
    while(length(setdiff(a, age_codes))==1) {
      a <- readline(cat(paste0("Invalid input. Calculate Weight-For-Height z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months.")))
    }

    if(a == "1") {min_age <- 6} else if(a == "2") {min_age <- 0}

    df <- df %>% dplyr::mutate(measured = ifelse(is.na(.data$age_months), 3, ifelse(.data$age_months <24, 2, ifelse(.data$age_months >= 24 & .data$age_months <60, 1, 3))))

    print("Calculating and adding weight-for-height z-scores (WHZ).")
    df <- zscorer::addWGSR(data = df,
                  sex = "sex",
                  firstPart = "weight",
                  secondPart = "height",
                  index = "wfh",
                  standing = "measured",
                  digits = 4)

    df <- df %>%
      dplyr::mutate(sam_wfhz = ifelse(is.na(.data$wfhz), NA, ifelse(is.na(.data$oedema), ifelse(.data$wfhz < -3, 1, 0), ifelse(.data$oedema == "y", 1, ifelse(.data$wfhz < -3, 1, 0)))) ,
             mam_wfhz = ifelse(is.na(.data$wfhz), NA, ifelse(.data$wfhz >= -3 & .data$wfhz < -2, 1, 0)),
             gam_wfhz = ifelse(is.na(.data$wfhz), NA, ifelse(is.na(.data$oedema), ifelse(.data$wfhz < -2, 1, 0), ifelse(.data$oedema == "y", 1, ifelse(.data$wfhz < -2, 1, 0)))) ,
             sam_wfhz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$sam_wfhz),
             mam_wfhz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$mam_wfhz),
             gam_wfhz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$gam_wfhz),
             wfhz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$wfhz))

  }

  hfa_vars <- c("height", "age_months")

  if(length(setdiff(hfa_vars, colnames(df)))==0) {

    age_codes <- c("1", "2")
    a <- readline(cat(paste0("Calculate Height-For-Age z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months." )))
    while(length(setdiff(a, age_codes))==1) {
      a <- readline(cat(paste0("Invalid input. Calculate Height-For-Age z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months.")))
    }

    if(a == "1") {min_age <- 6} else if(a == "2") {min_age <- 0}

    print("Calculating and adding height-for-age z-scores (HAZ).")
    df <- df %>% dplyr::mutate(age = .data$age_days)

    df <- zscorer::addWGSR(data = df,
                  sex = "sex",
                  firstPart = "height",
                  secondPart = "age",
                  index = "hfa")

    df <- df %>%
      dplyr::mutate(severe_stunting = ifelse(is.na(.data$hfaz), NA, ifelse(.data$hfaz < -3, 1, 0)),
             moderate_stunting = ifelse(is.na(.data$hfaz), NA, ifelse(.data$hfaz >= -3 & .data$hfaz < -2, 1, 0)),
             global_stunting = ifelse(is.na(.data$hfaz), NA, ifelse(.data$hfaz < -2, 1, 0)),
             severe_stunting = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$severe_stunting),
             moderate_stunting = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$moderate_stunting),
             global_stunting = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$global_stunting),
             hfaz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$hfaz))

  }

  wfa_vars <- c("weight", "age_months")

  if(length(setdiff(wfa_vars, colnames(df)))==0) {

    age_codes <- c("1", "2")
    a <- readline(cat(paste0("Calculate Weight-For-Age z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months." )))
    while(length(setdiff(a, age_codes))==1) {
      a <- readline(cat(paste0("Invalid input. Calculate Weight-For-Age z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months.")))
    }

    if(a == "1") {min_age <- 6} else if(a == "2") {min_age <- 0}

    print("Calculating and adding weight-for-age z-scores (WAZ).")
    df <- df %>% dplyr::mutate(age = .data$age_days)

    df <- zscorer::addWGSR(data = df,
                  sex = "sex",
                  firstPart = "weight",
                  secondPart = "age",
                  index = "wfa")

    df <- df %>%
      dplyr::mutate(severe_underweight = ifelse(is.na(.data$wfaz), NA, ifelse(.data$wfaz < -3, 1, 0)),
             moderate_underweight = ifelse(is.na(.data$wfaz), NA, ifelse(.data$wfaz >= -3 & .data$wfaz < -2, 1, 0)),
             global_underweight = ifelse(is.na(.data$wfaz), NA, ifelse(.data$wfaz < -2, 1, 0)),
             severe_underweight = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$severe_underweight),
             moderate_underweight = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$moderate_underweight),
             global_underweight = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$global_underweight))

  }

  index_vars <- c("muac", "age_days")

  if(length(setdiff(index_vars, colnames(df)))==0) {

    age_codes <- c("1", "2")
    a <- readline(cat(paste0("Calculate MUAC-For-Age z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months." )))
    while(length(setdiff(a, age_codes))==1) {
      a <- readline(cat(paste0("Invalid input. Calculate MUAC-For-Age z-scores for which age-group of children? Please input either: \n '1' for 6-59 months, or \n '2' for 0-59 months.")))
    }

    if(a == "1") {min_age <- 6} else if(a == "2") {min_age <- 0}

    print("Calculating and adding MUAC-for-age z-scores (MAZ).")
    df <- df %>% dplyr::mutate(age = .data$age_days)

    df <- zscorer::addWGSR(data = df,
                  sex = "sex",
                  firstPart = "muac",
                  secondPart = "age",
                  index = "mfa")

    df <- df %>%
      dplyr::mutate(severe_mfaz = ifelse(is.na(.data$mfaz), NA, ifelse(.data$mfaz < -3, 1, 0)),
             moderate_mfaz = ifelse(is.na(.data$mfaz), NA, ifelse(.data$mfaz >= -3 & .data$mfaz < -2, 1, 0)),
             global_mfaz = ifelse(is.na(.data$mfaz), NA, ifelse(.data$mfaz < -2, 1, 0)),
             severe_mfaz = ifelse(is.na(.data$oedema), .data$severe_mfaz, ifelse(.data$oedema == "y", 1, .data$severe_mfaz)),
             global_mfaz = ifelse(is.na(.data$oedema), .data$global_mfaz, ifelse(.data$oedema == "y", 1, .data$global_mfaz)),
             severe_mfaz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$severe_mfaz),
             moderate_mfaz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$moderate_mfaz),
             global_mfaz = ifelse(.data$age_months < min_age | .data$age_months >=60, NA, .data$global_mfaz))

  }

  index_vars <- c("muac")

  if(length(setdiff(index_vars, colnames(df)))==0) {

    if(c("oedema") %in% colnames(df)) {

      df <- df %>%
        dplyr::mutate(sam_muac = ifelse(is.na(.data$muac), NA, ifelse(.data$muac < 11.5, 1, 0)),
               mam_muac = ifelse(is.na(.data$muac), NA, ifelse(.data$muac >= 11.5 & .data$muac < 12.5, 1, 0)),
               gam_muac = ifelse(is.na(.data$muac), NA, ifelse(.data$muac < 12.5, 1, 0)),
               sam_muac = ifelse(is.na(.data$oedema), .data$sam_muac, ifelse(.data$oedema == "y", 1, .data$sam_muac)),
               gam_muac = ifelse(is.na(.data$oedema), .data$gam_muac, ifelse(.data$oedema == "y", 1, .data$gam_muac)),
               sam_muac = ifelse(.data$age_months < 6 | .data$age_months >=60, NA, .data$sam_muac),
               mam_muac = ifelse(.data$age_months < 6 | .data$age_months >=60, NA, .data$mam_muac),
               gam_muac = ifelse(.data$age_months < 6 | .data$age_months >=60, NA, .data$gam_muac))

    } else {
      df <- df %>%
        dplyr::mutate(sam_muac = ifelse(is.na(.data$muac), NA, ifelse(.data$muac < 11.5, 1, 0)),
               mam_muac = ifelse(is.na(.data$muac), NA, ifelse(.data$muac >= 11.5 & .data$muac < 12.5, 1, 0)),
               gam_muac = ifelse(is.na(.data$muac), NA, ifelse(.data$muac < 12.5, 1, 0)),
               sam_muac = ifelse(.data$age_months < 6 | .data$age_months >=60, NA, .data$sam_muac),
               mam_muac = ifelse(.data$age_months < 6 | .data$age_months >=60, NA, .data$mam_muac),
               gam_muac = ifelse(.data$age_months < 6 | .data$age_months >=60, NA, .data$gam_muac))
    }



  }

  if(c("age") %in% colnames(df)) {df <- df %>% dplyr::mutate(age = NULL)}

  # Calculating Washington Group Short Set Indicators

  wgss_vars <- c("wgss1_seeing", "wgss2_hearing", "wgss3_walking", "wgss4_remembering", "wgss5_selfcare", "wgss6_communicating")

  if(length(setdiff(wgss_vars, names(df)))==0) {

    df <- df %>%
      dplyr::mutate(wg_sum_seeing_234 = ifelse(is.na(.data$wgss1_seeing) , 0, ifelse(.data$wgss1_seeing == "2" | .data$wgss1_seeing == "3" | .data$wgss1_seeing == "4", 1, 0)),
             wg_sum_hearing_234 = ifelse(is.na(.data$wgss2_hearing) , 0, ifelse(.data$wgss2_hearing == "2" | .data$wgss2_hearing == "3" | .data$wgss2_hearing == "4", 1, 0)),
             wg_sum_communication_234 = ifelse(is.na(.data$wgss6_communicating) , 0, ifelse(.data$wgss6_communicating == "2" | .data$wgss6_communicating == "3" | .data$wgss6_communicating == "4", 1, 0)),
             wg_sum_walking_234 =  ifelse(is.na(.data$wgss3_walking) , 0, ifelse(.data$wgss3_walking == "2" | .data$wgss3_walking == "3" | .data$wgss3_walking == "4", 1, 0)),
             wg_sum_selfcare_234 = ifelse(is.na(.data$wgss5_selfcare) , 0, ifelse(.data$wgss5_selfcare == "2" | .data$wgss5_selfcare == "3" | .data$wgss5_selfcare == "4", 1, 0)),
             wg_sum_remembering_234 = ifelse(is.na(.data$wgss4_remembering) , 0, ifelse(.data$wgss4_remembering == "2" | .data$wgss4_remembering == "3" | .data$wgss4_remembering == "4", 1, 0)),
             wg_sum_seeing_34 = ifelse(is.na(.data$wgss1_seeing) , 0, ifelse(.data$wgss1_seeing == "3" | .data$wgss1_seeing == "4", 1, 0)),
             wg_sum_hearing_34 = ifelse(is.na(.data$wgss2_hearing) , 0, ifelse(.data$wgss2_hearing == "3" | .data$wgss2_hearing == "4", 1, 0)),
             wg_sum_communication_34 = ifelse(is.na(.data$wgss6_communicating) , 0, ifelse(.data$wgss6_communicating == "3" | .data$wgss6_communicating == "4", 1, 0)),
             wg_sum_walking_34 =  ifelse(is.na(.data$wgss3_walking) , 0, ifelse(.data$wgss3_walking == "3" | .data$wgss3_walking == "4", 1, 0)),
             wg_sum_selfcare_34 = ifelse(is.na(.data$wgss5_selfcare) , 0, ifelse(.data$wgss5_selfcare == "3" | .data$wgss5_selfcare == "4", 1, 0)),
             wg_sum_remembering_34 = ifelse(is.na(.data$wgss4_remembering) , 0, ifelse(.data$wgss4_remembering == "3" | .data$wgss4_remembering == "4", 1, 0)),
             wg_sum_seeing_4 = ifelse(is.na(.data$wgss1_seeing) , 0, ifelse(.data$wgss1_seeing == "4", 1, 0)),
             wg_sum_hearing_4 = ifelse(is.na(.data$wgss2_hearing) , 0, ifelse(.data$wgss2_hearing == "4", 1, 0)),
             wg_sum_communication_4 = ifelse(is.na(.data$wgss6_communicating) , 0, ifelse(.data$wgss6_communicating == "4", 1, 0)),
             wg_sum_walking_4 =  ifelse(is.na(.data$wgss3_walking) , 0, ifelse(.data$wgss3_walking == "4", 1, 0)),
             wg_sum_selfcare_4 = ifelse(is.na(.data$wgss5_selfcare) , 0, ifelse(.data$wgss5_selfcare == "4", 1, 0)),
             wg_sum_remembering_4 = ifelse(is.na(.data$wgss4_remembering) , 0, ifelse(.data$wgss4_remembering == "4", 1, 0)),
             wg_sco_score_seeing = ifelse(is.na(.data$wgss1_seeing), NA, ifelse(.data$wgss1_seeing == "2", 1, ifelse(.data$wgss1_seeing == "3", 6, ifelse(.data$wgss1_seeing == "4", 36, 0)))),
             wg_sco_score_hearing = ifelse(is.na(.data$wgss2_hearing), NA, ifelse(.data$wgss2_hearing == "2", 1, ifelse(.data$wgss2_hearing == "3", 6, ifelse(.data$wgss2_hearing == "4", 36, 0)))),
             wg_sco_score_communication = ifelse(is.na(.data$wgss6_communicating), NA, ifelse(.data$wgss6_communicating == "2", 1, ifelse(.data$wgss6_communicating == "3", 6, ifelse(.data$wgss6_communicating == "4", 36, 0)))),
             wg_sco_score_walking = ifelse(is.na(.data$wgss3_walking), NA, ifelse(.data$wgss3_walking == "2", 1, ifelse(.data$wgss3_walking == "3", 6, ifelse(.data$wgss3_walking == "4", 36,0)))),
             wg_sco_score_selfcare = ifelse(is.na(.data$wgss5_selfcare), NA, ifelse(.data$wgss5_selfcare == "2", 1, ifelse(.data$wgss5_selfcare == "3", 6, ifelse(.data$wgss5_selfcare == "4", 36,0)))),
             wg_sco_score_remembering = ifelse(is.na(.data$wgss4_remembering), NA, ifelse(.data$wgss4_remembering == "2", 1, ifelse(.data$wgss4_remembering == "3", 6, ifelse(.data$wgss4_remembering == "4", 36, 0)))),
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(wgss_sco_score = sum(c(.data$wg_sco_score_seeing, .data$wg_sco_score_hearing, .data$wg_sco_score_communication, .data$wg_sco_score_walking, .data$wg_sco_score_selfcare, .data$wg_sco_score_remembering), na.rm = TRUE),
                    wgss_sco_score = ifelse(.data$age_years < 5, NA, .data$wgss_sco_score),
             wg_sum_234 = sum(c(.data$wg_sum_seeing_234, .data$wg_sum_hearing_234, .data$wg_sum_communication_234, .data$wg_sum_walking_234, .data$wg_sum_selfcare_234, .data$wg_sum_remembering_234), na.rm = TRUE),
             wg_sum_34 = sum(c(.data$wg_sum_seeing_34, .data$wg_sum_hearing_34, .data$wg_sum_communication_34, .data$wg_sum_walking_34, .data$wg_sum_selfcare_34, .data$wg_sum_remembering_34), na.rm = TRUE),
             wg_sum_4 = sum(c(.data$wg_sum_seeing_4, .data$wg_sum_hearing_4, .data$wg_sum_communication_4, .data$wg_sum_walking_4, .data$wg_sum_selfcare_4, .data$wg_sum_remembering_4), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(wg_sum_234 = ifelse(is.na(.data$wg_sum_seeing_234), NA, .data$wg_sum_234),
             wg_sum_34 = ifelse(is.na(.data$wg_sum_seeing_34), NA, .data$wg_sum_34),
             wg_sum_4 = ifelse(is.na(.data$wg_sum_seeing_4), NA, .data$wg_sum_4),
             wg_sum_234 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years <5, NA, .data$wg_sum_234)),
             wg_sum_34 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years <5, NA, .data$wg_sum_34)),
             wg_sum_4 = ifelse(is.na(.data$age_years), NA, ifelse(.data$age_years <5, NA, .data$wg_sum_4)),
             wgss_hd_score = 0,
             wgss_hd_score = ifelse(is.na(.data$wg_sum_234), NA, ifelse(.data$wg_sum_234 > 0, 2, .data$wgss_hd_score)),
             wgss_hd_score = ifelse(is.na(.data$wg_sum_34), NA, ifelse(.data$wg_sum_34 > 0, 3, .data$wgss_hd_score)),
             wgss_hd_score = ifelse(is.na(.data$wg_sum_4), NA, ifelse(.data$wg_sum_4 > 0, 4, .data$wgss_hd_score)),
             wgss_hd_score = ifelse(is.na(.data$wg_sum_4), NA, ifelse(.data$wg_sum_4 == 6, 9, .data$wgss_hd_score)),
             wgss_hd_score = ifelse(is.na(.data$wgss_hd_score), NA, ifelse(.data$wgss_hd_score == 0, 1, .data$wgss_hd_score)),
             wgss_hd_score = ifelse(.data$age_years < 5, NA, .data$wgss_hd_score),

             disability1 = ifelse(is.na(.data$wg_sum_234), NA, ifelse(.data$wg_sum_234 > 0, 1, 0)),
             disability2 = ifelse(is.na(.data$wg_sum_34) & is.na(.data$wg_sum_234), NA, ifelse(.data$wg_sum_234 >=2 | .data$wg_sum_34 > 0, 1, 0)),
             disability3 = ifelse(is.na(.data$wg_sum_34), NA, ifelse(.data$wg_sum_34 > 0, 1, 0)),
             disability4 = ifelse(is.na(.data$wg_sum_4), NA, ifelse(.data$wg_sum_4 > 0, 1, 0))
      )




  } else if(length(setdiff(wgss_vars, names(df))) > 0 & length(setdiff(wgss_vars, names(df))) < 6) {print("At least one Washington Group Short Set Indicator is included, but all 6 domains are needed to calculate the indicator. Skipping - please check your input if you are trying to calculate Washington Group indicators.")}

  # Calculating IYCF Indicators

  ebf_vars <- c("age_months", "iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")

  isssf_vars <- c("age_months", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")

  mdd_vars <- c("age_months", "iycf_4", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o")
  mmf_vars <- c("age_months", "iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_8")
  mmff_vars <- c("age_months","iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_7a_num")
  flesh_foods_vars <- c("age_months","iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m")
  swt_bv_vars <- c("age_months","iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt")
  unhealthy_food_vars <- c("age_months","iycf_7p", "iycf_7q")
  zero_veg_fruit_vars <- c("age_months","iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h")

  # "IYCF Indicator 1: Ever Breastfed; YES"

  if(length(setdiff(c("iycf_1", "age_months"), colnames(df)))==0) {

    df[c("iycf_1", "age_months")] <- sapply(df[c("iycf_1", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(iycf_evbf = dplyr::case_when(.data$iycf_1 == 1 ~ 1,
                                                 .data$iycf_1 != 1 ~ 0,
                                                 .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_1) ~ NA_integer_))
  }

  # "IYCF Indicator 2: Early Initiation of Breastfeeding;

  if(length(setdiff(c("iycf_2", "age_months"), colnames(df)))==0) {

    df[c("iycf_2", "age_months")] <- sapply(df[c("iycf_2", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(iycf_eibf = dplyr::case_when(.data$iycf_2 == 1 | .data$iycf_2 == 2 ~ 1,
                                                 .data$iycf_2 != 1 & .data$iycf_2 != 2 ~ 0,
                                                 .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_2) ~ NA_integer_))


  }

  # "IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth;

  if(length(setdiff(c("iycf_3", "age_months"), colnames(df)))==0) {

    df[c("iycf_3", "age_months")] <- sapply(df[c("iycf_3", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(iycf_ebf2d = dplyr::case_when(.data$iycf_3 == 2 ~ 1,
                                                  .data$iycf_3 != 2 ~ 0,
                                                  .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_3) ~ NA_integer_))

  }

  # "IYCF Indicator 4: Exclusive Breastfeeding;

  # essential_ebf_vars <- c("age_months", "iycf_4", )
  ebf_foods <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  ebf_liquids <- c("iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j")

  a <- length(setdiff(c("age_months", "iycf_4"), names(df)))
  b <- length(setdiff(ebf_foods, names(df)))
  c <- length(setdiff(ebf_liquids, names(df)))

  if(a == 0 & b > 0 & c > 0) {

    ebf_foods <- intersect(ebf_foods, names(df))
    a <- length(ebf_foods)
    df$count_no_foods <- apply(df[c(ebf_foods)], 1, function(x) sum(x == "2"))

    if(length(ebf_foods) != length(a)) {
      print("Warning: Your dataset appears not to have all the foods from the standard IYCF 2021 question sequence. It is advised you ask about all recommended liquids or there is a risk of overestimating EBF.  ")
      print(paste0("Missing the following variables ", setdiff(ebf_foods, names(df))))
      }

    ebf_liquids <- intersect(ebf_liquids, names(df))
    b <- length(ebf_liquids)
    df$count_no_liquids <- apply(df[c(ebf_liquids)], 1, function(x) sum(x == "2"))

    if(length(ebf_liquids) != length(b)) {
      print("Warning: Your dataset appears not to have all the liquids from the standard IYCF 2021 question sequence. It is advised you ask about all recommended liquids or there is a risk of overestimating EBF.  ")
      print(paste0("Missing the following variables ", setdiff(ebf_liquids, names(df))))

      }

    df[c("iycf_4", ebf_foods, ebf_liquids)] <- sapply(df[c("iycf_4", ebf_foods, ebf_liquids)], as.numeric)

    df <- df %>%
      dplyr::mutate(
        count_foods = a - .data$count_no_foods,
        count_foods = b - .data$count_no_liquids,
        iycf_ebf = dplyr::case_when(
          .data$iycf_4 == 1 & .data$count_foods == 0 & .data$count_liquids == 0 ~ 1,
          .data$iycf_4 != 1 | .data$count_foods > 0 | .data$count_liquids > 0 ~ 0,
          .data$age_months >= 6 | is.na(.data$age_months) | is.na(.data$iycf_4) ~ NA_integer_

      ))

  }

  # "IYCF Indicator 5: Mixed Milk Feeding (MIxMF)

  if(length(setdiff(c("iycf_4", "iycf_6b", "iycf_6c", "age_months"), colnames(df)))==0) {

    df[c("iycf_4", "iycf_6b", "iycf_6c", "age_months")] <- sapply(df[c("iycf_4", "iycf_6b", "iycf_6c", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(iycf_mixmf = dplyr::case_when(
        .data$iycf_4 == 1 & (.data$iycf_6b == 1 | .data$iycf_6c == 1) ~ 1,
        .data$iycf_4 != 1 | (.data$iycf_6b != 1 & .data$iycf_6c != 1) ~ 0,
        .data$age_months >= 6 | is.na(.data$age_months) | is.na(.data$iycf_4) | is.na(.data$iycf_6b) | is.na(.data$iycf_6b) ~ NA_integer_
      ))


  }

  # "IYCF Indicator 6: Continued Breastfeeding 12-23 months;

  if(length(setdiff(c("iycf_4", "age_months"), colnames(df)))==0) {

    df[c("iycf_4", "age_months")] <- sapply(df[c("iycf_4", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(iycf_cbf = dplyr::case_when(
        .data$iycf_4 == 1 ~ 1,
        .data$iycf_4 != 1 ~ 0,
        .data$age_months < 12 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_4) ~ NA_integer_
      ))

  }

  # "IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods (ISSSF)

  ebf_foods <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")

  a <- length(setdiff(c("age_months"), names(df)))
  b <- length(setdiff(ebf_foods, names(df)))

  if(a == 0 & b < 18) {

    ebf_foods <- intersect(ebf_foods, names(df))
    a <- length(ebf_foods)
    df$count_no_foods <- apply(df[c(ebf_foods)], 1, function(x) sum(x == "2"))

    if(length(ebf_foods) != length(a)) {
      print("Warning: Your dataset appears not to have all the foods from the standard IYCF 2021 question sequence. It is advised you ask about all recommended liquids or there is a risk of overestimating ISSSF.  ")
      print(paste0("Missing the following variables ", setdiff(ebf_foods, names(df))))
    }

    ebf_foods <- intersect(ebf_foods, names(df))
    a <- length(ebf_foods)
    df$count_foods <- apply(df[c(ebf_foods)], 1, function(x) sum(x == "1"))

    df[c("age_months", ebf_foods)] <- sapply(df[c("age_months", ebf_foods)], as.numeric)

    df <- df %>%
      dplyr::mutate(
        iycf_isssf = dplyr::case_when(
        .data$count_foods > 0 ~ 1,
        .data$count_foods == 0 ~ 0,
        .data$age_months < 6 | .data$age_months > 8 | is.na(.data$age_months) ~ NA_integer_
      ))
  }

  # "IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD);

  if(length(setdiff(mdd_vars, colnames(df)))==0) {

    df[mdd_vars] <- sapply(df[mdd_vars], as.numeric)

    df <- df %>%
      dplyr::mutate(mdd1 = dplyr::case_when(.data$iycf_4 == 1 ~ 1,
                                            .data$iycf_4 != 1 ~ 0,
                                            is.na(.data$iycf_4) ~ NA_integer_),
                    mdd2 = dplyr::case_when(.data$iycf_7b == 1 | iycf_7b == 1 ~ 1,
                                            .data$iycf_7b != 1 & iycf_7b != 1 ~ 0,
                                            TRUE ~ NA_integer_),
                    mdd3 = dplyr::case_when(.data$iycf_7n == 1 ~ 1,
                                            .data$iycf_7n != 1 ~ 0,
                                            TRUE ~ NA_integer_),
                    mdd4 = dplyr::case_when(.data$iycf_6b == 1 | .data$iycf_6c == 1 | .data$iycf_6d == 1 | .data$iycf_7a == 1 | .data$iycf_7o == 1 ~ 1,
                                            .data$iycf_6b != 1 & .data$iycf_6c != 1 & .data$iycf_6d != 1 & .data$iycf_7a != 1 & .data$iycf_7o != 1 ~ 0,
                                            TRUE ~ NA_integer_),
                    mdd5 = dplyr::case_when(.data$iycf_7i == 1 | .data$iycf_7j == 1 | .data$iycf_7k == 1 | .data$iycf_7m == 1 ~ 1,
                                            .data$iycf_7i != 1 & .data$iycf_7j != 1 & .data$iycf_7k != 1 & .data$iycf_7m != 1 ~ 0,
                                            TRUE ~ NA_integer_),
                    mdd6 = dplyr::case_when(.data$iycf_7l == 1 ~ 1,
                                            .data$iycf_7l != 1 ~ 0,
                                            TRUE ~ NA_integer_),
                    mdd7 = dplyr::case_when(.data$iycf_7c == 1 | .data$iycf_7e == 1 | .data$iycf_7g == 1 ~ 1,
                                            .data$iycf_7c != 1 & .data&iycf_7e != 1 & .data$iycf_7g != 1 ~ 0,
                                            TRUE ~ NA_integer_),
                    mdd8 = dplyr::case_when(.data$iycf_7f == 1 | .data$iycf_7h == 1 ~ 1,
                                            .data$iycf_7f != 1 & .data$iycf_7h != 1 ~ 0,
                                            TRUE ~ NA_integer_)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(iycf_mdd_score = sum(c(.data$mdd1, .data$mdd2, .data$mdd3, .data$mdd4, .data$mdd5, .data$mdd6, .data$mdd7, .data$mdd8), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(iycf_mdd_cat = dplyr::case_when(.data$age_months >= 6 & .data$age_months < 24 & .data$iycf_mdd_score >= 5 ~ 1,
                                                    .data$age_months >= 6 & .data$age_months < 24 & .data$iycf_mdd_score < 5 ~ 0,
                                                    .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$iycf_mdd_score) ~ NA_integer_))

  }

  # "IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF);

  if(length(setdiff(mmf_vars, colnames(df)))==0) {

    df[mmf_vars] <- sapply(df[mmf_vars], as.numeric)

    df <- df %>%
      dplyr::mutate(
        mmf_bf_6to8months = dplyr::case_when(
          .data$iycf_4 == 1 & .data$iycf_8 >= 2 ~ 1,
          .data$iycf_4 != 1 | .data$iycf_8 <2 ~ 0,
          .data$age_months < 6 | .data$age_months >= 8 | is.na(.data$age_months) | is.na(.data$iycf_4) | is.na(.data$iycf_8) ~ NA_integer_),
        mmf_bf_9to23months = dplyr::case_when(
          .data$iycf_4 == 1 & .data$iycf_8 >= 3 ~ 1,
          .data$iycf_4 != 1 | .data$iycf_8 < 3 ~ 0,
          .data$age_months < 9 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_4) | is.na(.data$iycf_8) ~ NA_integer_)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(count_6b_6c_6d_8 = sum(c(.data$iycf_6b_num, .data$iycf_6c_num, .data$iycf_6d_num, .data$iycf_8), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        mmf_nonbf_6to23months = dplyr::case_when(
          .data$iycf_4 != 1 & .data$count_6b_6c_6d_8 >= 4 & .data$iycf_8 >= 1 ~ 1,
          .data$iycf_4 == 1 | .data$count_6b_6c_6d_8 < 4 | .data$iycf_8 < 1 ~ 0,
          .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_4) | is.na(.data$iycf_6b_num) | is.na(.data$iycf_6c_num) | is.na(.data$iycf_6d_num) | is.na(.data$iycf_8) ~ NA_integer_
        ))
  }

  # "IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF);

  if(length(setdiff(mmff_vars, colnames(df)))==0) {

    df[mmff_vars] <- sapply(df[mmff_vars], as.numeric)

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(count_dairy = sum(c(.data$iycf_6b_num, .data$iycf_6c_num, .data$iycf_6d_num, .data$iycf_7a_num), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        iycf_mmff = dplyr::case_when(
          .data$iycf_4 != 1 & .data$count_dairy >= 2 ~ 1,
          .data$iycf_4 == 1 | .data$count_dairy < 2 ~ 0,
          .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_6b_num) | is.na(.data$iycf_6c_num) | is.na(.data$iycf_6d_num) | is.na(.data$iycf_7a_num) ~ NA_integer_
        ))
  }

  # "IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD);"

  if(length(setdiff(c("iycf_mmf", "iycf_mdd_cat", "iycf_mmff", "age_months"), colnames(df)))==0) {

    df[c("iycf_mmf", "iycf_mdd_cat", "iycf_mmff", "age_months")] <- sapply(df[c("iycf_mmf", "iycf_mdd_cat", "iycf_mmff", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(
        iycf_mad = dplyr::case_when(
          .data$iycf_mdd_cat == 1 & .data$iycf_mmf == 1 & (.data$iycf_4 == 1 | .data$iycf_mmff == 1) ~ 1,
          .data$iycf_mdd_cat != 1 | .data$iycf_mmf != 1 | (.data$iycf_4 != 1 & .data$iycf_mmff != 1) ~ 0,
          .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_mdd_cat) | is.na(.data$iycf_mmf) | is.na(.data$iycf_mmff) | is.na(.data$iycf_4) ~ NA_integer_
        )
      )
  }

  # "IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF);

  if(length(setdiff(c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "age_months"), colnames(df)))==0) {

    df[c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "age_months")] <- sapply(df[c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(iycf_eff = dplyr::case_when(.data$iycf_7i == 1 | .data$iycf_7j == 1 | .data$iycf_7k == 1 | .data$iycf_7l == 1 | .data$iycf_7m == 1 ~ 1,
                                                .data$iycf_7i != 1 & .data$iycf_7j != 1 & .data$iycf_7k != 1 & .data$iycf_7l != 1 & .data$iycf_7m != 1 ~ 0,
                                                .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) ~ NA_integer_))

  }

  # "IYCF Indicator 13: Sweet Beverage Consumption 6-23 months (SWB)

  if(length(setdiff(c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt", "age_months"), colnames(df)))==0) {

    df[c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt", "age_months")] <- sapply(df[c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt", "age_months")], as.numeric)


    df <- df %>%
      dplyr::mutate(
        iycf_swb = dplyr::case_when(
          .data$iycf_6c_swt == 1 | .data$iycf_6d_swt == 1 | .data$iycf_6e == 1 | .data$iycf_6f == 1 | .data$iycf_6g == 1 | .data$iycf_6h_swt == 1 | .data$iycf_6j_swt == 1 ~ 1,
          .data$iycf_6c_swt != 1 & .data$iycf_6d_swt != 1 & .data$iycf_6e != 1 & .data$iycf_6f != 1 & .data$iycf_6g != 1 & .data$iycf_6h_swt != 1 & .data$iycf_6j_swt != 1 ~ 0,
          .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_6c_swt) | is.na(.data$iycf_6d_swt) | is.na(.data$iycf_6e) | is.na(.data$iycf_6f) | is.na(.data$iycf_6g) ~ NA_integer_
        )
      )
  }

  # "IYCF Indicator 14: Unhealthy Food Consumption (UFC)

  if(length(setdiff(c("iycf_7p", "iycf_7q", "age_months"), colnames(df)))==0) {

    df[c("iycf_7p", "iycf_7q", "age_months")] <- sapply(df[c("iycf_7p", "iycf_7q", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(
        iycf_ufc = dplyr::case_when(
          .data$iycf_7p == 1 | .data$iycf_7q == 1 ~ 1,
          .data$iycf_7p != 1 & .data$iycf_7q != 1 ~ 0,
          .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_7p) | is.na(.data$iycf_7q) ~ NA_integer_
        ))
  }

  # "IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF)

  if(length(setdiff(c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g" , "iycf_7h", "age_months"), colnames(df)))==0) {

    df[c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g" , "iycf_7h", "age_months")] <- sapply(df[c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g" , "iycf_7h", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(zvf1 = dplyr::case_when(.data$iycf_7c == 2 ~ 1,
                                             .data$iycf_7c != 2 ~ 0,
                                             TRUE ~ NA_integer_),
                    zvf2 = dplyr::case_when(.data$iycf_7e == 2 ~ 1,
                                             .data$iycf_7e != 2 ~ 0,
                                             TRUE ~ NA_integer_),
                    zvf3 = dplyr::case_when(.data$iycf_7f == 2 ~ 1,
                                             .data$iycf_7f != 2 ~ 0,
                                             TRUE ~ NA_integer_),
                    zvf4 = dplyr::case_when(.data$iycf_7g == 2 ~ 1,
                                             .data$iycf_7g != 2 ~ 0,
                                             TRUE ~ NA_integer_),
                    zvf5 = dplyr::case_when(.data$iycf_7h == 2 ~ 1,
                                             .data$iycf_7h != 2 ~ 0,
                                             TRUE ~ NA_integer_)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(zvf_sum = sum(c(.data$zvf1, .data$zvf2, .data$zvf3, .data$zvf4, .data$zvf5), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(iycf_zvf = dplyr::case_when(.data$zvf_sum == 5 ~ 1,
                                                .data$zvf_sum < 5 ~ 0,
                                                .data$age_months < 6 | .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$zvf1) | is.na(.data$zvf2) | is.na(.data$zvf3) | is.na(.data$zvf4) | is.na(.data$zvf5) ~ NA_integer_))
  }

  # "IYCF Indicator 16: Bottle Feeding 0-23 months

  if(length(setdiff(c("iycf_5", "age_months"), colnames(df)))==0) {

    df[c("iycf_5", "age_months")] <- sapply(df[c("iycf_5", "age_months")], as.numeric)

    df <- df %>%
      dplyr::mutate(
        iycf_bof = dplyr::case_when(
          .data$iycf_5 == 1 ~ 1,
          .data$iycf_5 != 1 ~ 0,
          .data$age_months >= 24 | is.na(.data$age_months) | is.na(.data$iycf_5) ~ NA_integer_
        ))
  }

  # Calculate Food Consumption Scores

  fcs_vars <- c("fcs_cereal", "fcs_legumes", "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", "fcs_oil", "fcs_sugar")

  if(length(setdiff(fcs_vars, colnames(df)))==0) {

    df[fcs_vars] <- sapply(df[fcs_vars], as.numeric)

    cutoffs <- c("1", "2")
    print("Now calculating Food Consumption Score (FCS) indicator:")
    a <- readline(cat(paste0("What thresholds do you want to use for Food Consumption Scores (FCS)? Please write '1' for normal cutoffs (21.5 / 35) or '2' for alternate cutoffs (28 / 42).")))
    while(length(setdiff(a, cutoffs))==1) {
      a <- readline(cat(paste0("Invalid input. Please select Please write '1' for normal cutoffs (21.5 / 35) or '2' for alternate cutoffs (28 / 42).", )))
    }

    cat("\014") #clears the console

    df <- df %>%
      dplyr::mutate(fcs_weight_cereal1 = ifelse(is.na(.data$fcs_cereal), NA, .data$fcs_cereal*2) ,
             fcs_weight_legume2 = ifelse(is.na(.data$fcs_legumes), NA, .data$fcs_legumes*3) ,
             fcs_weight_dairy3 = ifelse(is.na(.data$fcs_dairy), NA, .data$fcs_dairy*4) ,
             fcs_weight_meat4 = ifelse(is.na(.data$fcs_meat), NA, .data$fcs_meat*4),
             fcs_weight_veg5 = ifelse(is.na(.data$fcs_veg), NA, .data$fcs_veg*1),
             fcs_weight_fruit6 = ifelse(is.na(.data$fcs_fruit), NA, .data$fcs_fruit*1) ,
             fcs_weight_oil7 = ifelse(is.na(.data$fcs_oil), NA, .data$fcs_oil*0.5),
             fcs_weight_sugar8 = ifelse(is.na(.data$fcs_sugar), NA, .data$fcs_sugar*0.5)
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(fcs_score = sum(.data$fcs_weight_cereal1, .data$fcs_weight_legume2, .data$fcs_weight_dairy3, .data$fcs_weight_meat4, .data$fcs_weight_veg5, .data$fcs_weight_fruit6, .data$fcs_weight_oil7, .data$fcs_weight_sugar8, na.rm = TRUE),
      ) %>%
      dplyr::ungroup()

    if(a == "1") {

      df <- df %>%
        dplyr::mutate(fcs_score = ifelse(is.na(.data$fcs_weight_cereal1), NA, .data$fcs_score),
               fcs_cat = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score < 21.5, "Poor", ifelse(.data$fcs_score <=35, "Borderline", ifelse(.data$fcs_score>35 & .data$fcs_score < 200, "Acceptable", NA)))))

    } else if(a == "2") {

      df <- df %>%
        dplyr::mutate(fcs_score = ifelse(is.na(.data$fcs_weight_cereal1), NA, .data$fcs_score),
               fcs_cat = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score <= 28, "Poor", ifelse(.data$fcs_score <=42, "Borderline", ifelse(.data$fcs_score>42 & .data$fcs_score < 200, "Acceptable", NA)))))

    }



  }

  # Calculate Household Dietary Diversity Scores

  hdds_vars <- c("hdds_cereals", "hdds_tubers", "hdds_veg", "hdds_fruit", "hdds_meat", "hdds_eggs", "hdds_fish", "hdds_legumes",
                 "hdds_dairy", "hdds_oils", "hdds_sugars", "hdds_condiments")

  if(length(setdiff(hdds_vars, colnames(df)))==0) {

    df[hdds_vars] <- lapply(df[hdds_vars], as.numeric)

    df <- df %>%
      dplyr::mutate(hdds1 = ifelse(is.na(.data$hdds_cereals), NA, ifelse(.data$hdds_cereals == 1, 1, 0)),
             hdds2 = ifelse(is.na(.data$hdds_tubers), NA, ifelse(.data$hdds_tubers == 1, 1, 0)),
             hdds3 = ifelse(is.na(.data$hdds_veg), NA, ifelse(.data$hdds_veg == 1, 1, 0)),
             hdds4 = ifelse(is.na(.data$hdds_fruit), NA, ifelse(.data$hdds_fruit == 1, 1, 0)),
             hdds5 = ifelse(is.na(.data$hdds_meat), NA, ifelse(.data$hdds_meat == 1, 1, 0)),
             hdds6 = ifelse(is.na(.data$hdds_eggs), NA, ifelse(.data$hdds_eggs == 1, 1, 0)),
             hdds7 = ifelse(is.na(.data$hdds_fish), NA, ifelse(.data$hdds_fish == 1, 1, 0)),
             hdds8 = ifelse(is.na(.data$hdds_legumes), NA, ifelse(.data$hdds_legumes == 1, 1, 0)),
             hdds9 = ifelse(is.na(.data$hdds_dairy), NA, ifelse(.data$hdds_dairy == 1, 1, 0)),
             hdds10 = ifelse(is.na(.data$hdds_oils), NA, ifelse(.data$hdds_oils == 1, 1, 0)),
             hdds11 = ifelse(is.na(.data$hdds_sugars), NA, ifelse(.data$hdds_sugars == 1, 1, 0)),
             hdds12 = ifelse(is.na(.data$hdds_condiments), NA, ifelse(.data$hdds_condiments == 1, 1, 0))
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hdds_score = sum(.data$hdds1, .data$hdds2, .data$hdds3, .data$hdds4, .data$hdds5, .data$hdds6, .data$hdds7, .data$hdds8, .data$hdds9, .data$hdds10, .data$hdds11, .data$hdds12, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(hdds_cat = ifelse(.data$hdds_score <= 2, "Low", ifelse(.data$hdds_score <= 4, "Medium", ifelse(.data$hdds_score > 4 & .data$hdds_score <15, "High", NA))))

  }

  # Calculate Household Hunger Score

  hhs_vars <- c("hhs_nofoodhh_1", "hhs_nofoodhh_1a", "hhs_sleephungry_2", "hhs_sleephungry_2a", "hhs_alldaynight_3", "hhs_alldaynight_3a")

  if(length(setdiff(hhs_vars, colnames(df)))==0) {

    df <- df %>%
      dplyr::mutate(hhs_comp1 = ifelse(is.na(.data$hhs_nofoodhh_1), NA, ifelse(.data$hhs_nofoodhh_1 == "2", 0, ifelse(.data$hhs_nofoodhh_1 == "1" & (.data$hhs_nofoodhh_1a == "2" | .data$hhs_nofoodhh_1a == "1"), 1, ifelse(.data$hhs_nofoodhh_1 == "1" & .data$hhs_nofoodhh_1a == 3, 2, 0)))),
             hhs_comp2 = ifelse(is.na(.data$hhs_sleephungry_2), NA, ifelse(.data$hhs_sleephungry_2 == "2", 0, ifelse(.data$hhs_sleephungry_2 == "1" & (.data$hhs_sleephungry_2a == "2" | .data$hhs_sleephungry_2a == "1"), 1, ifelse(.data$hhs_sleephungry_2 == "1" & .data$hhs_sleephungry_2a == "3", 2, 0)))),
             hhs_comp3 = ifelse(is.na(.data$hhs_alldaynight_3), NA, ifelse(.data$hhs_alldaynight_3 == "2", 0, ifelse(.data$hhs_alldaynight_3 == "1" & (.data$hhs_alldaynight_3a == "2" | .data$hhs_alldaynight_3a == "1"), 1, ifelse(.data$hhs_alldaynight_3 == "1" & .data$hhs_alldaynight_3a == "3", 2, 0))))
      ) %>% dplyr::rowwise() %>%
      dplyr::mutate(hhs_score = sum(.data$hhs_comp1, .data$hhs_comp2, .data$hhs_comp3, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(hhs_cat_ipc = dplyr::case_when(.data$hhs_score == 0 ~ "None",
                                                   .data$hhs_score == 1 ~ "Little",
                                                   .data$hhs_score == 2 | .data$hhs_score == 3 ~ "Moderate",
                                                   .data$hhs_score == 4 ~ "Severe",
                                                   .data$hhs_score == 5 | .data$hhs_score == 6 ~ "Very Severe",
                                                   TRUE ~ NA_character_),
                    hhs_cat = dplyr::case_when(.data$hhs_score == 0 | .data$hhs_score == 1 ~ "No or Little",
                                               .data$hhs_score == 2 | .data$hhs_score == 3 ~ "Moderate",
                                               .data$hhs_score == 4 | .data$hhs_score == 5 | .data$hhs_score == 6 ~ "Severe",
                                               TRUE ~ NA_character_))

  }

  # Calculate reduced Coping Strategies Index

  rcsi_vars <- c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5")

  if(length(setdiff(rcsi_vars, colnames(df)))==0) {

    df <- df %>%
      dplyr::mutate(rcsi_weight1 = .data$rcsi_lesspreferred_1*1,
             rcsi_weight2 = .data$rcsi_borrowfood_2*2,
             rcsi_weight3 = .data$rcsi_limitportion_3*1,
             rcsi_weight4 = .data$rcsi_restrict_4*3,
             rcsi_weight5 = .data$rcsi_reducemeals5*1) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rcsi_score = sum(.data$rcsi_weight1, .data$rcsi_weight2, .data$rcsi_weight3, .data$rcsi_weight4, .data$rcsi_weight5, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(rcsi_cat = ifelse(is.na(.data$rcsi_score), NA, ifelse(.data$rcsi_score <=3, "No to Low", ifelse(.data$rcsi_score>=4 & .data$rcsi_score<=18, "Medium", ifelse(.data$rcsi_score>18 & .data$rcsi_score <=1000, "High", NA)))),
      )

  }

  # Livelihood Coping Strategies

  lcs_vars <- c("lcs_emergency", "lcs_crisis", "lcs_stress",
                "lcs_stress_yes", "lcs_crisis_yes", "lcs_emergency_yes",
                "lcs_stress_exhaust", "lcs_crisis_exhaust", "lcs_emergency_exhaust")

  if(length(setdiff(lcs_vars, colnames(df)))==0) {

    df[lcs_vars] <- lapply(df[lcs_vars], as.numeric)

    df <- df %>%
      dplyr::mutate(lcs_cat = ifelse(.data$lcs_emergency == 1, "Emergency", ifelse(.data$lcs_crisis == 1, "Crisis", ifelse(.data$lcs_stress == 1, "Stress", ifelse(.data$lcs_stress == 0 & .data$lcs_crisis == 0 & .data$lcs_emergency == 0, "None", NA)))),
                    lcs_cat_yes = dplyr::case_when(.data$lcs_emergency_yes == 1 ~ "Emergency", .data$lcs_crisis_yes == 1 ~ "Crisis", .data$lcs_stress_yes == 1 ~ "Stress", .data$lcs_cat == "None" ~ "None", TRUE ~ NA_character_ ),
                    lcs_cat_exhaust = dplyr::case_when(.data$lcs_emergency_exhaust == 1 ~ "Emergency", .data$lcs_crisis_exhaust == 1 ~ "Crisis", .data$lcs_stress_exhaust == 1 ~ "Stress", .data$lcs_cat == "None" ~ "None", TRUE ~ NA_character_ ))

  }

  # Calculating FEWSNET Food Consumption Matrix

  fews_vars <- c("fcs_cat", "rcsi_cat", "hhs_cat_ipc")

  if(length(setdiff(fews_vars, colnames(df)))==0) {

    df <- df %>% dplyr::mutate(fc_cell = ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "No to Low", 1,
                                         ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "No to Low", 2,
                                                ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "No to Low", 3,
                                                       ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "No to Low", 4,
                                                              ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "No to Low", 5,
                                                                     ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "No to Low", 6,
                                                                            ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "No to Low", 7,
                                                                                   ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "No to Low", 8,
                                                                                          ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "No to Low", 9,
                                                                                                 ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "No to Low", 10,
                                                                                                        ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "No to Low", 11,
                                                                                                               ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "No to Low", 12,
                                                                                                                      ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "No to Low", 13,
                                                                                                                             ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "No to Low", 14,
                                                                                                                                    ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "No to Low", 15,
                                                                                                                                           ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "Medium", 16,
                                                                                                                                                  ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "Medium", 17,
                                                                                                                                                         ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "Medium", 18,
                                                                                                                                                                ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "Medium", 19,
                                                                                                                                                                       ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "Medium", 20,
                                                                                                                                                                              ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "Medium", 21,
                                                                                                                                                                                     ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "Medium", 22,
                                                                                                                                                                                            ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "Medium", 23,
                                                                                                                                                                                                   ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "Medium", 24,
                                                                                                                                                                                                          ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "Medium", 25,
                                                                                                                                                                                                                 ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "Medium", 26,
                                                                                                                                                                                                                        ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "Medium", 27,
                                                                                                                                                                                                                               ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "Medium", 28,
                                                                                                                                                                                                                                      ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "Medium", 29,
                                                                                                                                                                                                                                             ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "Medium", 30,
                                                                                                                                                                                                                                                    ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "Severe", 31,
                                                                                                                                                                                                                                                           ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "Severe", 32,
                                                                                                                                                                                                                                                                  ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "Severe", 33,
                                                                                                                                                                                                                                                                         ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "Severe", 34,
                                                                                                                                                                                                                                                                                ifelse(.data$fcs_cat == "Acceptable" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "Severe", 35,
                                                                                                                                                                                                                                                                                       ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "Severe", 36,
                                                                                                                                                                                                                                                                                              ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "Severe", 37,
                                                                                                                                                                                                                                                                                                     ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "Severe", 38,
                                                                                                                                                                                                                                                                                                            ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "Severe", 39,
                                                                                                                                                                                                                                                                                                                   ifelse(.data$fcs_cat == "Borderline" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "Severe", 40,
                                                                                                                                                                                                                                                                                                                          ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "None" & .data$rcsi_cat == "Severe", 41,
                                                                                                                                                                                                                                                                                                                                 ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Little" & .data$rcsi_cat == "Severe", 42,
                                                                                                                                                                                                                                                                                                                                        ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Moderate" & .data$rcsi_cat == "Severe", 43,
                                                                                                                                                                                                                                                                                                                                               ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Severe" & .data$rcsi_cat == "Severe", 44,
                                                                                                                                                                                                                                                                                                                                                      ifelse(.data$fcs_cat == "Poor" & .data$hhs_cat_ipc == "Very Severe" & .data$rcsi_cat == "Severe", 45, NA))))))))))))))))))))))))))))))))))))))))))))))

    df <- df %>% dplyr::mutate(fc_phase = ifelse(.data$fc_cell == 1 | .data$fc_cell == 6, "Phase 1 FC",
                                          ifelse(.data$fc_cell == 2 |.data$fc_cell == 3|.data$fc_cell == 7|.data$fc_cell == 11|.data$fc_cell == 12|.data$fc_cell == 16|.data$fc_cell == 17|.data$fc_cell == 18|.data$fc_cell == 21|.data$fc_cell == 22|.data$fc_cell == 26|.data$fc_cell == 31|.data$fc_cell == 32|.data$fc_cell == 36, "Phase 2 FC",
                                                 ifelse(.data$fc_cell == 4|.data$fc_cell == 5|.data$fc_cell == 8|.data$fc_cell == 9|.data$fc_cell == 13|.data$fc_cell == 19|.data$fc_cell == 20|.data$fc_cell == 23|.data$fc_cell == 24|.data$fc_cell == 27|.data$fc_cell == 28|.data$fc_cell == 33|.data$fc_cell == 34|.data$fc_cell == 37|.data$fc_cell == 38|.data$fc_cell == 41|.data$fc_cell == 42|.data$fc_cell == 43,"Phase 3 FC",
                                                        ifelse(.data$fc_cell == 10|.data$fc_cell == 14|.data$fc_cell == 15|.data$fc_cell == 25|.data$fc_cell == 29|.data$fc_cell == 35|.data$fc_cell == 39|.data$fc_cell == 40|.data$fc_cell == 44, "Phase 4 FC",
                                                               ifelse(.data$fc_cell == 30 | .data$fc_cell == 45, "Phase 5 FC", NA))))))

  }

  # Calculating FEWSNET Food Consumption-Livelihood Coping Matrix

  fews_lcs_vars <- c("fcs_cat", "rcsi_cat", "hhs_cat", "lcs_cat")

  if(length(setdiff(fews_lcs_vars, colnames(df)))==0) {

    df <- df %>% dplyr::mutate(fclc_phase = ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "None", "Phase 1 - FCLC",
                                            ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "Stress", "Phase 1 - FCLC",
                                                   ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "Crisis", "Phase 2 - FCLC",
                                                          ifelse(.data$fc_phase == "Phase 1 FC" & .data$lcs_cat_yes == "Emergency", "Phase 3 - FCLC",
                                                                 ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "None", "Phase 2 - FCLC",
                                                                        ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "Stress", "Phase 2 - FCLC",
                                                                               ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "Crisis", "Phase 3 - FCLC",
                                                                                      ifelse(.data$fc_phase == "Phase 2 FC" & .data$lcs_cat_yes == "Emergency", "Phase 3 - FCLC",
                                                                                             ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "None", "Phase 3 - FCLC",
                                                                                                    ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "Stress", "Phase 3 - FCLC",
                                                                                                           ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "Crisis", "Phase 3 - FCLC",
                                                                                                                  ifelse(.data$fc_phase == "Phase 3 FC" & .data$lcs_cat_yes == "Emergency", "Phase 4 - FCLC",
                                                                                                                         ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "None", "Phase 4 - FCLC",
                                                                                                                                ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "Stress", "Phase 4 - FCLC",
                                                                                                                                       ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "Crisis", "Phase 4 - FCLC",
                                                                                                                                              ifelse(.data$fc_phase == "Phase 4 FC" & .data$lcs_cat_yes == "Emergency", "Phase 4 - FCLC",
                                                                                                                                                     ifelse(.data$fc_phase == "Phase 5 FC", "Phase 5 - FCLC", NA))))))))))))))))))

  }

  # Calculating Severe and Catastrophic Health Expenditures

  if(!is.null(monthly_expenditures) & (c("health_exp") %in% names(df))) {

    if(!is.null(period_expenditures) & !is.null(num_period_months)) {

      df[monthly_expenditures] <- lapply(df[monthly_expenditures], as.numeric)
      df[period_expenditures] <- lapply(df[period_expenditures], as.numeric)

      df$month_exp1 <- rowSums(df[,monthly_expenditures], na.rm = TRUE)
      df$month_exp2 <- rowSums(df[,period_expenditures], na.rm = TRUE) / as.numeric(num_period_months)

      df$total_monthly_exp <- rowSums(df[,c("month_exp1", "month_exp2")])


      df <- df %>%
        dplyr::mutate(health_exp = ifelse(is.na(.data$health_exp), 0, .data$health_exp / as.numeric(num_period_months)),
               prop_health_exp = ifelse(is.na(.data$total_monthly_exp), NA , .data$health_exp / .data$total_monthly_exp) ,
               severe_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp >= 0.10 & .data$prop_health_exp <=0.25, 1, 0)),
               catastrophic_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp > 0.25, 1, 0)))

    } else {

      df[monthly_expenditures] <- lapply(df[monthly_expenditures], as.numeric)
      df$total_monthly_exp <- rowSums(df[,monthly_expenditures], na.rm = TRUE)

      df <- df %>%
        dplyr::mutate(health_exp = ifelse(is.na(.data$health_exp), 0, .data$health_exp),
               prop_health_exp = ifelse(is.na(.data$total_monthly_exp), NA , .data$health_exp / .data$total_monthly_exp) ,
               severe_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp >= 0.10 & .data$prop_health_exp <=0.25, 1, 0)),
               catastrophic_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp > 0.25, 1, 0)))

    }

  }

  # Calculating Food Expenditures Share

  if(!is.null(monthly_expenditures) & (c("food_exp") %in% names(df))) {

    if(!is.null(period_expenditures) & !is.null(num_period_months)) {

      df[monthly_expenditures] <- lapply(df[monthly_expenditures], as.numeric)
      df[period_expenditures] <- lapply(df[period_expenditures], as.numeric)

      df$month_exp1 <- rowSums(df[,monthly_expenditures], na.rm = TRUE)
      df$month_exp2 <- rowSums(df[,period_expenditures], na.rm = TRUE) / as.numeric(num_period_months)

      df$total_monthly_exp <- rowSums(df[,c("month_exp1", "month_exp2")])


      df <- df %>%
        dplyr::mutate(food_exp = ifelse(is.na(.data$health_exp), 0, .data$health_exp / as.numeric(num_period_months)),
                      prop_food_exp = ifelse(is.na(.data$total_monthly_exp), NA , .data$health_exp / .data$total_monthly_exp) ,
                      food_exp_share = dplyr::case_when(prop_food_exp < 0.5 ~ "1", prop_food_exp >= 0.5 & prop_food_exp <0.65 ~ "2", prop_food_exp >= 0.65 & prop_food_exp < 0.75 ~ "3", prop_food_exp >= 0.75 ~ "4", TRUE ~ NA_character_ )
                      )

    } else {

      df[monthly_expenditures] <- lapply(df[monthly_expenditures], as.numeric)
      df$total_monthly_exp <- rowSums(df[,monthly_expenditures], na.rm = TRUE)

      df <- df %>%
        dplyr::mutate(food_exp = ifelse(is.na(.data$health_exp), 0, .data$health_exp),
                      prop_food_exp = ifelse(is.na(.data$total_monthly_exp), NA , .data$health_exp / .data$total_monthly_exp) ,
                      food_exp_share =  dplyr::case_when(prop_food_exp < 0.5 ~ "1", prop_food_exp >= 0.5 & prop_food_exp <0.65 ~ "2", prop_food_exp >= 0.65 & prop_food_exp < 0.75 ~ "3", prop_food_exp >= 0.75 ~ "4", TRUE ~ NA_character_ )
                      )

    }

  }


  # Calculating Childhood Vaccination Indicators

  # if(c("penta") %in% names(df)) {
  #   df <- df %>%
  #     dplyr::mutate(age_days_penta1 = as.numeric(abs(lubridate::as_date(.data$penta_date1) - lubridate::as_date(.data$dob_date))),
  #            age_days_penta2 = as.numeric(abs(lubridate::as_date(.data$penta_date2) - lubridate::as_date(.data$dob_date))),
  #            age_days_penta3 = as.numeric(abs(lubridate::as_date(.data$penta_date3) - lubridate::as_date(.data$dob_date))),
  #            age_days_at_interview = ifelse(!is.na(.data$dob_date), as.numeric(abs(lubridate::as_date(.data$date_dc) - lubridate::as_date(.data$dob_date))), ifelse(is.na(.data$age_months), NA, .data$age_months*(365/12))),
  #
  #            diff_age_penta12 = ifelse(is.na(.data$age_days_penta2), NA, .data$age_days_penta2 - .data$age_days_penta1),
  #            diff_age_penta23 = ifelse(is.na(.data$age_days_penta3), NA, .data$age_days_penta3 - .data$age_days_penta2),
  #
  #            crude_penta_count = ifelse(is.na(.data$penta_count), NA, as.numeric(.data$penta_count)),
  #
  #            crude_penta = ifelse(is.na(.data$penta), NA, ifelse(.data$penta == 1 | .data$penta == 2, 1, 0)),
  #            crude_penta_dose1 = ifelse(is.na(.data$crude_penta), NA, ifelse(.data$crude_penta == 1 & .data$crude_penta_count >= 1, 1, 0)),
  #            crude_penta_dose2 = ifelse(is.na(.data$crude_penta), NA, ifelse(.data$crude_penta == 1 & .data$crude_penta_count >= 2, 1, 0)),
  #            crude_penta_dose3 = ifelse(is.na(.data$crude_penta), NA, ifelse(.data$crude_penta == 1 & .data$crude_penta_count >= 3, 1, 0)),
  #
  #            flag_age_penta_dose1 = ifelse(is.na(.data$age_days_at_interview), NA, ifelse(is.na(.data$crude_penta_dose1), NA, ifelse(.data$age_days_at_interview < 42 & .data$crude_penta == 1, 1, 0))), # if 1st dose of PENTA occured before appropriate age
  #            flag_age_penta_dose2 = ifelse(is.na(.data$age_days_at_interview), NA, ifelse(is.na(.data$crude_penta_dose2), NA, ifelse(.data$age_days_at_interview < 72 & .data$crude_penta == 1, 1, 0))), # if 2nd dose of PENTA occured before appropriate age
  #            flag_age_penta_dose3 = ifelse(is.na(.data$age_days_at_interview), NA, ifelse(is.na(.data$crude_penta_dose3), NA, ifelse(.data$age_days_at_interview < 98 & .data$crude_penta == 1, 1, 0))), # if 3rd dose of PENTA occured before appropriate age
  #            flag_timediff_penta12 = ifelse(is.na(.data$diff_age_penta12), NA, ifelse(.data$diff_age_penta12 < 28, 1, 0)), # if 2nd dose of PENTA was administered less than 4 weeks after the 1st dose
  #            flag_timediff_penta23 = ifelse(is.na(.data$diff_age_penta23), NA, ifelse(.data$diff_age_penta23 < 28, 1, 0)), # if 3rd dose of PENTA was administered less than 4 weeks after the 2nd dose
  #
  #            valid_penta_dose1 = ifelse(is.na(.data$penta), NA, ifelse(.data$penta == 2, ifelse(!is.na(.data$penta_date1), 1, 0), 0)),
  #            valid_penta_dose1 = ifelse(is.na(.data$penta), NA, ifelse(is.na(.data$age_days_penta1), .data$valid_penta_dose1, ifelse(.data$age_days_penta1 < 42, NA, .data$valid_penta_dose1))), #if received penta dose 1 before recommended age
  #            valid_penta_dose2 = ifelse(is.na(.data$penta), NA, ifelse(.data$penta == 2, ifelse(!is.na(.data$penta_date2), 1, 0), 0)),
  #            valid_penta_dose2 = ifelse(is.na(.data$penta), NA, ifelse(is.na(.data$age_days_penta2), .data$valid_penta_dose2, ifelse(.data$age_days_penta2 < 70, NA, .data$valid_penta_dose2))), #if received penta dose 2 before recommended age
  #            valid_penta_dose2 = ifelse(is.na(.data$penta), NA, ifelse(is.na(.data$diff_age_penta12), .data$valid_penta_dose2, ifelse(.data$diff_age_penta12 < 28, 0, .data$valid_penta_dose2))),
  #            valid_penta_dose3 = ifelse(is.na(.data$penta), NA, ifelse(.data$penta == 2, ifelse(!is.na(.data$penta_date3), 1, 0), 0)),
  #            valid_penta_dose3 = ifelse(is.na(.data$penta), NA, ifelse(is.na(.data$age_days_penta3), .data$valid_penta_dose3, ifelse(.data$age_days_penta3 < 98, NA, .data$valid_penta_dose3))), #if received penta dose 3 before recommended age
  #            valid_penta_dose3 = ifelse(is.na(.data$penta), NA, ifelse(is.na(.data$diff_age_penta23), .data$valid_penta_dose3, ifelse(.data$diff_age_penta23 < 28, 0, .data$valid_penta_dose3))),
  #
  #            valid_penta_count = ifelse(!is.na(.data$valid_penta_dose3), 3, ifelse(!is.na(.data$valid_penta_dose2), 2, ifelse(!is.na(.data$valid_penta_dose1), 1, 0))),
  #
  #
  #
  #     )
  # }

  # if(c("measles") %in% names(df)) {
  #
  #   df <- df %>%
  #     dplyr::mutate(age_days_measles1 = as.numeric(abs(lubridate::as_date(.data$measles_date1) - lubridate::as_date(.data$dob_date))),
  #            age_days_at_interview = ifelse(!is.na(.data$dob_date), as.numeric(abs(lubridate::as_date(.data$date_dc) - lubridate::as_date(.data$dob_date))), ifelse(is.na(.data$age_months), NA, .data$age_months*(365/12))),
  #            crude_measles = ifelse(is.na(.data$measles), NA, ifelse(.data$measles == 1 | .data$measles == 2, 1, 0)),
  #            valid_measles = ifelse(is.na(.data$measles), NA, ifelse(.data$measles == 2, 1, 0)),
  #            valid_measles = ifelse(is.na(.data$measles), NA, ifelse(is.na(.data$age_days_measles1), .data$valid_measles, ifelse(.data$age_days_measles1 < 252, NA, .data$valid_measles))), #if received measles dose 1 before recommended age
  #
  #            flag_age_crude_measles = ifelse(is.na(.data$age_days_at_interview), NA, ifelse(is.na(.data$crude_measles), NA, ifelse(.data$crude_measles == 1 & .data$age_days_at_interview < 252, 1, 0))),
  #
  #     )
  #
  # }

  return(df)

}
