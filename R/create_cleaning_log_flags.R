#' Create Cleaning Log from Data Quality Flags
#'
#' This function aggregates all flags for nutrition and health indicators analysed in this package and
#' generates a REACH style cleaning log. An analyst must still review the log to make final decisions
#' on what should or should not be changed.
#'
#' @param df Inputs a dataframe which has been standardized with the format_nut_health_indicators function and
#' flagged with the flag_nut_health_issues function. It will also recognize flags from the flag_anthro_issues,
#' flag_mortality_issues, and flag_iycf_issues functions
#' @param uuid_col Inputs a character value to identify the uuid column in the dataset. This will be included
#' in the returned dataframe to facilitate cleaning.
#' @param file_path Inputs a character file_path. Use this argument if you want to save a xlsx file of the cleaning log
#' as a part of running this function.
#'
#' @return Returns a dataframe object, styled as a REACH cleaning log.
#' @export
#'
#' @examples
#' \dontrun{create_cleaning_log_flags(df = mydata, uuid_col = "X_uuid",
#' file_path = "mycleaninglogs/health_cleaning_log.xlsx")}
#' @importFrom rlang .data
create_cleaning_log_flags <- function(df, uuid_col, file_path = NULL) {

  if(is.null(uuid_col)) { stop("Must specify a uuid_col, or any column that is specific for each record in the dataset. Please revise your input.")}
  if(dplyr::is_grouped_df(df)) {df <- df %>% dplyr::ungroup()}

  uuid <- c("")
  question.name <- c("")
  issue <- c("")
  feedback <- c("")
  changed <- c("")
  old.value <- c("")
  new.value <- c("")
  description <- c("")

  cl <- data.frame(uuid, question.name, issue, feedback, changed, old.value, new.value, description)

  #demographic and mortality flags
  if(length(setdiff(c("flag_birth", "age_years", "birth"), names(df)))==0) {

    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_birth", cols = c("age_years", "birth"), description = "Person was born within recall period, but is 1 full year or older, not possible. ")
    cl <- rbind(cl, cl2)

  }
  if(length(setdiff(c("flag_join_left", "join", "left"), names(df)))==0) {

    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_join_left", cols = c("join", "left"), description = "Person joined and left during recall period. Possible, but not common.")
    cl <- rbind(cl, cl2)

  }
  if(length(setdiff(c("flag_missing_sex", "sex"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_sex", cols = c("sex"), description = "Sex of individual is missing.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_missing_age_years", "age_years"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_age_years", cols = c("age_years"), description = "Age in years of individual is missing.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_missing_date_dc", "date_dc"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_date_dc", cols = c("date_dc"), description = "Date of Data Collection is missing.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_missing_cluster", "cluster"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_cluster", cols = c("cluster"), description = "Cluster id is missing.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_missing_enum", "enum"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_enum", cols = c("enum"), description = "Enumerator or Team id is missing.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_missing_death_cause", "death", "death_cause"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_death_cause", cols = c("death", "death_cause"), description = "Cause of death is missing for reported death.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_missing_death_location", "death", "death_location"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_missing_death_cause", cols = c("death", "death_location"), description = "Location of death is missing for reported death.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_births"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_birth", cols = c("birth"), description = "Multiple births reported ina household. It is possible, but less likely to occur.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_deaths"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_deaths", cols = c("death"), description = "Multiple deaths reported in a household. It is possible, but less likely to occur.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_prop_joins", "birth"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_prop_joins", cols = c("join"), description = "Multiple deaths reported in a household. It is possible, but less likely to occur.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_prop_lefts", "left"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_prop_lefts", cols = c("left"), description = "Multiple deaths reported in a household. It is possible, but less likely to occur.")
    cl <- rbind(cl, cl2)
  }

  # if(length(setdiff(c("flag_duplicate_hh"), names(df)))==0) {
  #   cl2 <- cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_duplicate_hh", cols = names(df), description = "Multiple deaths reported in a household. It is possible, but less likely to occur.")
  #   cl <- rbind(cl, cl2)
  # }
  #
  # #anthropometric flags
  if(length(setdiff(c("wfhz_smart_flag", "weight", "height"), names(df)))==0) {
    print((names(cl)))
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "wfhz_smart_flag", cols = c("weight", "height"), description = "Extreme weight-for-height z-score detected. Check weight and height inputs for data entry errors, or check with enuemrator for measurement errors.")
    print((names(cl2)))
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("hfaz_smart_flag", "height", "age_months"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "hfaz_smart_flag", cols = c("height", "age_months"), description = "Extreme height-for-age z-score detected. Check height and age in months inputs for data entry errors, or check with enumerator for measurement errors.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("wfaz_smart_flag", "weight", "age_months"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "wfaz_smart_flag", cols = c("weight", "age_months"), description = "Extreme weight-for-age z-score detected. Check weight and age in months inputs for data entry errors, or check with enumerator for measurement errors.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("mfaz_smart_flag", "muac", "age_months"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "mfaz_smart_flag", cols = c("muac", "age_months"), description = "Extreme MUAC-for-age z-score detected. Check MUAC and age in months inputs for data entry errors, or check with enumerator for measurement errors.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("muac_flag", "muac"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "muac_flag", cols = c("muac"), description = "Extreme MUAC measurement detected. Double check data entry or check with enumerator for measurement error.")
    cl <- rbind(cl, cl2)
  }

  # #iycf flags

  foods <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  foods_to_check <- intersect(foods, colnames(df))
  liquids <- c("iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j")
  liquids_to_check <- intersect(liquids, colnames(df))

  if(length(setdiff(c("flag_no_foods ", foods_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_no_foods", cols = foods_to_check, description = "No foods at all were reported consumed by the child 6-23 months, which is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_yes_foods ", foods_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_yes_foods", cols = foods_to_check, description = "All foods at all were reported consumed by the child 6-23 months, which is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_yes_foods ", foods_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_yes_foods", cols = foods_to_check, description = "All foods at all were reported consumed by the child 6-23 months, which is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_some_foods_no_meal ", foods_to_check, "iycf_8"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_some_foods_no_meal", cols = c(foods_to_check, "iycf_8"), description = "Some foods were reported consumed by the child 6-23 months, but soft, semi-solid or solid foods were reported consumed 0 times. Logical inconsistency.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_yes_liquids", "iycf_4", foods_to_check, liquids_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_yes_liquids", cols = c(liquids_to_check), description = "All liquids were mentioned consumed yesterday, which is unlikely.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_no_anything", liquids_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_no_anything", cols = c("iycf_4", foods_to_check, liquids_to_check), description = "No foods or liquids consumed at all for a child 6-23 months, which is unlikely.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_high_mdd_low_mmf", foods_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_high_mdd_low_mmf", cols = c("iycf_4", foods_to_check, liquids_to_check, "iycf_8"), description = "High dietary diversity, but only one or fewer meals consumed at for a child 6-23 months, which is unlikely.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_under6_nobf_nomilk", foods_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_under6_nobf_nomilk", cols = c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num"), description = "Child under 6 months with no breastmilk, milk, formula, or dairy at all, which is unlikely.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_meats_nostaples", foods_to_check), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_meats_nostaples", cols = c(foods_to_check), description = "Child consumed protein foods, but no staple foods, which is unusual.")
    cl <- rbind(cl, cl2)
  }



  #washington group flags

  wgss_vars <- c("wgss1_seeing", "wgss2_hearing", "wgss3_walking", "wgss4_remembering", "wgss5_selfcare", "wgss6_communicating")

  if(length(setdiff(c("flag_wgss_age", "age_years", wgss_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_wgss_age", cols = c("age_years", wgss_vars), description = "Washington Group Short Set values are present for a person under-5 years of age. This indicator set is not applicable for this age group.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_wgss_sco_score", wgss_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_wgss_sco_score", cols = wgss_vars, description = "Washington Group Short Set values are present for a person under-5 years of age. This indicator set is not applicable for this age group.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("wgss_hd_score", wgss_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "wgss_hd_score", cols = wgss_vars, description = "Washington Group Short Set values are present for a person under-5 years of age. This indicator set is not applicable for this age group.")
    cl <- rbind(cl, cl2)
  }

  # FSL Flags ##########

  fcs_vars <- c("fcs_cereal", "fcs_legumes", "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", "fcs_oil", "fcs_sugar")
  hhs_vars <- c("hhs_nofoodhh_1", "hhs_nofoodhh_1a", "hhs_sleephungry_2", "hhs_sleephungry_2a", "hhs_alldaynight_3", "hhs_alldaynight_3a")
  hdds_vars <- c("hdds_cereals", "hdds_tubers", "hdds_veg", "hdds_fruit", "hdds_meat", "hdds_eggs", "hdds_fish", "hdds_legumes",
                 "hdds_dairy", "hdds_oils", "hdds_sugars", "hdds_condiments", "hdds_score")
  rcsi_vars <- c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5", "rcsi_score")
  lcs_vars <- c("lcs_crisis", "lcs_emergency", "lcs_stress")

  if(length(setdiff(c("flag_above7_fcs", fcs_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_above7_fcs", cols = fcs_vars, description = "At least one FCS variable was reported more than 7 days, which is not possible for the question.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_meat_cereal_ratio", fcs_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_meat_cereal_ratio", cols = fcs_vars, description = "Significantly more days of meat/fish/egg consumption was reported compared to staple foods like cereals or tubers. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_low_fcs", fcs_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_low_fcs", cols = fcs_vars, description = "This household had a very low Food Consumption Score reported (< 5). This is improbable.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_high_fcs", fcs_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_high_fcs", cols = fcs_vars, description = "This household had a very high Food Consumption Score reported (> 60). This is improbable.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_hhs_nofoodhh_noeat", hhs_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_hhs_nofoodhh_noeat", cols = hhs_vars, description = "This household reported a most severe HHS behaviour, but not the less severe behaviours. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_severe_hhs", hhs_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_severe_hhs", cols = hhs_vars, description = "This household reported an HHS score of 5 or 6, 'Very Severe' . While not impossible, it should be followed up to make sure the enumerator is collecting correctly.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_low_hdds", hdds_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_low_hdds", cols = hdds_vars, description = "This household reported a very low HDDS score. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_high_hdds", hdds_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_high_hdds", cols = hdds_vars, description = "This household reported a very high HDDS score. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_low_sugar_oil_hdds", hdds_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_low_sugar_oil_hdds", cols = hdds_vars, description = "This household reported a very low HDDS score but also sugars and oil consumption. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_high_rcsi", rcsi_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_high_rcsi", cols = rcsi_vars, description = "This household reported a very high rCSI score. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_proteins_rcsi", c(rcsi_vars, "fcs_dairy", "fcs_meat")), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_proteins_rcsi", cols = c(rcsi_vars, "fcs_dairy", "fcs_meat"), description = "This household reported a very high rCSI score. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }
  if(length(setdiff(c("flag_lcs_severity", hdds_vars), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_lcs_severity", cols = lcs_vars, description = "This household reported an emergency livelihood coping strategy used or exhausted, but did not report any stress or crisis behaviours. While not impossible, it is unusual.")
    cl <- rbind(cl, cl2)
  }

  if(length(setdiff(c("flag_fc_cell", "fc_cell", "fc_phase"), names(df)))==0) {
    cl2 <- healthyr::cleaning_log_helper(df = df, uuid = uuid_col, flag = "flag_fc_cell", cols = c("fc_cell", "fc_phase"), description = "This household was classified in the FEWSNET Matrix either cells 3,4,5,8,9, or 10. These are logically inconsistent cells showing high HHS, moderate or high food consumption, but low food coping behaviours. ")
    cl <- rbind(cl, cl2)
  }

  # adding all text other responses for columns ending in '_other'
  if(length(grep(pattern = "_other", x = colnames(df), value = TRUE)) > 0) {
    cl2 <- healthyr::cleaning_log_helper_others(df = df, uuid = uuid_col)
    cl <- rbind(cl, cl2)
  }

  # vaccination indicator flags
  # coming shortly

  cl <- cl[-1,]
  cl <- cl %>% dplyr::filter(!is.na(.data$uuid)) %>% dplyr::arrange(.data$uuid)

  if(!is.null(file_path)) {writexl::write_xlsx(cl, file_path)}

  return(cl)

}
