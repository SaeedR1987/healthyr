
#' Flag Nutrition Health Issues
#'
#' @param df Inputs a dataframe which has already been processed by the format_nut_health_indicators, reformat_nut_health_indicators, and
#' calculate_nut_health_indicators functions. This function is not intended to be run directly, and instead is called automatically
#' within the format_nut_health_indicators function.
#' @param use_flags Optional input to specify if IYCF flags should be automatically blanked in the numerator.
#'
#' @return Returns a dataframe with additional columns of flags/data quality checks for individual records.
#' @export
#'
#' @examples
#' \dontrun{flag_nut_health_issues(df)}
#'
#' @importFrom rlang .data
flag_nut_health_issues <- function(df, use_flags = NULL) {

    # Washington Group Flags

  if(length(setdiff(c("wgss_sco_score", "age_years"), names(df)))==0) {
    df <- df %>%
      dplyr::mutate(flag_wgss_age = ifelse(is.na(.data$wgss_sco_score), NA, ifelse(.data$age_years <5, 1, 0)))
  }
  if(c("sex") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(flag_missing_sex = ifelse(is.na(.data$sex), 1, 0))

  }
  if(c("age_years") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(flag_missing_age_years = ifelse(is.na(.data$age_years), 1, 0))
  }
  if(c("wgss_sco_score") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(flag_wgss_sco_score = ifelse(is.na(.data$wgss_sco_score), NA, ifelse(.data$wgss_sco_score >= 20, 1, 0)))

  }
  if(c("wgss_hd_score") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(flag_wgss_extreme = ifelse(is.na(.data$wgss_hd_score), NA, ifelse(.data$wgss_hd_score >= 9, 1, 0)))

  }

  # IYCF flags

  liquids <- c("iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j")
  liquids_to_check <- intersect(liquids, colnames(df))

  foods <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  foods_to_check <- intersect(foods, colnames(df))

  if(length(foods_to_check) > 0) {

    df$foods_all_same <- apply(df[foods_to_check], 1, function(x) length(unique(x)) == 1)
    df$foods_all_no <- apply(df[,foods_to_check]==2, 1, all)
    df$foods_all_yes <- apply(df[,foods_to_check]==1, 1, all)

    df <- df %>%
      dplyr::mutate(flag_yes_foods = ifelse(.data$foods_all_yes == TRUE, 1, 0))



  }
  if(length(liquids_to_check) > 0) {

    df$liquids_all_same <- apply(df[liquids_to_check], 1, function(x) length(unique(x)) == 1)
    df$liquids_all_no <- apply(df[,liquids_to_check]==2, 1, all)
    df$liquids_all_yes <- apply(df[,liquids_to_check]==1, 1, all)

    df <- df %>%
      dplyr::mutate(flag_yes_liquids = ifelse(.data$liquids_all_yes == TRUE, 1, 0))


  }
  if(length(setdiff(c("iycf_4", "age_months"), colnames(df)))==0 & length(foods_to_check) > 0 & length(liquids_to_check) > 0) {

    df <- df %>%
      dplyr::mutate(flag_no_anything = ifelse(is.na(.data$iycf_4), NA, ifelse(.data$iycf_4 != 1 & .data$foods_all_no == TRUE & .data$liquids_all_no == TRUE & .data$age_months > 5, 1, 0)))

  }
  if(length(setdiff(c("iycf_8", "age_months"), colnames(df)))==0 & length(foods_to_check) > 0) {

    df <- df %>%
      dplyr::mutate(flag_no_foods = ifelse(.data$foods_all_no == TRUE & .data$iycf_8 > 0 & .data$age_months > 5, 1, 0),
             flag_all_foods_no_meal = ifelse(.data$foods_all_yes == TRUE & .data$iycf_8 == 0, 1, 0),
             flag_some_foods_no_meal = ifelse(.data$foods_all_no == FALSE & .data$iycf_8 == 0 & .data$age_months > 5, 1, 0))

  }

  if(length(setdiff(c("iycf_mdd_score", "iycf_8"), colnames(df)))==0) {
    df <- df %>%
      dplyr::mutate(flag_high_mdd_low_mmf = ifelse(is.na(.data$iycf_mdd_score), NA,
                                            ifelse(is.na(.data$iycf_8), NA,
                                                   ifelse(.data$iycf_mdd_score >= 6 & .data$iycf_8 <= 1, 1, 0))))


  }

  if(length(setdiff(c("age_months", "iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num"), colnames(df)))==0) {

    df <- df %>%
      dplyr::mutate(flag_under6_nobf_nomilk = ifelse(is.na(.data$age_months), NA,
                                              ifelse(is.na(.data$iycf_4), NA,
                                                     ifelse(is.na(.data$iycf_6b_num), NA,
                                                            ifelse(is.na(.data$iycf_6c_num), NA,
                                                                   ifelse(is.na(.data$iycf_6d_num), NA,
                                                                          ifelse(.data$age_months < 6 & .data$iycf_4 != 1 & .data$iycf_6b_num == 0 & .data$iycf_6c_num == 0 & .data$iycf_6d_num == 0, 1, 0)))))))



  }

  if(length(setdiff(c("iycf_7b", "iycf_7d", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m"), colnames(df)))==0) {

    df <- df %>%
      dplyr::mutate(flag_meats_nostaples = ifelse(is.na(.data$iycf_7b), NA,
                                           ifelse(is.na(.data$iycf_7d), NA,
                                                  ifelse(is.na(.data$iycf_7i), NA,
                                                         ifelse(is.na(.data$iycf_7j), NA,
                                                                ifelse(is.na(.data$iycf_7k), NA,
                                                                       ifelse(is.na(.data$iycf_7l), NA,
                                                                              ifelse(is.na(.data$iycf_7m), NA,
                                                                                     ifelse( .data$iycf_7b != 1 & .data$iycf_7d != 1 & (.data$iycf_7i == 1 | .data$iycf_7j == 1 | .data$iycf_7k == 1 | .data$iycf_7l == 1 | .data$iycf_7m == 1), 1 , 0 )))))))))

  }


  if(use_flags == "yes" & (c("iycf_ufc") %in% colnames(df))) {
    df <- df %>% dplyr::mutate(iycf_ufc = ifelse(is.na(.data$flag_yes_foods), .data$iycf_ufc, ifelse(.data$flag_yes_foods == 1, 0, .data$iycf_ufc)))
  }

  if(use_flags == "yes" & (c("iycf_zvf") %in% colnames(df))) {
    df <- df %>% dplyr::mutate(iycf_zvf = ifelse(is.na(.data$flag_no_foods), .data$iycf_zvf, ifelse(.data$flag_no_foods == 1, 0, .data$iycf_zvf)),
                               iycf_zvf = ifelse(is.na(.data$flag_yes_foods), .data$iycf_zvf, ifelse(.data$flag_yes_foods == 1, 0, .data$iycf_zvf)))
  }

  if(use_flags == "yes" & (c("iycf_eff") %in% colnames(df))) {
    df <- df %>% dplyr::mutate(iycf_eff = ifelse(is.na(.data$flag_no_foods), .data$iycf_eff, ifelse(.data$flag_no_foods == 1, 0 , .data$iycf_eff)),
                               iycf_eff = ifelse(is.na(.data$flag_yes_foods), .data$iycf_eff, ifelse(.data$flag_yes_foods == 1, 0, .data$iycf_eff)))
  }

  if(use_flags == "yes" & (c("iycf_ebf") %in% colnames(df))) {
    df <- df %>% dplyr::mutate(iycf_ebf = ifelse(is.na(.data$flag_yes_liquids), .data$iycf_ebf, ifelse(.data$flag_yes_liquids == 1, 0, .data$iycf_ebf)))
    }

  if(use_flags == "yes" & (c("iycf_swb") %in% colnames(df))) {
    df <- df %>% dplyr::mutate(iycf_swb = ifelse(is.na(.data$flag_yes_liquids), .data$iycf_swb, ifelse(.data$flag_yes_liquids == 1, 0, .data$iycf_swb)))
  }

  if(use_flags == "yes" & (c("iycf_mad") %in% colnames(df))) {
    df <- df %>%
      dplyr::mutate(iycf_mad = ifelse(is.na(.data$flag_no_foods), .data$iycf_mad, ifelse(.data$flag_no_foods == 1, 0, .data$iycf_mad)),
                    iycf_mad = ifelse(is.na(.data$flag_yes_foods), .data$iycf_mad, ifelse(.data$flag_yes_foods == 1, 0, .data$iycf_mad)),
                    iycf_mad = ifelse(is.na(.data$flag_some_foods_no_meal), .data$iycf_mad, ifelse(.data$flag_some_foods_no_meal == 1, 0, .data$iycf_mad)))
  }

  if(use_flags == "yes" & (c("iycf_mdd_score") %in% colnames(df))) {
    df <- df %>%
      dplyr::mutate(iycf_mdd_score = ifelse(is.na(.data$flag_yes_foods), .data$iycf_mdd_score, ifelse(.data$flag_yes_foods == 1, NA, .data$iycf_mdd_score)),
                    iycf_mdd_score = ifelse(is.na(.data$flag_no_foods), .data$iycf_mdd_score, ifelse(.data$flag_no_foods == 1, NA, .data$iycf_mdd_score)))
  }

  if(use_flags == "yes" & (c("iycf_mdd_cat") %in% colnames(df))) {
    df <- df %>%
      dplyr::mutate(iycf_mdd_cat = ifelse(is.na(.data$flag_no_foods), .data$iycf_mdd_cat, ifelse(.data$flag_no_foods == 1, 0, .data$iycf_mdd_cat)),
                    iycf_mdd_cat = ifelse(is.na(.data$flag_yes_foods), .data$iycf_mdd_cat, ifelse(.data$flag_yes_foods == 1, 0, .data$iycf_mdd_cat)))
  }

  if(use_flags == "yes" & (c("iycf_mmf") %in% colnames(df))) {
    df <- df %>%
      dplyr::mutate(iycf_mmf = ifelse(is.na(.data$flag_no_foods), .data$iycf_mmf, ifelse(.data$flag_no_foods == 1, 0, .data$iycf_mmf)),
                    iycf_mmf = ifelse(is.na(.data$flag_yes_foods), .data$iycf_mmf, ifelse(.data$flag_yes_foods == 1, 0, .data$iycf_mmf)),
                    iycf_mmf = ifelse(is.na(.data$flag_some_foods_no_meal), .data$iycf_mmf, ifelse(.data$flag_some_foods_no_meal == 1, 0, .data$iycf_mmf)))
  }



  # FSL flags

  fcs_vars <- c("fcs_cereal", "fcs_legumes", "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", "fcs_oil", "fcs_sugar")
  if(length(setdiff(fcs_vars, colnames(df)))==0) {

    # All 0s or 7s for FCS vars
    # Much higher meats than cereals
    df <- df %>%
      dplyr::mutate(flag_above7_fcs = ifelse(is.na(.data$fcs_cereal), NA, ifelse(.data$fcs_cereal > 7 | .data$fcs_legumes  > 7 | .data$fcs_dairy  > 7 | .data$fcs_meat  > 7 | .data$fcs_veg  > 7 | .data$fcs_fruit  > 7 | .data$fcs_oil  > 7 | .data$fcs_sugar > 7, 1, 0)),
                        flag_meat_cereal_ratio = ifelse(is.na(.data$fcs_cereal), NA, ifelse(as.numeric(.data$fcs_meat) - as.numeric(.data$fcs_cereal) > 0, 1, 0 )),
                        flag_zero_fcs = ifelse(is.na(.data$fcs_cereal), NA, ifelse(.data$fcs_cereal == 0 & .data$fcs_legumes == 0 & .data$fcs_dairy == 0 & .data$fcs_meat == 0 & .data$fcs_veg == 0 & .data$fcs_fruit == 0 & .data$fcs_oil == 0 & .data$fcs_sugar == 0, 1, 0)),
                        flag_all_fcs = ifelse(is.na(.data$fcs_cereal), NA, ifelse(.data$fcs_cereal == 7 & .data$fcs_legumes == 7 & .data$fcs_dairy == 7 & .data$fcs_meat == 7 & .data$fcs_veg == 7 & .data$fcs_fruit == 7 & .data$fcs_oil == 7 & .data$fcs_sugar == 7, 1, 0)),
    )

    # Extreme FCS_score ( <3 or >60 )
    if(c("fcs_score") %in% colnames(df)) {

      df <- df %>%
        dplyr::mutate(flag_low_fcs = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score < 5, 1, 0)),
               flag_high_fcs = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score > 60, 1, 0)))

    }

    df <- df %>%
      dplyr::mutate(flag_fcs_extreme = ifelse(is.na(.data$fcs_score), NA, ifelse(.data$fcs_score < 3 | .data$fcs_score > 60, 1, 0)))

  }

  # HHS flags

  hhs_vars <- c("hhs_nofoodhh_1", "hhs_nofoodhh_1a", "hhs_sleephungry_2", "hhs_sleephungry_2a", "hhs_alldaynight_3", "hhs_alldaynight_3a")


  if(length(setdiff(hhs_vars, colnames(df)))==0) {
    # All day night without food, but no food in the household issue.

    if(length(setdiff(c("hhs_comp1", "hhs_comp3"), colnames(df)))==0) {
      df <- df %>%
        dplyr::mutate(flag_hhs_nofoodhh_noeat = ifelse(is.na(.data$hhs_comp1), NA, ifelse(.data$hhs_comp1 == 0 & .data$hhs_comp3 > 0, 1, 0 )))

    }
    # Very Severe HHS scores
    if(c("hhs_score") %in% colnames(df)) {

      df <- df %>%
        dplyr::mutate(flag_severe_hhs = ifelse(is.na(.data$hhs_score), NA, ifelse(.data$hhs_score >= 5, 1, 0)))

    }

  }

  # HDDS Flags

  hdds_vars <- c("hdds_cereals", "hdds_tubers", "hdds_veg", "hdds_fruit", "hdds_meat", "hdds_eggs", "hdds_fish", "hdds_legumes",
                 "hdds_dairy", "hdds_oils", "hdds_sugars", "hdds_condiments", "hdds_score")

  if(length(setdiff(hdds_vars, colnames(df)))==0) {

    if(c("hdds_score") %in% colnames(df))

      df <- df %>%
        dplyr::mutate(flag_low_hdds = ifelse(is.na(.data$hdds_score), NA, ifelse(.data$hdds_score <2, 1, 0)),
               flag_high_hdds = ifelse(is.na(.data$hdds_score), NA, ifelse(.data$hdds_score >9, 1, 0)),
               flag_low_sugar_oil_hdds = ifelse(is.na(.data$hdds_score), NA, ifelse( (.data$hdds_score <= 2 & .data$hdds_sugars == 1 & .data$hdds_condiments == 1) | (.data$hdds_score <= 1 & .data$hdds_sugars == 1) | (.data$hdds_score <= 1 & .data$hdds_condiments == 1), 1, 0)))

  }

  # rCSI Flags

  rcsi_vars <- c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5", "rcsi_score")

  if(length(setdiff(rcsi_vars, colnames(df)))==0) {

    df <- df %>%
      dplyr::mutate(flag_high_rcsi = ifelse(is.na(.data$rcsi_score), NA, ifelse(.data$rcsi_score >= 50, 1, 0)))

  }

  # LCS flags

  lcs_vars <- c("lcs_crisis", "lcs_emergency", "lcs_stress")

  if(length(setdiff(lcs_vars, colnames(df)))==0) {

    df <- df %>%
      dplyr::mutate(flag_lcs_severity = ifelse(is.na(.data$lcs_emergency), NA, ifelse( (.data$lcs_emergency == 1 & .data$lcs_stress == 0) | (.data$lcs_emergency == 1 & .data$lcs_crisis == 0) | (.data$lcs_crisis == 1 & .data$lcs_stress == 0), 1, 0)))

  }

  # FEWS NET flag

  if(c("fc_cell") %in% names(df)) {

    # Very few households (<0.1%) fall into cells 3, 4, 5, 8, 9, and 10.These represent cells which reflect very low levels of consumption coping and borderline or better food consumption, but Moderate/Severe household hunger – a
    # FEWS NET Integrated analysis of survey-based indicators for classification of acute food insecurity May 2021
    # Famine Early Warning Systems Network 12
    # combination of indicators that seems implausible. If a significant number of households did report this combination of responses, it could raise questions about data quality and would, at a minimum, suggest the need for further enquiry.

    df <- df %>%
      dplyr::mutate(flag_fc_cell = ifelse(is.na(.data$fc_cell), NA, ifelse(.data$fc_cell == 3 | .data$fc_cell == 4 | .data$fc_cell == 5 | .data$fc_cell == 8 | .data$fc_cell == 9 | .data$fc_cell == 10, 1, 0)))

  }

  # Vaccination Flags
  # to be added

  # Health Expenditure Flags
  # to be added

  return(df)

}
