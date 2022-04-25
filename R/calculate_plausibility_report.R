
#' Calculate Plausibility Report
#'
#' Calculates an approximation of the standard ENA plausibility scores and classification for anthropometric data.
#' Individually assess each key parameter, and if all parameters are present it will additionally create the
#' plausibility score and classification.
#'
#' @param df Inputs a dataframe that has been created from the create_anthro_summary_report function.
#'
#' @return Returns a dataframe with additional columns for plausibility penalty scores and classifications,
#' depending on the technical indicators that were in the original df.
#' @export
#'
#' @examples
#' \dontrun{calculate_plausibility_report(df)}
#' @importFrom rlang .data
calculate_plausibility_report <- function(df) {

  print("Now Calculating Plausibility Scoring and Classifications.")

  anthro_plaus_vars <- c("prop_smart_flags", "sd_wfhz_noflag", "age_ratio.pvalue", "sex_ratio.pvalue", "dps_weight", "dps_height", "dps_muac",
                         "skewness_wfhz", "kurtosis_wfhz", "poisson_pvalues.wfhz")

  # Anthropometric Plausibility Criteria
  if(c("prop_smart_flags") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_smart_flags = ifelse(.data$prop_smart_flags <=2.5, 0,
                                        ifelse(.data$prop_smart_flags <=5.0, 5,
                                               ifelse(.data$prop_smart_flags <=7.5, 10,
                                                      ifelse(.data$prop_smart_flags >7.5, 20, NA)))))
  }
  if(c("sd_wfhz_noflag") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_sd_whz = ifelse(.data$sd_wfhz_noflag <1.1 & .data$sd_wfhz_noflag >0.9, 0,
                                   ifelse(.data$sd_wfhz_noflag <1.15 & .data$sd_wfhz_noflag >0.85, 5,
                                          ifelse(.data$sd_wfhz_noflag <1.2 & .data$sd_wfhz_noflag >0.8, 10,
                                                 ifelse(.data$sd_wfhz_noflag >=1.2 | .data$sd_wfhz_noflag <=0.8, 20, NA)))))
  }
  if(c("age_ratio.pvalue") %in% names(df)) {
    df <- df %>%
      dplyr:: mutate(plaus_ageratio = ifelse(.data$age_ratio.pvalue > 0.1, 0,
                                     ifelse(.data$age_ratio.pvalue >0.05, 2,
                                            ifelse(.data$age_ratio.pvalue >0.001, 4,
                                                   ifelse(.data$age_ratio.pvalue<=0.001, 10, NA)))))
  }
  if( (c("sex_ratio.pvalue") %in% names(df)) & !(c("cdr") %in% names(df)) ) {
    df <- df %>%
      dplyr::mutate(plaus_sexratio = ifelse(.data$sex_ratio.pvalue >0.1, 0,
                                     ifelse(.data$sex_ratio.pvalue >0.05, 2,
                                            ifelse(.data$sex_ratio.pvalue >=0.001, 4,
                                                   ifelse(.data$sex_ratio.pvalue <=0.001, 10, NA)))))
  }
  if(c("dps_weight") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_dps_weight = ifelse(.data$dps_weight >= 0 & .data$dps_weight < 8, 0,
                                       ifelse(.data$dps_weight >= 8 & .data$dps_weight <13, 2,
                                              ifelse(.data$dps_weight >= 13 & .data$dps_weight <20, 4,
                                                     ifelse(.data$dps_weight >=20, 10, NA)))))
  }
  if(c("dps_height") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_dps_height = ifelse(.data$dps_height >= 0 & .data$dps_height < 8, 0,
                                       ifelse(.data$dps_height >= 8 & .data$dps_height < 13, 2,
                                              ifelse(.data$dps_height >= 13 & .data$dps_height <20, 4,
                                                     ifelse(.data$dps_height >=20, 10, NA)))))
  }
  if(c("dps_muac") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_dps_muac = ifelse(.data$dps_muac >= 0 & .data$dps_muac < 8, 0,
                                     ifelse(.data$dps_muac >= 8 & .data$dps_muac < 13, 2,
                                            ifelse(.data$dps_muac >= 13 & .data$dps_muac <20, 4,
                                                   ifelse(.data$dps_muac >=20, 10, NA)))))
  }
  if(c("skewness_wfhz") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_skewness = ifelse(.data$skewness_wfhz <0.2, 0,
                                     ifelse(.data$skewness_wfhz <0.4, 1,
                                            ifelse(.data$skewness_wfhz <0.6, 3,
                                                   ifelse(.data$skewness_wfhz >=0.6, 5, NA)))))
  }
  if(c("kurtosis_wfhz") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_kurtosis = ifelse(.data$kurtosis_wfhz <0.2, 0,
                                     ifelse(.data$kurtosis_wfhz <0.4, 1,
                                            ifelse(.data$kurtosis_wfhz <0.6, 3,
                                                   ifelse(.data$kurtosis_wfhz >=0.6, 5, NA)))))
  }
  if(c("poisson_pvalues.wfhz") %in% names(df)) {
    df <- df %>%
      dplyr::mutate(plaus_poisson = ifelse(.data$poisson_pvalues.wfhz >0.05,0,
                                    ifelse(.data$poisson_pvalues.wfhz >0.01, 1,
                                           ifelse(.data$poisson_pvalues.wfhz >0.001, 3,
                                                  ifelse(.data$poisson_pvalues.wfhz <=0.001, 5, NA)))))
  }

  # Anthropometric Plausibility Score and Classification
  if(length(setdiff(anthro_plaus_vars, names(df)))==0) {
    print("Generating anthropometric plausibility score and classification.")
    df <- df %>%
      dplyr::mutate(anthro_plaus_score = .data$plaus_smart_flags + .data$plaus_sd_whz + .data$plaus_ageratio + .data$plaus_sexratio + .data$plaus_dps_height + .data$plaus_dps_weight + .data$plaus_dps_muac + .data$plaus_skewness + .data$plaus_kurtosis + .data$plaus_poisson,
             anthro_plaus_cat = ifelse(.data$anthro_plaus_score >=0 & .data$anthro_plaus_score <=9, "Excellent (0-9)",
                                       ifelse(.data$anthro_plaus_score >9 & .data$anthro_plaus_score <=14, "Good (10-14)",
                                              ifelse(.data$anthro_plaus_score >14 & .data$anthro_plaus_score <25, "Acceptable (15-24)",
                                                     ifelse(.data$anthro_plaus_score >=25, "Problematic (>=25)", NA)))))

  } else {
    print(paste0("Not all necessary variables for anthropometric plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(anthro_plaus_vars, names(df)))
  }

  # Mortality Plausibility Criteria

  mort_plaus_vars <- c("plaus_cdr", "plaus_prop_hh_flag_deaths", "plaus_sex_ratio.pvalue", "plaus_age_ratio_0_5.pvalue", "plaus_age_ratio_2_5.pvalue", "plaus_age_ratio_5_10.pvalue",
                       "plaus_mean_hh_size.pvalue", "plaus_prop_joiners", "plaus_prop_leavers", "plaus_poisson_pvalues.deaths")

  if(c("cdr") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_cdr = ifelse(.data$cdr < 1, 0,
                                ifelse(.data$cdr <2, 5,
                                       ifelse(.data$cdr <3.5, 10,
                                              ifelse(.data$cdr >= 3.5, 20, 0)))))

  }

  if(c("prop_hh_flag_deaths") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_prop_hh_flag_deaths = ifelse(.data$prop_hh_flag_deaths < 0.005, 0,
                                                ifelse(.data$prop_hh_flag_deaths < 0.01, 2,
                                                       ifelse(.data$prop_hh_flag_deaths < 0.015, 5,
                                                              ifelse(.data$prop_hh_flag_deaths >= 1.5, 10, 0)))))

  }

  if(length(setdiff(c("sex_ratio.pvalue", "cdr"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_sex_ratio.pvalue = ifelse(.data$sex_ratio.pvalue > 0.05, 0,
                                             ifelse(.data$sex_ratio.pvalue > 0.001, 2,
                                                    ifelse(.data$sex_ratio.pvalue > 0.0001, 5,
                                                           ifelse(.data$sex_ratio.pvalue <= 0.0001, 10, 0)))))

  }

  if(c("age_ratio_0_5.pvalue") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_age_ratio_0_5.pvalue = ifelse(.data$age_ratio_0_5.pvalue > 0.1, 0,
                                                 ifelse(.data$age_ratio_0_5.pvalue > 0.05, 2,
                                                        ifelse(.data$age_ratio_0_5.pvalue > 0.001, 5,
                                                               ifelse(.data$age_ratio_0_5.pvalue <= 0.001, 10, 0)))))

  }

  if(c("age_ratio_2_5.pvalue") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_age_ratio_2_5.pvalue = ifelse(.data$age_ratio_2_5.pvalue > 0.1, 0,
                                                 ifelse(.data$age_ratio_2_5.pvalue > 0.05, 2,
                                                        ifelse(.data$age_ratio_2_5.pvalue > 0.001, 5,
                                                               ifelse(.data$age_ratio_2_5.pvalue <= 0.001, 10, 0)))))

  }

  if(c("age_ratio_5_10.pvalue") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_age_ratio_5_10.pvalue = ifelse(.data$age_ratio_5_10.pvalue > 0.1, 0,
                                                  ifelse(.data$age_ratio_5_10.pvalue > 0.05, 2,
                                                         ifelse(.data$age_ratio_5_10.pvalue > 0.001, 5,
                                                                ifelse(.data$age_ratio_5_10.pvalue <= 0.001, 10, 0)))))

  }

  if(c("mean_hh_size.pvalue") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_mean_hh_size.pvalue = ifelse(.data$mean_hh_size.pvalue > 0.05, 0,
                                                ifelse(.data$mean_hh_size.pvalue > 0.001, 2,
                                                       ifelse(.data$mean_hh_size.pvalue > 0.0001, 5,
                                                              ifelse(.data$mean_hh_size.pvalue <= 0.0001, 10, 0)))))

  }

  if(c("prop_join_people") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_prop_joiners = ifelse(.data$prop_join_people < 10, 0,
                                         ifelse(.data$prop_join_people < 20, 2,
                                                ifelse(.data$prop_join_people < 30, 5,
                                                       ifelse(.data$prop_join_people >= 30, 10, 0)))))

  }

  if(c("prop_left_people") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_prop_leavers = ifelse(.data$prop_left_people < 10, 0,
                                         ifelse(.data$prop_left_people < 20, 2,
                                                ifelse(.data$prop_left_people < 30, 5,
                                                       ifelse(.data$prop_left_people >= 30, 10, 0)))))

  }

  if(length(setdiff(c("poisson_pvalues.deaths"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_poisson_pvalues.deaths = ifelse(.data$poisson_pvalues.deaths > 0.1, 0,
                                                   ifelse(.data$poisson_pvalues.deaths > 0.05, 2,
                                                          ifelse(.data$poisson_pvalues.deaths > 0.001, 5,
                                                                 ifelse(.data$poisson_pvalues.deaths <= 0.001, 10, 0)))))
  }

  # Mortality Plausibility Score and Classification

  if(length(setdiff(mort_plaus_vars, names(df)))==0) {

    df <- df %>%
      dplyr::mutate(mort_plaus_score = .data$plaus_cdr + .data$plaus_prop_hh_flag_deaths + .data$plaus_sex_ratio.pvalue + .data$plaus_age_ratio_0_5.pvalue + .data$plaus_age_ratio_2_5.pvalue + .data$plaus_age_ratio_5_10.pvalue + .data$plaus_mean_hh_size.pvalue + .data$plaus_prop_joiners + .data$plaus_prop_leavers + .data$plaus_poisson_pvalues.deaths,
             mort_plaus_cat = ifelse(.data$mort_plaus_score >=0 & .data$mort_plaus_score < 10, "Excellent (0-<10)",
                                     ifelse(.data$mort_plaus_score >= 10 & .data$mort_plaus_score < 20, "Good (10-<20)",
                                            ifelse(.data$mort_plaus_score >= 20 & .data$mort_plaus_score < 25, "Acceptable (20 - <25)",
                                                   ifelse(.data$mort_plaus_score >= 25, "Problematic (>=25)", NA)))))

  } else {
    print(paste0("Not all necessary variables for mortality plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(mort_plaus_vars, names(df)))
  }

  # Food Security Plausibility Criteria

  if(c("prop_fc_flags") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_prop_fc_flags = ifelse(.data$prop_fc_flags < 0.02, 0,
                                          ifelse(.data$prop_fc_flags <0.04, 5,
                                                 ifelse(.data$prop_fc_flags <0.1, 10,
                                                        ifelse(.data$prop_fc_flags >= 0.10, 20, 0)))))
  }
  if(c("flag_lcs_severity") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_lcs_severity = ifelse(.data$flag_lcs_severity < 2, 0,
                                              ifelse(.data$flag_lcs_severity < 4, 5,
                                                     ifelse(.data$flag_lcs_severity < 10, 10,
                                                            ifelse(.data$flag_lcs_severity >= 10, 20, 0)))))
  }
  if(c("sd_fcs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_sd_fcs = ifelse(.data$sd_fcs >= 9 & .data$sd_fcs < 14, 0,
                                   ifelse(.data$sd_fcs >= 8 & .data$sd_fcs < 16, 5,
                                          ifelse(.data$sd_fcs < 8 | .data$sd_fcs >= 16, 10, 0))))
  }
  if(c("flag_high_rcsi") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_high_rcsi = ifelse(.data$flag_high_rcsi < 1, 0,
                                           ifelse(.data$flag_high_rcsi < 2, 2,
                                                  ifelse(.data$flag_high_rcsi < 5, 4,
                                                         ifelse(.data$flag_high_rcsi >= 5, 10, 0)))))
  }
  if(c("flag_severe_hhs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_severe_hhs = ifelse(.data$flag_severe_hhs < 1, 0,
                                            ifelse(.data$flag_severe_hhs < 5, 2,
                                                   ifelse(.data$flag_severe_hhs < 10, 4,
                                                          ifelse(.data$flag_severe_hhs >= 10, 10, 0)))))
  }
  if(length(setdiff(c("corr.fcs_rcsi", "corr.fcs_rcsi.pvalue"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_corr.fcs_rcsi = ifelse(.data$corr.fcs_rcsi < -0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 0,
                                                 ifelse(.data$corr.fcs_rcsi < -0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 3,
                                                        ifelse(.data$corr.fcs_rcsi >= -0.2 & .data$corr.fcs_rcsi < 0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 5,
                                                               ifelse(.data$corr.fcs_rcsi >= -0.2 & .data$corr.fcs_rcsi < 0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 10,
                                                                      ifelse(.data$corr.fcs_rcsi >= 0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 12,
                                                                             ifelse(.data$corr.fcs_rcsi >= 0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 15, 0)))))))
  }
  if(length(setdiff(c("corr.fcs_hhs", "corr.fcs_hhs.pvalue"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_corr.fcs_hhs = ifelse(.data$corr.fcs_hhs < -0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 0,
                                                ifelse(.data$corr.fcs_hhs < -0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 1,
                                                       ifelse(.data$corr.fcs_hhs >= -0.2 & .data$corr.fcs_hhs < 0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 2,
                                                              ifelse(.data$corr.fcs_hhs >= -0.2 & .data$corr.fcs_hhs < 0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 3,
                                                                     ifelse(.data$corr.fcs_hhs >= 0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 4,
                                                                            ifelse(.data$corr.fcs_hhs >= 0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 5, 0)))))))
  }
  if(length(setdiff(c("corr.hhs_rcsi", "corr.hhs_rcsi.pvalue"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_corr.hhs_rcsi = ifelse(.data$corr.hhs_rcsi > 0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 0,
                                                 ifelse(.data$corr.hhs_rcsi > 0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 1,
                                                        ifelse(.data$corr.hhs_rcsi > -0.2 & .data$corr.hhs_rcsi <= 0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 2,
                                                               ifelse(.data$corr.hhs_rcsi > -0.2 & .data$corr.hhs_rcsi <= 0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 3,
                                                                      ifelse(.data$corr.hhs_rcsi <= -0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 4,
                                                                             ifelse(.data$corr.hhs_rcsi <= -0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 5, 0)))))))
  }
  if(length(setdiff(c("poisson_pvalues.hhs_very_severe"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_poisson.hhs = ifelse(is.na(.data$poisson_pvalues.hhs_very_severe), 0 ,
                                        ifelse(.data$poisson_pvalues.hhs_very_severe >= 0.05, 0,
                                               ifelse(.data$poisson_pvalues.hhs_very_severe >= 0.01 & .data$poisson_pvalues.hhs_very_severe < 0.05, 1,
                                                      ifelse(.data$poisson_pvalues.hhs_very_severe >= 0.001 & .data$poisson_pvalues.hhs_very_severe < 0.01, 3,
                                                             ifelse(.data$poisson_pvalues.hhs_very_severe < 0.001, 5, 0))))))
  }

  # Food Security Plausibility Score and Classification

  fsl_plaus_vars <- c("prop_fc_flags", "flag_lcs_severity", "sd_fcs", "flag_high_rcsi", "flag_severe_hhs", "corr.fcs_rcsi", "corr.fcs_rcsi.pvalue",
                      "corr.fcs_hhs", "corr.fcs_hhs.pvalue", "corr.hhs_rcsi", "corr.hhs_rcsi.pvalue", "poisson_pvalues.hhs_very_severe")

  if(length(setdiff(fsl_plaus_vars, names(df)))==0) {

    df <- df %>%
      dplyr::mutate(fsl_plaus_score = .data$plaus_prop_fc_flags + .data$plaus_flag_lcs_severity + .data$plaus_sd_fcs + .data$plaus_flag_high_rcsi + .data$plaus_flag_severe_hhs + .data$plaus_corr.fcs_rcsi + .data$plaus_corr.fcs_hhs + .data$plaus_corr.hhs_rcsi + .data$plaus_poisson.hhs,
             fsl_plaus_cat = ifelse(.data$fsl_plaus_score >=0 & .data$fsl_plaus_score < 10, "Excellent (0-<10)",
                                    ifelse(.data$fsl_plaus_score >= 10 & .data$fsl_plaus_score < 20, "Good (10-<20)",
                                           ifelse(.data$fsl_plaus_score >= 20 & .data$fsl_plaus_score < 30, "Acceptable (20 - <30)",
                                                  ifelse(.data$fsl_plaus_score >= 30, "Problematic (>=30)", NA)))))

  } else {
    print(paste0("Not all necessary variables for FSL plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(fsl_plaus_vars, names(df)))
  }

  # IYCF Plausibility Criteria

  if(c("mad_ratio.pvalue") %in% colnames(df)) {

    df <- df %>%
      dplyr::mutate(plaus_mad_ratio.pvalue = ifelse(.data$mad_ratio.pvalue > 0.05, 0,
                                             ifelse(.data$mad_ratio.pvalue > 0.001, 5,
                                                    ifelse(.data$mad_ratio.pvalue > 0.0001, 10,
                                                           ifelse(.data$mad_ratio.pvalue <= 0.0001, 20, 0)))))

  }

  if(c("prop_flag_high_mdd_low_mmf") %in% colnames(df)) {

    df <- df %>%
      dplyr::mutate(plaus_prop_flag_high_mdd_low_mmf = ifelse(.data$prop_flag_high_mdd_low_mmf < 0.01, 0,
                                                       ifelse(.data$prop_flag_high_mdd_low_mmf <0.05, 5,
                                                              ifelse(.data$prop_flag_high_mdd_low_mmf <0.1, 10,
                                                                     ifelse(.data$prop_flag_high_mdd_low_mmf >= 0.1, 20, 0)))))

  }

  # if(length(setdiff(c("sex_ratio.pvalue", "mad_ratio.pvalue"), names(df)))==0) {
  #
  #   df <- df %>%
  #     dplyr::mutate(plaus_sex_ratio.pvalue = ifelse(.data$sex_ratio.pvalue > 0.05, 0,
  #                                            ifelse(.data$sex_ratio.pvalue > 0.01, 2,
  #                                                   ifelse(.data$sex_ratio.pvalue > 0.001, 5,
  #                                                          ifelse(.data$sex_ratio.pvalue <= 0.001, 10, 0)))))
  #
  # }

  if(c("age_ratio_under6m_6to23m.pvalue") %in% colnames(df)) {

    df <- df %>%
      dplyr::mutate(plaus_age_ratio_under6m_6to23m.pvalue = ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.05, 0,
                                                            ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.01, 2,
                                                                   ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.001, 5,
                                                                          ifelse(.data$age_ratio_under6m_6to23m.pvalue <= 0.001, 10, 0)))))


  }

  if(c("sd_mdd") %in% colnames(df)) {

    df <- df %>%
      dplyr::mutate(plaus_sdd_mdd = ifelse(.data$sd_mdd > 1 & .data$sd_mdd < 2, 0,
                                    ifelse(.data$sd_mdd > 0.8 & .data$sd_mdd < 2.2, 5,
                                           ifelse(.data$sd_mdd <= 0.8 | .data$sd_mdd >= 2.2, 10, 0))))
  }

  if(c("prop_iycf_caregiver" %in% colnames(df))) {

    df <- df %>%
      dplyr::mutate(plaus_prop_iycf_caregiver = ifelse(.data$prop_iycf_caregiver >= 0.9, 0,
                                                ifelse(.data$prop_iycf_caregiver >= .8, 2,
                                                       ifelse(.data$prop_iycf_caregiver >= 0.7, 5,
                                                              ifelse(.data$prop_iycf_caregiver >=0.5, 10, 10)))))

  }

  # IYCF Plausibility Score and Classification

  iycf_plaus_vars <- c("plaus_sdd_mdd", "plaus_age_ratio_under6m_6to23m.pvalue", "plaus_sexratio",
                       "plaus_prop_flag_high_mdd_low_mmf", "plaus_mad_ratio.pvalue")

  if(length(setdiff(iycf_plaus_vars, colnames(df)))==0) {

    if(!(c("plaus_prop_iycf_caregiver") %in% colnames(df))) {
      df <- df %>% dplyr::mutate(plaus_prop_iycf_caregiver = 10)
      print("No iycf_caregiver variable was available. Plaus penalty of 10 applied assuming this wasn't done during the survey.")
    }

    df <- df %>%
      dplyr::mutate(iycf_plaus_score = .data$plaus_prop_iycf_caregiver + .data$plaus_sdd_mdd + .data$plaus_age_ratio_under6m_6to23m.pvalue + .data$plaus_sexratio + .data$plaus_prop_flag_high_mdd_low_mmf + .data$plaus_mad_ratio.pvalue,
             iycf_plaus_cat = ifelse(.data$iycf_plaus_score >=0 & .data$iycf_plaus_score < 10, "Excellent (0-<10)",
                                     ifelse(.data$iycf_plaus_score >= 10 & .data$iycf_plaus_score < 15, "Good (10-<15)",
                                            ifelse(.data$iycf_plaus_score >= 15 & .data$iycf_plaus_score < 25, "Acceptable (15 - <25)",
                                                   ifelse(.data$iycf_plaus_score >= 25, "Problematic (>=25)", NA)))))


  } else {
    print(paste0("Not all necessary variables for IYCF plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(iycf_plaus_vars, names(df)))
    print(setdiff(c("prop_iycf_caregiver"), names(df)))
  }

  return(df)
}
