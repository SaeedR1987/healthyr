
#' Calculate Plausibility Report
#'
#' Calculates an approximation of the standard ENA plausibility scores and classification for anthropometric data.
#' Individually assess each key parameter, and if all parameters are present it will additionally create the
#' plausibility score and classification.
#'
#' @param df Inputs a dataframe that has been created from one of the create quality report functions.
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

  # Anthropometric Plausibility Criteria ####
  anthro_plaus_vars <- c("prop_smart_flags", "sd_wfhz_noflag", "age_ratio.pvalue", "sex_ratio.pvalue", "dps_weight", "dps_height", "dps_muac",
                         "skewness_wfhz", "kurtosis_wfhz", "poisson_pvalues.wfhz")
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

  # Mortality Plausibility Score and Classification ####

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


  # Food Security Plausibility Criteria ####

  # FCS PLAUS CHECKS
  if(c("sd_fcs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_sd_fcs = dplyr::case_when(.data$sd_fcs < 8 ~ 5,
                                                                   .data$sd_fcs >= 8 & .data$sd_fcs < 9 ~ 2.5,
                                                                   .data$sd_fcs >= 9 & .data$sd_fcs < 14 ~ 0,
                                                                   .data$sd_fcs >= 14 & .data$sd_fcs <16 ~ 2.5,
                                                                   .data$sd_fcs >= 16 ~ 5, TRUE ~ 0))
  }

  if(c("flag_low_fcs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_low_fcs = dplyr::case_when(.data$flag_low_fcs < 2 ~ 0,
                                                          .data$flag_low_fcs >= 2 & .data$flag_low_fcs < 4 ~ 1,
                                                          .data$flag_low_fcs >= 4 & .data$flag_low_fcs < 10 ~ 2,
                                                          .data$flag_low_fcs >= 10 ~ 3, TRUE ~ 0))
  }

  if(c("flag_high_fcs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_high_fcs = dplyr::case_when(.data$flag_high_fcs < 2 ~ 0,
                                                           .data$flag_high_fcs >= 2 & .data$flag_high_fcs < 4 ~ 0.5,
                                                           .data$flag_high_fcs >= 4 & .data$flag_high_fcs < 10 ~ 1,
                                                           .data$flag_high_fcs >= 10 ~ 2, TRUE ~ 0))
  }

  if(c("flag_sd_foodgroup") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_sd_foodgroup = dplyr::case_when(.data$flag_sd_foodgroup < 2 ~ 0,
                                                               .data$flag_sd_foodgroup >= 2 & .data$flag_sd_foodgroup < 4 ~ 2,
                                                               .data$flag_sd_foodgroup >= 4 & .data$flag_sd_foodgroup < 10 ~ 4,
                                                               .data$flag_sd_foodgroup >= 10 ~ 6, TRUE ~ 0))
  }

  if(c("flag_meat_cereal_ratio") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_meat_cereal_ratio = dplyr::case_when(.data$flag_meat_cereal_ratio < 2 ~ 0,
                                                                    .data$flag_meat_cereal_ratio >= 2 & .data$flag_meat_cereal_ratio < 4 ~ 0.5,
                                                                    .data$flag_meat_cereal_ratio >= 4 & .data$flag_meat_cereal_ratio < 10 ~ 1,
                                                                    .data$flag_meat_cereal_ratio >= 10 ~ 2, TRUE ~ 0))
  }

  if(c("flag_protein_fcs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_protein_fcs = dplyr::case_when(.data$flag_protein_fcs < 2 ~ 0,
                                                              .data$flag_protein_fcs >= 2 & .data$flag_protein_fcs < 4 ~ .5,
                                                              .data$flag_protein_fcs >= 4 & .data$flag_protein_fcs < 10 ~ 1,
                                                              .data$flag_protein_fcs >= 10 ~ 2, TRUE ~ 0))
  }

  fcs_plaus_vars <- c("plaus_sd_fcs", "plaus_flag_low_fcs", "plaus_flag_high_fcs", "plaus_flag_sd_foodgroup", "plaus_flag_meat_cereal_ratio", "plaus_flag_protein_fcs")

  if(length(setdiff(c(fcs_plaus_vars), names(df))) < 6) {

    plaus_nms <- intersect(fcs_plaus_vars, names(df))

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_fcs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()

  }

  # rCSI PLAUS CHECKS

  if(c("flag_high_rcsi") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_high_rcsi = dplyr::case_when(.data$flag_high_rcsi < 2 ~ 0,
                                                            .data$flag_high_rcsi >= 2 & .data$flag_high_rcsi < 4 ~ 2,
                                                            .data$flag_high_rcsi >= 4 & .data$flag_high_rcsi < 10 ~ 3,
                                                            .data$flag_high_rcsi >= 10 ~ 4, TRUE ~ 0 ))
  }

  if(c("flag_sd_rcsicoping") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_sd_rcsicoping = dplyr::case_when(.data$flag_sd_rcsicoping < 2 ~ 0,
                                                                .data$flag_sd_rcsicoping >= 2 & .data$flag_sd_rcsicoping < 4 ~ 2,
                                                                .data$flag_sd_rcsicoping >= 4 & .data$flag_sd_rcsicoping < 10 ~ 4,
                                                                .data$flag_sd_rcsicoping >= 10 ~ 6, TRUE ~ 0))
  }

  if(c("sd_rcsi") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_sd_rcsi = dplyr::case_when(.data$sd_rcsi < 8 ~ 6,
                                                     .data$sd_rcsi >= 8 & .data$sd_rcsi < 9 ~ 3,
                                                     .data$sd_rcsi >= 9 & .data$sd_rcsi < 14 ~ 0,
                                                     .data$sd_rcsi >= 14 & .data$sd_rcsi <16 ~ 3,
                                                     .data$sd_rcsi >= 16 ~ 6, TRUE ~ 0))
  }

  if(c("flag_proteins_rcsi") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_proteins_rcsi = dplyr::case_when(.data$flag_proteins_rcsi < 2 ~ 0,
                                                                .data$flag_proteins_rcsi >= 2 & .data$flag_proteins_rcsi < 4 ~ 1,
                                                                .data$flag_proteins_rcsi >= 4 & .data$flag_proteins_rcsi < 10 ~ 2,
                                                                .data$flag_proteins_rcsi >= 10 ~ 3, TRUE ~ 0))

  }

  rcsi_plaus_vars <- c("plaus_flag_proteins_rcsi", "plaus_flag_sd_rcsicoping", "plaus_flag_high_rcsi", "plaus_sd_rcsi")

  if(length(setdiff(c(rcsi_plaus_vars), names(df))) < 4) {

    plaus_nms <- intersect(rcsi_plaus_vars, names(df))

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_rcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()

  }

  # HHS PLAUS CHECKS

  if(c("flag_severe_hhs") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_severe_hhs = dplyr::case_when(.data$flag_severe_hhs < 1 ~ 0,
                                                             .data$flag_severe_hhs >= 1 & .data$flag_severe_hhs < 5 ~ 4,
                                                             .data$flag_severe_hhs >= 5 & .data$flag_severe_hhs < 10 ~ 6,
                                                             .data$flag_severe_hhs >= 10 ~ 8, TRUE ~ 0))
  }

  if(c("poisson_pvalues.hhs_very_severe") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_poisson.hhs = ifelse(is.na(.data$poisson_pvalues.hhs_very_severe), 0 ,
                                               ifelse(.data$poisson_pvalues.hhs_very_severe >= 0.05, 0,
                                                      ifelse(.data$poisson_pvalues.hhs_very_severe >= 0.01 & .data$poisson_pvalues.hhs_very_severe < 0.05, 4,
                                                             ifelse(.data$poisson_pvalues.hhs_very_severe >= 0.001 & .data$poisson_pvalues.hhs_very_severe < 0.01, 6,
                                                                    ifelse(.data$poisson_pvalues.hhs_very_severe < 0.001, 8, 0))))))
  }

  hhs_plaus_vars <- c("plaus_flag_severe_hhs", "poisson_pvalues.hhs_very_severe")

  if(length(setdiff(c(hhs_plaus_vars), names(df))) < 2) {

    plaus_nms <- intersect(hhs_plaus_vars, names(df))

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_hhs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()

  }

  # LCSI PLAUS CHECKS

  if(c("flag_lcsi_liv_livestock") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_liv_livestock = dplyr::case_when(.data$flag_lcsi_liv_livestock < 2 ~ 0,
                                                                                .data$flag_lcsi_liv_livestock >= 2 & .data$flag_lcsi_liv_livestock < 4 ~ 1,
                                                                                .data$flag_lcsi_liv_livestock >= 4 & .data$flag_lcsi_liv_livestock < 10 ~ 2,
                                                                                .data$flag_lcsi_liv_livestock >= 10 ~ 3, TRUE ~ 0))
  }

  if(c("flag_lcsi_liv_agriculture") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_liv_agriculture = dplyr::case_when(.data$flag_lcsi_liv_agriculture < 2 ~ 0,
                                                                                  .data$flag_lcsi_liv_agriculture >= 2 & .data$flag_lcsi_liv_agriculture < 4 ~ 1,
                                                                                  .data$flag_lcsi_liv_agriculture >= 4 & .data$flag_lcsi_liv_agriculture < 10 ~ 2,
                                                                                  .data$flag_lcsi_liv_agriculture >= 10 ~ 3, TRUE ~ 0))
  }

  if(c("flag_lcsi_liv_agriculture") %in% names(df) | c("flag_lcsi_liv_livestock") %in% names(df)) {

    if(length(setdiff(c("flag_lcsi_liv_agriculture", "flag_lcsi_liv_livestock"), names(df)))==0) {

      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = dplyr::case_when( .data$plaus_flag_lcsi_liv_livestock >= .data$plaus_flag_lcsi_liv_agriculture ~ .data$plaus_flag_lcsi_liv_livestock,
                                                                                   .data$plaus_flag_lcsi_liv_livestock < .data$plaus_flag_lcsi_liv_agriculture ~ .data$plaus_flag_lcsi_liv_agriculture,
                                                                                   TRUE ~ 0))
    } else if(length(setdiff(c("flag_lcsi_liv_agriculture"), names(df)))==0) {

      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = .data$plaus_flag_lcsi_liv_agriculture)


    } else if(length(setdiff(c("flag_lcsi_liv_livestock"), names(df)))==0) {

      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = .data$plaus_flag_lcsi_liv_livestock)

    }


  }

  if(c("flag_lcsi_coherence") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_coherence = dplyr::case_when(.data$flag_lcsi_coherence < 2 ~ 0,
                                                                            .data$flag_lcsi_coherence >= 2 & .data$flag_lcsi_coherence < 4 ~ 3,
                                                                            .data$flag_lcsi_coherence >= 4 & .data$flag_lcsi_coherence < 10 ~ 5,
                                                                            .data$flag_lcsi_coherence >= 10 ~ 7, TRUE ~ 0))
  }

  if(c("flag_lcsi_na") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_na = dplyr::case_when(.data$flag_lcsi_na < 2 ~ 0,
                                                                     .data$flag_lcsi_na >= 2 & .data$flag_lcsi_na < 4 ~ 1,
                                                                     .data$flag_lcsi_na >= 4 & .data$flag_lcsi_na < 10 ~ 3,
                                                                     .data$flag_lcsi_na >= 10 ~ 5, TRUE ~ 0))
  }

  if(c("flag_lcsi_severity") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_severity = dplyr::case_when(.data$flag_lcsi_severity < 2 ~ 0,
                                                                           .data$flag_lcsi_severity >= 2 & .data$flag_lcsi_severity < 4 ~ 1,
                                                                           .data$flag_lcsi_severity >= 4 & .data$flag_lcsi_severity < 10 ~ 3,
                                                                           .data$flag_lcsi_severity >= 10 ~ 5, TRUE ~ 0))
  }

  lcs_plaus_vars <- c("plaus_flag_lcsi_agr_livestock", "plaus_flag_lcsi_coherence", "plaus_flag_lcsi_na", "plaus_flag_lcsi_severity")

  if(length(setdiff(lcs_plaus_vars, names(df))) < 4) {

    plaus_nms <- intersect(lcs_plaus_vars, names(df))

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_lcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()

  }

  # Overall FSL PLAUS CHECKS

  if(length(setdiff(c("corr.fcs_rcsi", "corr.fcs_rcsi.pvalue"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_corr.fcs_rcsi = ifelse(.data$corr.fcs_rcsi < -0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 0,
                                                 ifelse(.data$corr.fcs_rcsi < -0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 1,
                                                        ifelse(.data$corr.fcs_rcsi >= -0.2 & .data$corr.fcs_rcsi < 0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 1.5,
                                                               ifelse(.data$corr.fcs_rcsi >= -0.2 & .data$corr.fcs_rcsi < 0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 2,
                                                                      ifelse(.data$corr.fcs_rcsi >= 0.2 & .data$corr.fcs_rcsi.pvalue >= 0.05, 2.5,
                                                                             ifelse(.data$corr.fcs_rcsi >= 0.2 & .data$corr.fcs_rcsi.pvalue < 0.05, 3, 0)))))))
  }
  if(length(setdiff(c("corr.fcs_hhs", "corr.fcs_hhs.pvalue"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_corr.fcs_hhs = ifelse(.data$corr.fcs_hhs < -0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 0,
                                                ifelse(.data$corr.fcs_hhs < -0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 1,
                                                       ifelse(.data$corr.fcs_hhs >= -0.2 & .data$corr.fcs_hhs < 0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 1.5,
                                                              ifelse(.data$corr.fcs_hhs >= -0.2 & .data$corr.fcs_hhs < 0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 2,
                                                                     ifelse(.data$corr.fcs_hhs >= 0.2 & .data$corr.fcs_hhs.pvalue >= 0.05, 2.5,
                                                                            ifelse(.data$corr.fcs_hhs >= 0.2 & .data$corr.fcs_hhs.pvalue < 0.05, 3, 0)))))))
  }
  if(length(setdiff(c("corr.hhs_rcsi", "corr.hhs_rcsi.pvalue"), names(df)))==0) {

    df <- df %>%
      dplyr::mutate(plaus_corr.hhs_rcsi = ifelse(.data$corr.hhs_rcsi > 0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 0,
                                                 ifelse(.data$corr.hhs_rcsi > 0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 1,
                                                        ifelse(.data$corr.hhs_rcsi > -0.2 & .data$corr.hhs_rcsi <= 0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 1.5,
                                                               ifelse(.data$corr.hhs_rcsi > -0.2 & .data$corr.hhs_rcsi <= 0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 2,
                                                                      ifelse(.data$corr.hhs_rcsi <= -0.2 & .data$corr.hhs_rcsi.pvalue >= 0.05, 2.5,
                                                                             ifelse(.data$corr.hhs_rcsi <= -0.2 & .data$corr.hhs_rcsi.pvalue < 0.05, 3, 0)))))))
  }
  if(c("prop_fc_flags") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_prop_fc_flags = ifelse(.data$prop_fc_flags < 0.02, 0,
                                                 ifelse(.data$prop_fc_flags <0.04, 2,
                                                        ifelse(.data$prop_fc_flags <0.1, 4,
                                                               ifelse(.data$prop_fc_flags >= 0.10, 6, 0)))))
  }

  if(c("flag_fcsrcsi_box") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_fcsrcsi_box = dplyr::case_when(.data$flag_fcsrcsi_box < 2 ~ 0,
                                                              .data$flag_fcsrcsi_box >= 2 & .data$flag_fcsrcsi_box < 4 ~ 1,
                                                              .data$flag_fcsrcsi_box >= 4 & .data$flag_fcsrcsi_box < 10 ~ 2,
                                                              .data$flag_fcsrcsi_box >= 10 ~ 3, TRUE ~ 0))

  }

  if(c("flag_fcs_rcsi") %in% names(df)) {

    df <- df %>%
      dplyr::mutate(plaus_flag_fcs_rcsi = dplyr::case_when(.data$flag_fcs_rcsi < 2 ~ 0,
                                                           .data$flag_fcs_rcsi >= 2 & .data$flag_fcs_rcsi < 4 ~ 1,
                                                           .data$flag_fcs_rcsi >= 4 & .data$flag_fcs_rcsi < 10 ~ 1.5,
                                                           .data$flag_fcs_rcsi >= 10 ~ 2, TRUE ~ 0))

  }

  other_fsl_plaus_vars <- c("plaus_prop_fc_flags", "plaus_corr.hhs_rcsi", "plaus_corr.fcs_hhs", "plaus_corr.fcs_rcsi", "plaus_flag_fcsrcsi_box", "plaus_flag_fcs_rcsi")

  if(length(setdiff(other_fsl_plaus_vars, names(df))) < 6) {

    plaus_nms <- intersect(other_fsl_plaus_vars, names(df))

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_other_fsl = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()

  }

  # CONSOLIDATED FSL PLAUS SCORE

  all_fsl_plaus_vars <- c("plaus_lcsi", "plaus_fcs", "plaus_rcsi", "plaus_hhs", "plaus_other_fsl")

  if(length(setdiff(all_fsl_plaus_vars, names(df))) < 5) {

    plaus_nms <- intersect(all_fsl_plaus_vars, names(df))

    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_fsl_score = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(plaus_fsl_cat = dplyr::case_when(
        .data$plaus_fsl_score < 35 ~ "Excellent",
        .data$plaus_fsl_score >= 35 & .data$plaus_fsl_score < 50 ~ "Good",
        .data$plaus_fsl_score >= 50 & .data$plaus_fsl_score < 70 ~ "Acceptable",
        .data$plaus_fsl_score >= 70 ~ "Problematic"
      ))

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
