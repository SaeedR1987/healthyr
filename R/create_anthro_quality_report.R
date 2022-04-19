#' Create Anthro Quality Report
#'
#' Generates a summary report of anthropometric data including summary results, key quality indicators,
#' and plausability report results.
#'
#' @param df Inputs a dataframe that has been standardized/formatted y the format_nut_health_indicators function.
#' @param grouping Inputs an optional character value as a grouping var. If included, results will be grouped by this variable
#' @param file_path Inputs an optional character value specifying a file location and name to save a copy of the output.
#' @param short_report Inputs a boolean value TRUE or FALSE to return just key variables. If FALSE,
#' returns a dataframe of all the variables calculated.
#'
#' @return Returns a dataframe with a results table for the anthropometric indicators.
#' @export
#'
#' @examples
#' \dontrun{create_anthro_quality_report(df = myanthrodata, index = "wfhz")}
#' @importFrom rlang .data
create_anthro_quality_report <- function(df, grouping = NULL, file_path = NULL, short_report = NULL) {

  options(warn=-1)
  if(is.null(short_report)) {short_report <- FALSE}

  # check which indexes are present

  indexes <- intersect(c("wfhz", "hfaz", "wfaz", "mfaz", "muac", "cgam"), names(df))
  if(length(indexes)==0) {stop("There are no anthropometric indexes present in the dataset, at least not with the standard variable names from the format_nut_health_indicators function. Missing any column name with wfhz', 'hfaz', 'wfaz', 'mfaz', 'muac', or 'cgam'. Please check your input")}
  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }

  if(!(c("oedema") %in% names(df))) {

    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(.data$oedema), 0, ifelse(.data$oedema == "y", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(num_oedema = sum(.data$oedemas, na.rm = TRUE))

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  } else {df <- df %>% dplyr::mutate(oedema = 0)}
  if(c("cluster") %in% names(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_clusters = dplyr::n_distinct(.data$cluster))

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("weight") %in% names(df)) {
    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(dps_weight = nipnTK::digitPreference(.data$weight)[[1]])

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("height") %in% names(df)) {
    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(dps_height = nipnTK::digitPreference(.data$height)[[1]])

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("sex") %in% names(df)) {
    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(sex_ratio = round(as.numeric(nipnTK::sexRatioTest(.data$sex, codes = c("1", "2"), pop = c(1,1))[1]),3),
                       sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(.data$sex, codes = c("1", "2"), pop = c(1,1))[5]),2))

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("age_months") %in% names(df)) {

    df2 <- df %>%
      dplyr::filter(!is.na(.data$age_months)) %>%
      dplyr::filter(.data$age_months >= 6 & .data$age_months < 60) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(age_ratio = nipnTK::ageRatioTest(.data$age_months, ratio = 0.85)[3],
                       age_ratio = round(as.numeric(.data$age_ratio),2),
                       age_ratio.pvalue = round(as.numeric(nipnTK::ageRatioTest(.data$age_months, ratio = 0.85)[7]),2),)

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("muac") %in% names(df)) {
    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(.data$oedema), 0, ifelse(.data$oedema == "y", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_muac = sum(!is.na(.data$muac), na.rm = TRUE),
                       dps_muac = nipnTK::digitPreference(.data$muac)[[1]],
                       mean_muac = round(mean(.data$muac, na.rm = TRUE),3),
                       sd_muac = round(stats::sd(.data$muac, na.rm = TRUE),2),
                       mean_muac = round(mean(.data$muac, na.rm = TRUE),3),
                       sd_muac = round(stats::sd(.data$muac, na.rm = TRUE),2),
                       num_muac_flags = sum(.data$muac_flag, na.rm = TRUE),
                       mean_muac_noflag = round(mean(.data$muac_noflag, na.rm = TRUE),3),
                       sd_muac_noflag = round(stats::sd(.data$muac_noflag, na.rm = TRUE),2),
                       gam_muac_abs = round(mean(.data$gam_muac_noflag, na.rm = TRUE),3)*100,
                       gam_muac_low = round(.data$gam_muac_abs - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$gam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       gam_muac_upp = round(.data$gam_muac_abs + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$gam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       mam_muac_abs = round(mean(.data$mam_muac_noflag, na.rm = TRUE),3)*100,
                       mam_muac_low = round(.data$mam_muac_abs - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$mam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       mam_muac_upp = round(.data$mam_muac_abs + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$mam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       sam_muac_abs = round(mean(.data$sam_muac_noflag, na.rm = TRUE),3)*100,
                       sam_muac_low = round(.data$sam_muac_abs - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$sam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       sam_muac_upp = round(.data$sam_muac_abs + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$sam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(sam_muac_low = ifelse(.data$sam_muac_low < 0, 0, .data$sam_muac_low),
             mam_muac_low = ifelse(.data$mam_muac_low < 0, 0, .data$mam_muac_low),
             gam_muac_low = ifelse(.data$gam_muac_low < 0, 0, .data$gam_muac_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(gam_muac_results = paste0(.data$gam_muac_abs, "% [",.data$gam_muac_low, " - ", .data$gam_muac_upp, "]"),
             mam_muac_results = paste0(.data$mam_muac_abs, "% [",.data$mam_muac_low, " - ", .data$mam_muac_upp, "]"),
             sam_muac_results = paste0(.data$sam_muac_abs, "% [",.data$sam_muac_low, " - ", .data$sam_muac_upp, "]"),
             mean_sd_muac = paste0(round(.data$mean_muac_noflag, 2), "+/-", round(.data$sd_muac_noflag, 2)),
             gam_muac_abs = NULL, gam_muac_low = NULL, gam_muac_upp = NULL, mam_muac_abs = NULL, mam_muac_low = NULL, mam_muac_upp = NULL,
             sam_muac_abs = NULL,  sam_muac_low = NULL, sam_muac_upp = NULL)

    if(c("cluster") %in% names(df)) {

      poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "gam_muac")
      names(poisson_pvalues)[2] <- "poisson_pvalues.muac"
      df2 <- merge(df2, poisson_pvalues, by = grouping)
    }

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("wfhz") %in% names(df)) {

    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(.data$oedema), 0, ifelse(.data$oedema == "y", 1, 0)),
                    gam_wfhz = ifelse(is.na(.data$gam_wfhz), NA, ifelse(.data$oedemas == 1, 1, .data$gam_wfhz)),
                    sam_wfhz = ifelse(is.na(.data$sam_wfhz), NA, ifelse(.data$oedemas == 1, 1, .data$sam_wfhz)),
                    gam_wfhz_noflag = ifelse(is.na(.data$gam_wfhz_noflag), NA, ifelse(.data$oedemas == 1, 1, .data$gam_wfhz_noflag)),
                    sam_wfhz_noflag = ifelse(is.na(.data$sam_wfhz_noflag), NA, ifelse(.data$oedemas == 1, 1, .data$sam_wfhz_noflag))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_wfhz = sum(!is.na(.data$wfhz), na.rm = TRUE),
                       mean_wfhz = round(mean(.data$wfhz, na.rm = TRUE),3),
                       sd_wfhz = round(stats::sd(.data$wfhz, na.rm = TRUE),2),
                       mean_wfhz_noflag = round(mean(.data$wfhz_noflag, na.rm = TRUE),3),
                       sd_wfhz_noflag = round(stats::sd(.data$wfhz_noflag, na.rm = TRUE),2),
                       num_smart_flags = sum(.data$wfhz_smart_flag, na.rm = TRUE),
                       prop_smart_flags = round((sum(.data$wfhz_smart_flag, na.rm = TRUE) / .data$n_children_wfhz)*100,2),
                       skewness_wfhz = abs(as.numeric(nipnTK::skewKurt(.data$wfhz_noflag)[1])),
                       kurtosis_wfhz = abs(as.numeric(nipnTK::skewKurt(.data$wfhz_noflag)[5])),
                       gam = round(mean(.data$gam_wfhz_noflag, na.rm = TRUE),3)*100,
                       gam_low = round(.data$gam - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$gam_wfhz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       gam_upp = round(.data$gam + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$gam_wfhz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       mam = round(mean(.data$mam_wfhz_noflag, na.rm = TRUE),3)*100,
                       mam_low = round(.data$mam - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$mam_wfhz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       mam_upp = round(.data$mam + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$mam_wfhz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       sam = round(mean(.data$sam_wfhz_noflag, na.rm = TRUE),3)*100,
                       sam_low = round(.data$sam - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$sam_wfhz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       sam_upp = round(.data$sam + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$sam_wfhz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(sam_low = ifelse(.data$sam_low <0, 0, .data$sam_low),
             mam_low = ifelse(.data$mam_low <0, 0, .data$mam_low),
             gam_low = ifelse(.data$gam_low <0, 0, .data$gam_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(gam_results = paste0(.data$gam, "% [",.data$gam_low, " - ", .data$gam_upp, "]"),
             mam_results = paste0(.data$mam, "% [",.data$mam_low, " - ", .data$mam_upp, "]"),
             sam_results = paste0(.data$sam, "% [",.data$sam_low, " - ", .data$sam_upp, "]"),
             mean_sd = paste0(round(.data$mean_wfhz_noflag, 2), "+/-", round(.data$sd_wfhz_noflag, 2)),
             gam = NULL, gam_low = NULL, gam_upp = NULL, mam = NULL, mam_low = NULL, mam_upp = NULL,
             sam = NULL,  sam_low = NULL, sam_upp = NULL)

    if(c("cluster") %in% names(df)) {

      poisson_pvalues <- calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "gam_wfhz_noflag")
      names(poisson_pvalues)[2] <- "poisson_pvalues.wfhz"
      df2 <- merge(df2, poisson_pvalues, by = grouping)

    }

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}


  }
  if(c("hfaz") %in% names(df)) {

    df2 <- df %>%
      dplyr::filter(.data$age_months >= 0 & .data$age_months <60) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_hfaz = sum(!is.na(.data$hfaz), na.rm = TRUE),
                       mean_hfaz = round(mean(.data$hfaz, na.rm = TRUE),3),
                       sd_hfaz = round(stats::sd(.data$hfaz, na.rm = TRUE),2),
                       mean_hfaz_noflag = round(mean(.data$hfaz_noflag, na.rm = TRUE),3),
                       sd_hfaz_noflag = round(stats::sd(.data$hfaz_noflag, na.rm = TRUE),2),
                       num_smart_flags_hfaz = sum(.data$hfaz_smart_flag, na.rm = TRUE),
                       prop_smart_flags_hfaz = round((sum(.data$hfaz_smart_flag, na.rm = TRUE) / .data$n_children_hfaz)*100,2),
                       skewness_hfaz = abs(as.numeric(nipnTK::skewKurt(.data$hfaz_noflag)[1])),
                       kurtosis_hfaz = abs(as.numeric(nipnTK::skewKurt(.data$hfaz_noflag)[5])),
                       stunting = round(mean(.data$global_stunting_noflag, na.rm = TRUE),3)*100,
                       stunting_low = round(.data$stunting - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$global_stunting_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       stunting_upp = round(.data$stunting + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$global_stunting_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_stunting = round(mean(.data$moderate_stunting_noflag, na.rm = TRUE),3)*100,
                       moderate_stunting_low = round(.data$moderate_stunting - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$moderate_stunting_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_stunting_upp = round(.data$moderate_stunting + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$moderate_stunting_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_stunting = round(mean(.data$severe_stunting_noflag, na.rm = TRUE),3)*100,
                       severe_stunting_low = round(.data$severe_stunting - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$severe_stunting_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_stunting_upp = round(.data$severe_stunting + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$severe_stunting_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(severe_stunting_low = ifelse(.data$severe_stunting_low <0, 0 , .data$severe_stunting_low),
             moderate_stunting_low = ifelse(.data$moderate_stunting_low <0, 0, .data$moderate_stunting_low),
             stunting_low = ifelse(.data$stunting_low <0, 0, .data$stunting_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(stunting_results = paste0(.data$stunting, "% [", .data$stunting_low, " - ", .data$stunting_upp, "]"),
             moderate_stunting_results = paste0(.data$moderate_stunting, "% [", .data$moderate_stunting_low, " - ", .data$moderate_stunting_upp, "]"),
             severe_stunting_results = paste0(.data$severe_stunting, "% [", .data$severe_stunting_low, " - ", .data$severe_stunting_upp, "]"),
             mean_sd_hfaz = paste0(round(.data$mean_hfaz_noflag, 2), "+/-", round(.data$sd_hfaz_noflag, 2)),
             stunting = NULL, stunting_low = NULL, stunting_upp = NULL, moderate_stunting = NULL, moderate_stunting_low = NULL, moderate_stunting_upp = NULL,
             severe_stunting = NULL,  severe_stunting_low = NULL, severe_stunting_upp = NULL)

    if(c("cluster") %in% names(df)) {

      poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "global_stunting_noflag")
      names(poisson_pvalues)[2] <- "poisson_pvalues.hfaz"
      df2 <- merge(df2, poisson_pvalues, by = grouping)

    }

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("wfaz") %in% names(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_wfaz = sum(!is.na(.data$wfaz), na.rm = TRUE),
                       mean_wfaz = round(mean(.data$wfaz, na.rm = TRUE),3),
                       sd_wfaz = round(stats::sd(.data$wfaz, na.rm = TRUE),2),
                       mean_wfaz_noflag = round(mean(.data$wfaz_noflag, na.rm = TRUE),3),
                       sd_wfaz_noflag = round(stats::sd(.data$wfaz_noflag, na.rm = TRUE),2),
                       num_smart_flags_wfaz = sum(.data$wfaz_smart_flag, na.rm = TRUE),
                       prop_smart_flags_wfaz = round((sum(.data$wfaz_smart_flag, na.rm = TRUE) / .data$n_children_wfaz)*100,2),
                       skewness_wfaz = abs(as.numeric(nipnTK::skewKurt(.data$wfaz_noflag)[1])),
                       kurtosis_wfaz = abs(as.numeric(nipnTK::skewKurt(.data$wfaz_noflag)[5])),
                       underweight = round(mean(.data$global_underweight_noflag, na.rm = TRUE),3)*100,
                       #gam_se = (sd(gam_wfhz_noflag, na.rm = TRUE) / sqrt(n()))*100,
                       underweight_low = round(.data$underweight - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$global_underweight_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       underweight_upp = round(.data$underweight + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$global_underweight_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_underweight = round(mean(.data$moderate_underweight_noflag, na.rm = TRUE),3)*100,
                       moderate_underweight_low = round(.data$moderate_underweight - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$moderate_underweight_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_underweight_upp = round(.data$moderate_underweight + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$moderate_underweight_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_underweight = round(mean(.data$severe_underweight_noflag, na.rm = TRUE),3)*100,
                       severe_underweight_low = round(.data$severe_underweight - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$severe_underweight_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_underweight_upp = round(.data$severe_underweight + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$severe_underweight_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(severe_underweight_low = ifelse(.data$severe_underweight_low <0, 0 , .data$severe_underweight_low),
             moderate_underweight_low = ifelse(.data$moderate_underweight_low <0, 0, .data$moderate_underweight_low),
             underweight_low = ifelse(.data$underweight_low <0, 0, .data$underweight_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(underweight_results = paste0(.data$underweight, "% [",.data$underweight_low, " - ", .data$underweight_upp, "]"),
             moderate_underweight_results = paste0(.data$moderate_underweight, "% [",.data$moderate_underweight_low, " - ", .data$moderate_underweight_upp, "]"),
             severe_underweight_results = paste0(.data$severe_underweight, "% [",.data$severe_underweight_low, " - ", .data$severe_underweight_upp, "]"),
             mean_sd_wfaz = paste0(round(.data$mean_wfaz_noflag, 2), "+/-", round(.data$sd_wfaz_noflag, 2)),
             underweight = NULL, underweight_low = NULL, underweight_upp = NULL, moderate_underweight = NULL, moderate_underweight_low = NULL, moderate_underweight_upp = NULL,
             severe_underweight = NULL,  severe_underweight_low = NULL, severe_underweight_upp = NULL)

    if(c("cluster") %in% names(df)) {

      poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "global_underweight_noflag")
      names(poisson_pvalues)[2] <- "poisson_pvalues.wfaz"
      df2 <- merge(df2, poisson_pvalues, by = grouping)

    }

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
  }
  if(c("mfaz") %in% names(df)) {

    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(.data$oedema), NA, ifelse(.data$oedema == "y", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_mfaz = sum(!is.na(.data$mfaz), na.rm = TRUE),
                       mean_mfaz = round(mean(.data$mfaz, na.rm = TRUE),3),
                       sd_mfaz = round(stats::sd(.data$mfaz, na.rm = TRUE),2),
                       mean_mfaz_noflag = round(mean(.data$mfaz_noflag, na.rm = TRUE),3),
                       sd_mfaz_noflag = round(stats::sd(.data$mfaz_noflag, na.rm = TRUE),2),
                       num_smart_flags_mfaz = sum(.data$mfaz_smart_flag, na.rm = TRUE),
                       prop_smart_flags_mfaz = round((sum(.data$mfaz_smart_flag, na.rm = TRUE) / .data$n_children_mfaz)*100,2),
                       skewness_mfaz = abs(as.numeric(nipnTK::skewKurt(.data$mfaz_noflag)[1])),
                       kurtosis_mfaz = abs(as.numeric(nipnTK::skewKurt(.data$mfaz_noflag)[5])),
                       global_mfaz = round(mean(.data$global_mfaz_noflag, na.rm = TRUE),3)*100,
                       #gam_se = (sd(gam_wfhz_noflag, na.rm = TRUE) / sqrt(n()))*100,
                       global_mfaz_low = round(.data$global_mfaz - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$global_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       global_mfaz_upp = round(.data$global_mfaz + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$global_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_mfaz = round(mean(.data$moderate_mfaz_noflag, na.rm = TRUE),3)*100,
                       moderate_mfaz_low = round(.data$moderate_mfaz - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$moderate_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_mfaz_upp = round(.data$moderate_mfaz + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$moderate_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_mfaz = round(mean(.data$severe_mfaz_noflag, na.rm = TRUE),3)*100,
                       severe_mfaz_low = round(.data$severe_mfaz - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$severe_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_mfaz_upp = round(.data$severe_mfaz + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$severe_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(severe_mfaz_low = ifelse(.data$severe_mfaz_low <0, 0, .data$severe_mfaz_low),
             moderate_mfaz_low = ifelse(.data$moderate_mfaz_low <0, 0, .data$moderate_mfaz_low),
             global_mfaz_low = ifelse(.data$global_mfaz_low <0, 0, .data$global_mfaz_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(mfaz_results = paste0(.data$global_mfaz, "% [",.data$global_mfaz_low, " - ", .data$global_mfaz_upp, "]"),
             moderate_mfaz_results = paste0(.data$moderate_mfaz, "% [",.data$moderate_mfaz_low, " - ", .data$moderate_mfaz_upp, "]"),
             severe_mfaz_results = paste0(.data$severe_mfaz, "% [",.data$severe_mfaz_low, " - ", .data$severe_mfaz_upp, "]"),
             mean_sd_mfaz = paste0(round(.data$mean_mfaz_noflag, 2), "+/-", round(.data$sd_mfaz_noflag, 2)),
             global_mfaz = NULL, global_mfaz_low = NULL, global_mfaz_upp = NULL, moderate_mfaz = NULL, moderate_mfaz_low = NULL, moderate_mfaz_upp = NULL,
             severe_mfaz = NULL,  severe_mfaz_low = NULL, severe_mfaz_upp = NULL)

    if(c("cluster") %in% names(df)) {

      poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "global_mfaz_noflag")
      names(poisson_pvalues)[2] <- "poisson_pvalues.mfaz"
      df2 <- merge(df2, poisson_pvalues, by = grouping)

    }

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}

  }
  if(c("c_gam") %in% names(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_cgam = sum(!is.na(.data$c_gam), na.rm = TRUE),
                       combined_gam = round(mean(.data$c_gam, na.rm = TRUE),3)*100,
                       combined_gam_low = round(.data$combined_gam - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$c_gam, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       combined_gam_upp = round(.data$combined_gam + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$c_gam, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       combined_sam = round(mean(.data$c_sam, na.rm = TRUE),3)*100,
                       combined_sam_low = round(.data$combined_sam - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$c_sam, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       combined_sam_upp = round(.data$combined_sam + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(.data$c_sam, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(combined_sam_low = ifelse(.data$combined_sam_low <0, 0, .data$combined_sam_low),
             combined_gam_low = ifelse(.data$combined_gam_low <0, 0, .data$combined_gam_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(combined_gam_results = paste0(.data$combined_gam, "% [",.data$combined_gam_low, " - ", .data$combined_gam_upp, "]"),
             combined_sam_results = paste0(.data$combined_sam, "% [",.data$combined_sam_low, " - ", .data$combined_sam_upp, "]"),
             combined_gam = NULL, combined_gam_low = NULL, combined_gam_upp = NULL, combined_mam = NULL, combined_mam_low = NULL, combined_mam_upp = NULL,
             combined_sam = NULL,  combined_sam_low = NULL, combined_sam_upp = NULL)

    if(c("cluster") %in% names(df)) {
      poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "c_gam")
      names(poisson_pvalues)[2] <- "poisson_pvalues.cgam"
      df2 <- merge(df2, poisson_pvalues, by = grouping)
    }

    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
  }

  if(length(indexes) != 0) {results.table <- healthyr::calculate_plausibility_report(results.table)}

  a <- c("n_clusters", "n_children_wfhz", "prop_smart_flags", "mean_sd", "gam_results", "sam_results",
         "n_children_muac", "mean_sd_muac", "gam_muac_results", "sam_muac_results",
         "anthro_plaus_score", "anthro_plaus_cat")

  b <- intersect(a, colnames(results.table))

  if(short_report == TRUE & length(setdiff(b, colnames(results.table)))==0) {

    results.table <- results.table %>%
      dplyr::select(1, b)
  }

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(results.table, file_path)}
  options(warn=0)
  return(results.table)

}
