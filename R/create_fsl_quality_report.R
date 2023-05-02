#' Create FSL Quality Report
#'
#' This function summarizes key indicators for food security outcome indicators to help
#' the analyst assess the overall FSL data quality. This summary is intended for FSL
#' outcome indicators only, focusing on FCS, HHS, rCSI, HDDS, and LCS indicators. This
#' function also calls a helper function 'healthyr::calculate_plausiblity_report' to assign
#' penalty scores and recommend an overall quality level for the FSL data.
#'
#' @param df Inputs a data frame that includes FSL outcome data that has already been
#' processed and standardized by the format_nut_health_indicators function.
#' @param grouping Inputs an optional character value specifying a column by which to
#' group or aggregate the results by.
#' @param short_report Inputs a boolean value TRUE or FALSE to return just key variables. If FALSE,
#' returns a dataframe of all the variables calculated.
#' @param file_path Inputs an optional character value specifying the file location to save a copy
#' of the results.
#'
#' @return Returns a dataframe of a summary table of the quality indicators.
#' @export
#'
#' @examples
#'\dontrun{create_fsl_report(df = proc_fsl1, grouping = "enum",
#' file_path = "folder/qualityreport.xlsx")}
#' @importFrom rlang .data
create_fsl_quality_report <- function(df, grouping = NULL, short_report = NULL, file_path = NULL) {

  options(warn=-1)

  if(is.null(short_report)) {short_report <- FALSE}

  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }

  # if using groups, drop records of a group if less than 10, so that doesn't get stuck. If too few overall, throw a stop error message.



  # Mean and SD of each FSL score

  if(c("fcs_score") %in% colnames(df)) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_fcs = round(mean(.data$fcs_score, na.rm = TRUE),2),
                       sd_fcs = round(stats::sd(.data$fcs_score, na.rm = TRUE),2),
                       mean_days_cereals = round(mean(.data$fcs_cereal, na.rm = TRUE),2),
                       sd_days_cereals = round(stats::sd(.data$fcs_cereal, na.rm = TRUE),2),
                       mean_days_legumes = round(mean(.data$fcs_legumes, na.rm = TRUE),2),
                       sd_days_legumes = round(stats::sd(.data$fcs_legumes, na.rm = TRUE),2),
                       mean_days_dairy = round(mean(.data$fcs_dairy, na.rm = TRUE),2),
                       sd_days_dairy = round(stats::sd(.data$fcs_dairy, na.rm = TRUE),2),
                       mean_days_meat = round(mean(.data$fcs_meat, na.rm = TRUE),2),
                       sd_days_meat = round(stats::sd(.data$fcs_meat, na.rm = TRUE),2),
                       mean_days_veg = round(mean(.data$fcs_veg, na.rm = TRUE),2),
                       sd_days_veg = round(stats::sd(.data$fcs_veg, na.rm = TRUE),2),
                       mean_days_fruit = round(mean(.data$fcs_fruit, na.rm = TRUE),2),
                       sd_days_fruit = round(stats::sd(.data$fcs_fruit, na.rm = TRUE),2),
                       mean_days_oils = round(mean(.data$fcs_oil, na.rm = TRUE),2),
                       sd_days_oils = round(stats::sd(.data$fcs_oil, na.rm = TRUE),2),
                       mean_days_sugar = round(mean(.data$fcs_sugar, na.rm = TRUE),2),
                       sd_days_sugar = round(stats::sd(.data$fcs_sugar, na.rm = TRUE),2))

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}


  }
  if(c("rcsi_score") %in% colnames(df)) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_rcsi = round(mean(.data$rcsi_score, na.rm = TRUE),2),
                       sd_rcsi = round(stats::sd(.data$rcsi_score, na.rm = TRUE),2),
                       mean_rcsi_lesspreferred_1 = round(mean(.data$rcsi_lesspreferred_1, na.rm = TRUE),2),
                       sd_rcsi_lesspreferred_1 = round(stats::sd(.data$rcsi_lesspreferred_1, na.rm = TRUE), 2),
                       mean_rcsi_borrowfood_2 = round(mean(.data$rcsi_borrowfood_2, na.rm = TRUE),2),
                       sd_rcsi_borrowfood_2 = round(stats::sd(.data$rcsi_borrowfood_2, na.rm = TRUE),2),
                       mean_rcsi_limitportion_3 = round(mean(.data$rcsi_limitportion_3, na.rm = TRUE),2),
                       sd_rcsi_limitportion_3 = round(stats::sd(.data$rcsi_limitportion_3, na.rm = TRUE), 2),
                       mean_rcsi_restrict_4 = round(mean(.data$rcsi_restrict_4, na.rm = TRUE), 2),
                       sd_rcsi_restrict_4 = round(stats::sd(.data$rcsi_restrict_4),2),
                       mean_rcsi_reducemeals5 = round(mean(.data$rcsi_reducemeals5, na.rm = TRUE), 2),
                       sd_rcsi_reducemeals5 = round(stats::sd(.data$rcsi_reducemeals5, na.rm = TRUE), 2)
                       )

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}


  }
  if(c("hhs_score") %in% colnames(df)) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_hhs = round(mean(.data$hhs_score, na.rm = TRUE),2),
                       sd_hhs = round(stats::sd(.data$hhs_score, na.rm = TRUE),2))

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}


  }
  if(c("hdds_score") %in% colnames(df)) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_hdds = round(mean(.data$hdds_score, na.rm = TRUE),2),
                       sd_hdds = round(stats::sd(.data$hdds_score, na.rm = TRUE),2))

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}


  }

  # Correlations between FCS and rCSI
  if(length(setdiff(c("fcs_score", "rcsi_score"), colnames(df)))==0) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(corr.fcs_rcsi = round(as.numeric(stats::cor.test(.data$fcs_score, .data$rcsi_score)[4]),2),
                       corr.fcs_rcsi.pvalue = as.numeric(stats::cor.test(.data$fcs_score, .data$rcsi_score)[3]))
    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }

  # Correlations between FCS and HHS
  if(length(setdiff(c("fcs_score", "hhs_score"), colnames(df)))==0) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(corr.fcs_hhs = round(as.numeric(stats::cor.test(.data$fcs_score, .data$hhs_score)[4]),2),
                       corr.fcs_hhs.pvalue = round(as.numeric(stats::cor.test(.data$fcs_score, .data$hhs_score)[3]),6))
    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }
  # Correlations between HDDS and FCS

  if(length(setdiff(c("fcs_score", "hdds_score"), colnames(df)))==0) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(corr.fcs_hdds = round(as.numeric(stats::cor.test(.data$fcs_score, .data$hdds_score)[4]),2),
                       corr.fcs_hdds.pvalue = round(as.numeric(stats::cor.test(.data$fcs_score, .data$hdds_score)[3]),3))
    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }

  # Correlations between HDDS and rCSI
  if(length(setdiff(c("hdds_score", "rcsi_score"), colnames(df)))==0) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(corr.hdds_rcsi = round(as.numeric(stats::cor.test(.data$hdds_score, .data$rcsi_score)[4]),2),
                       corr.hdds_rcsi.pvalue = round(as.numeric(stats::cor.test(.data$hdds_score, .data$rcsi_score)[3]),3))
    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }

  # Correlations between HHS and rCSI
  if(length(setdiff(c("hhs_score", "rcsi_score"), colnames(df)))==0) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(corr.hhs_rcsi = round(as.numeric(stats::cor.test(.data$hhs_score, .data$rcsi_score)[4]),2),
                       corr.hhs_rcsi.pvalue = round(as.numeric(stats::cor.test(.data$hhs_score, .data$rcsi_score)[3]),3))

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }

  # FCS-rCSI box (fcs > 56 and rCSI > 18)

  # if(length(setdiff(c("fcs_score", "rcsi_score"), colnames(df)))==0) {
  #
  #   results2 <- df %>%
  #     dplyr::group_by(!!rlang::sym(grouping)) %>%
  #     dplyr::summarise(flag_)
  #
  # }

  # Poisson or clustering of extreme results

  if(length(setdiff(c("hhs_cat", "cluster"), colnames(df)))==0) {

    df <- df %>% dplyr::mutate(hhs_severe = ifelse(is.na(.data$hhs_cat), NA, ifelse(.data$hhs_cat == "Very Severe" | .data$hhs_cat == "Severe", 1, 0)))

    poisson_pvalues <- healthyr::calculate_poisson_pvalues(df, strata = grouping, cluster = "cluster", case = "hhs_severe")
    names(poisson_pvalues)[2] <- "poisson_pvalues.hhs_severe"

    if(!exists("results")) {

      results <- poisson_pvalues

    } else {results <-  merge(results, poisson_pvalues, by = grouping)}

  }

  # % of different flags

  # if the df doesn't have any of the below, the difference is 5 and it will skip this part.
  if(length(setdiff(c("fcs_score", "hhs_score", "hdds_score", "rcsi_score", "flag_lcsi_coherence"), names(df)))<5) {

    nms <- df %>% dplyr::select(dplyr::starts_with('flag')) %>% names()

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise_at(.vars = nms, ~ round(mean(., na.rm = TRUE),3)*100)

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }


  results2 <- df %>%
    dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::summarise(n = dplyr::n())

  if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  results <- results %>% dplyr::select(c(1, .data$n, dplyr::everything()))

  # % of households with FEWS NET flags

  if(c("flag_fc_cell") %in% names(df)) {

    results2 <- df %>%
      dplyr::mutate(p1 = ifelse(is.na(.data$fc_phase), NA, ifelse(.data$fc_phase == "Phase 1 FC", 1, 0)),
                    p2 = ifelse(is.na(.data$fc_phase), NA, ifelse(.data$fc_phase == "Phase 2 FC", 1, 0)),
                    p3 = ifelse(is.na(.data$fc_phase), NA, ifelse(.data$fc_phase == "Phase 3 FC", 1, 0)),
                    p4 = ifelse(is.na(.data$fc_phase), NA, ifelse(.data$fc_phase == "Phase 4 FC", 1, 0)),
                    p5 = ifelse(is.na(.data$fc_phase), NA, ifelse(.data$fc_phase == "Phase 5 FC", 1, 0)),
      ) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_fc_flags = sum(.data$flag_fc_cell, na.rm = TRUE) /sum(!is.na(.data$fc_cell), na.rm = TRUE),
                       fews_p1 = round(sum(.data$p1, na.rm = TRUE) / sum(!is.na(.data$fc_cell)),2),
                       fews_p2 = round(sum(.data$p2, na.rm = TRUE) / sum(!is.na(.data$fc_cell)),2),
                       fews_p3 = round(sum(.data$p3, na.rm = TRUE) / sum(!is.na(.data$fc_cell)),2),
                       fews_p4 = round(sum(.data$p4, na.rm = TRUE) / sum(!is.na(.data$fc_cell)),2),
                       fews_p5 = round(sum(.data$p5, na.rm = TRUE) / sum(!is.na(.data$fc_cell)),2))

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

    results <- results %>% dplyr::select(c(1, .data$n, dplyr::everything()))

  }

  if(c("food_exp_share") %in% names(df)) {

    results2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_fc_flags = sum(.data$flag_fc_cell, na.rm = TRUE) /sum(!is.na(.data$fc_cell), na.rm = TRUE),
                       fes_1 = round(sum(.data$food_exp_share == "1", na.rm = TRUE) / sum(!is.na(.data$food_exp_share)),2),
                       fes_2 = round(sum(.data$food_exp_share == "2", na.rm = TRUE) / sum(!is.na(.data$food_exp_share)),2),
                       fes_3 = round(sum(.data$food_exp_share == "3", na.rm = TRUE) / sum(!is.na(.data$food_exp_share)),2),
                       fes_4 = round(sum(.data$food_exp_share == "4", na.rm = TRUE) / sum(!is.na(.data$food_exp_share)),2))

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

    results <- results %>% dplyr::select(c(1, .data$n, dplyr::everything()))

  }

  results <- healthyr::calculate_plausibility_report(df = results)

  a <- c("n",
         "fews_p1", "fews_p2", "fews_p3", "fews_p4", "fews_p5",
         "flag_severe_hhs", "flag_lcsi_severity",
         "plaus_fcs", "plaus_rcsi", "plaus_hhs", "plaus_lcsi", "plaus_other_fsl",
         "plaus_fsl_score", "plaus_fsl_cat")

  b <- intersect(a, colnames(results))

  if(short_report == TRUE & length(setdiff(b, colnames(results)))==0) {

    results <- results %>%
      dplyr::select(1, b)

  }

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(results, file_path)}

  options(warn=0)

  return(results)

}
