
#' Create IYCF Quality Report
#'
#' This function creates a summary report of indicators on an IYCF dataset to help
#' an analyst evaluate the quality of the data. This function also calls the
#' healthyr::create_plausibility_report function to add columns for plausibility
#' penalties and categorization.
#'
#' @param df Inputs a dataframe which has already been processed and standardized by
#' the healthyr::format_nut_health_indicators function.
#' @param grouping Inputs an optional character value specifying a column name by which
#' to group the results.
#' @param file_path Inputs an optional character value specifying a file_path to save
#' an xlsx copy of the results.
#' @param exp_prevalence_mad Input for a numeric value on the expected prevalence of the
#' Minimum Acceptable Diet (MAD) indicator. If no value is given, a default of 0.3 is used.
#' It is highly recommended to look at DHS, MICS, SMART or other health surveys from the area
#' to determine a realistic value.
#' @param exp_sex_ratio Input for a numeric value of the expected sex ratio (male : female) of children 6-23
#' months in the population. If no value is given, a default of 1 is used.
#' @param exp_ratio_under6m_6to23m Input for a numeric value of the expected ratio of children
#' under 6 months of age to children 6-23 months of age. If no value is given, a default of 0.33
#' is used assuming roughly 25% of children are under 6 months out of all children under 2 years
#' of age.
#'
#' @return Returns a dataframe summarized overall or at the specified grouping level, of
#' several key quality indicators, and plausibility penalties and categorization.
#' @export
#'
#' @examples
#' \dontrun{create_iycf_quality_report(df, grouping = "enum",
#' file_path = "myreports/iycf_report.xlsx", exp_prevalence_mad = 0.12, exp_sex_ratio = 1.1)}
#' @importFrom rlang .data
create_iycf_quality_report <- function(df, grouping = NULL, file_path = NULL, exp_prevalence_mad = NULL, exp_sex_ratio = NULL, exp_ratio_under6m_6to23m = NULL) {

  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }

  if(!is.null(exp_prevalence_mad)) {
    if(!is.numeric(exp_prevalence_mad) & length(exp_prevalence_mad)==1) {stop("Invalid input for exp_sex_ratio. Please put a single numeric value for the expected sex ratio in the population of male:female...or leave blank to assume a 1:1 male to female ratio (or 1).")}
    if(exp_prevalence_mad > 1) {stop("Please input exp_prevalence_mad as a decimal between 0 and 1. E.g ")}

    tot <- 100*exp_prevalence_mad + 100
    left <- (exp_prevalence_mad)
    right <- (1 - exp_prevalence_mad)

    mad_ratio <- c(left, right)

  } else {
    print("No expected prevalence for Minimum Acceptable Diet (MAD) was given, defaulting to 30%. It's highly recommended to look for a recent national or local prevalence from DHS, MICS, SMART or other surveys from the area. ")
    mad_ratio <- c(0.3,0.7)
  }

  if(!is.null(exp_sex_ratio)) {
    if(!is.numeric(exp_sex_ratio) & length(exp_sex_ratio)==1) {stop("Invalid input for exp_sex_ratio. Please put a single numeric value for the expected sex ratio in the population of male:female...or leave blank to assume a 1:1 male to female ratio (or 1).")}

    tot <- 100*exp_sex_ratio + 100
    left <- (100*exp_sex_ratio) / tot
    right <- (100) / tot

    sex_ratio <- c(left, right)

  } else {
    print("No expected sex ratio given, defaulting to 1:1 (male:female).")
    sex_ratio <- c(1,1)
  }

  if(!is.null(exp_ratio_under6m_6to23m)) {
    if(!is.numeric(exp_ratio_under6m_6to23m) & length(exp_ratio_under6m_6to23m)==1) {stop("Invalid input for exp_ratio_2_5 Please put a single numeric value for the expected age ratio of children 0-<2 years to children 2-<5years in the population...or leave blank to assume a 0-<2 years to 2-<5years ratio of 0.7 (that is, ~41% of individuals are <2 years of age out of under-5 children.).")}

    tot <- 100*exp_ratio_under6m_6to23m + 100
    left <- (100*exp_ratio_under6m_6to23m) / tot
    right <- (100) / tot

    age_under6m_23m_ratio <- c(left, right)
  } else {
    print("No expected age ratio given, defaulting to 0.33 (or approx. 25% of individuals are <6m months of age of all children <2 years.")
    age_under6m_23m_ratio <- c(0.25, 0.75)
  }

  if(c("age_months") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::filter(!is.na(.data$age_months)) %>%
      dplyr::filter(.data$age_months < 24) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n = dplyr::n())

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}
  }



  if(c("flag_high_mdd_low_mmf") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_high_mdd_low_mmf = sum(.data$flag_high_mdd_low_mmf, na.rm = TRUE) / sum(!is.na(.data$flag_high_mdd_low_mmf)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }
  if(c("age_months") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::mutate(is_under6m = ifelse(is.na(.data$age_months), NA, ifelse(.data$age_months < 6, 1, NA)),
                    is_6to23m = ifelse(is.na(.data$age_months), NA, ifelse(.data$age_months > 5 & .data$age_months < 24, 1, NA))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarize(age_ratio_under6m_6to23m = sum(!is.na(.data$is_under6m)) / sum(!is.na(.data$is_6to23m)),
                       age_ratio_under6m_6to23m.pvalue = stats::chisq.test(x = c(sum(!is.na(.data$is_under6m)), sum(!is.na(.data$is_6to23m))), p = age_under6m_23m_ratio)[3])

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }
  if(!is.null(mad_ratio)) {

    if(is.null(exp_prevalence_mad)) { exp_prevalence_mad <- (mad_ratio[1] / mad_ratio[2])}

    left <- mad_ratio[1]
    right <- mad_ratio[2]

    df2 <- df %>%
      dplyr::mutate(is_mad = ifelse(is.na(.data$iycf_mad), NA, ifelse(.data$iycf_mad == 1, 1, NA)),
                    is_not_mad = ifelse(is.na(.data$iycf_mad), NA, ifelse(.data$iycf_mad == 0, 1, NA))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarize(prop_mad_exp = exp_prevalence_mad,
                       prop_mad_obs = sum(!is.na(.data$is_mad)) / sum(!is.na(.data$iycf_mad)),
                       mad_ratio = sum(!is.na(.data$is_mad)) / sum(!is.na(.data$is_not_mad)),
                       sum_is_mad = sum(!is.na(.data$is_mad)),
                       sum_is_not_mad = sum(!is.na(.data$is_not_mad)),
                       mad_ratio.pvalue = stats::chisq.test(x = c(.data$sum_is_mad, .data$sum_is_not_mad), p = c(left, right))[3])

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }
  if(c("sex") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarize(sex_ratio = round(as.numeric(nipnTK::sexRatioTest(.data$sex, codes = c("1", "2"), pop = sex_ratio)[1]),3),
                       sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(.data$sex, codes = c("1", "2"), pop = sex_ratio)[5]),2))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}


  }
  if(c("iycf_mdd_score") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_mdd = mean(.data$iycf_mdd_score, na.rm = TRUE),
                       sd_mdd = stats::sd(.data$iycf_mdd_score, na.rm = TRUE))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }
  if(c("iycf_8") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_mmf = mean(.data$iycf_8, na.rm = TRUE),
                       sd_mmf = stats::sd(.data$iycf_8, na.rm = TRUE))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }
  if(c("flag_no_foods") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_no_foods = sum(.data$flag_no_foods, na.rm = TRUE) / sum(!is.na(.data$flag_no_foods)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

    num_flag_no_foods <- sum(df$flag_no_foods, na.rm = TRUE)
    if(num_flag_no_foods > 0) {cat(paste0("\n IYCF FLAG: No foods mentioned consumed yesterday, but more than 0 solid or semi-solid meals consumed for a child 6 months or older. \n", num_flag_no_foods, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators. \n")) }

  }
  if(c("flag_yes_foods") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_yes_foods = sum(.data$flag_yes_foods, na.rm = TRUE) / sum(!is.na(.data$flag_yes_foods)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

    num_flag_yes_foods <- sum(df$flag_yes_foods, na.rm = TRUE)
    if(num_flag_yes_foods > 0) {cat(paste0("\n IYCF FLAG: All foods mentioned consumed yesterday. \n", num_flag_yes_foods, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators.\n \n")) }

  }
  if(c("flag_all_foods_no_meal") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_all_foods_no_meal = sum(.data$flag_all_foods_no_meal, na.rm = TRUE) / sum(!is.na(.data$flag_all_foods_no_meal)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

    num_flag_all_foods_no_meal <- sum(df$flag_all_foods_no_meal, na.rm = TRUE)
    if(num_flag_all_foods_no_meal > 0) {cat(paste0("\n IYCF FLAG: All foods mentioned consumed yesterday, but 0 solid or semi-solid meals consumed. \n", num_flag_all_foods_no_meal, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators.\n")) }

  }
  if(c("flag_some_foods_no_meal") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_some_foods_no_meal = sum(.data$flag_some_foods_no_meal, na.rm = TRUE) / sum(!is.na(.data$flag_some_foods_no_meal)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

    num_flag_some_foods_no_meal <- sum(df$flag_some_foods_no_meal, na.rm = TRUE)
    if(num_flag_some_foods_no_meal > 0) {cat(paste0("\n IYCF FLAG: Some foods mentioned consumed yesterday, but 0 solid or semi-solid meals consumed for a child 6 months or older. \n", num_flag_some_foods_no_meal, " records were flagged. They will not be counted for numerators of EFF, MMF, MDD, or MAD indicators, but still for denominators.\n")) }


  }
  if(c("flag_yes_liquids") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_yes_liquids = sum(.data$flag_yes_liquids , na.rm = TRUE) / sum(!is.na(.data$flag_yes_liquids )))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

    num_flag_yes_liquids <- sum(df$flag_yes_liquids , na.rm = TRUE)
    if(num_flag_yes_liquids > 0) {cat(paste0("\n IYCF FLAG: All liquids were mentioned consumed yesterday, which is unlikely. \n", num_flag_yes_liquids, " records were flagged. They will not be counted for numerators of EBF indicator, but still for denominators.\n")) }

  }
  if(c("flag_no_anything") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_flag_no_anything = sum(.data$flag_no_anything, na.rm = TRUE) / sum(!is.na(.data$flag_no_anything)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

    num_flag_no_anything <- sum(df$flag_no_anything, na.rm = TRUE)
    if(num_flag_no_anything > 0) {cat(paste0("\n IYCF FLAG: No foods or liquids were mentioned consumed yesterday for a child 6 months or older, which is unlikely. \n", num_flag_no_anything, " records were flagged. No indicators are affected, but please check for data quality.\n")) }

  }
  print(results)

  if(c("iycf_caregiver") %in% colnames(df)) {

    df2 <- df %>%
      dplyr::mutate(iycf_caregiver = as.numeric(.data$iycf_caregiver)) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_iycf_caregiver = sum(.data$iycf_caregiver, na.rm = TRUE) / sum(!is.na(.data$iycf_caregiver)))

    if(!exists("results")) {results <- df2} else {results <- merge(results, df2)}

  }

  results <- healthyr::calculate_plausibility_report(df = results)
  print(results)

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(results, file_path)}

  return(results)

}
