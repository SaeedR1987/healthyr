#' Create Mortality Quality Report
#'
#' This function generates a summary table of the mortality data, either at an overall level or disaggregated by
#' a specified grouping, such as team_id, cluster, or other.
#'
#' @param df Inputs a dataframe that has been standardized by the format_mortality_current_census function.
#' @param grouping A character value specifying the column name by which you want to generate disaggregated results for
#' for the mortality quality summary.
#' @param short_report Inputs a boolean value TRUE or FALSE to return just key variables. If FALSE,
#' returns a dataframe of all the variables calculated.
#' @param file_path Inputs an optional character value specifying the file location to save a copy
#' of the results.
#' @param exp_sex_ratio Inputs a numeric value specifying the expected sex ratio in the population.
#' If no value is given, a default value of 1:1 is used.
#' @param exp_ratio_0_4 Inputs a numeric value specifying the expected ratio of people under-5 years
#' of age : people greater than or equal to 5 years of age in the population. If no value is given, a
#' default of 0.25 is used, which is about 20% of the population is under-5. It is highly recommended to
#' use an actual value specific to your context as this default ratio is likely not specific enough.
#' @param exp_ratio_2_5 Inputs a numeric value specifying the expected ratio of people under-5 years
#' of age : people greater than or equal to 5 years of age in the population. If no value is given, a
#' default of 0.7 is used, which is about 41% are children under-2 years of age out of the under-5 population. It is highly recommended to
#' use an actual value specific to your context as this default ratio is likely not specific enough.
#' @param exp_ratio_5_10 Inputs a numeric value specifying the expected ratio of people under-5 years
#' of age : people greater than or equal to 5 years of age in the population. If no value is given, a
#' default of 1.1 is used, which is about 52% are children under-5 years of age out of the under-10 population. It is highly recommended to
#' use an actual value specific to your context as this default ratio is likely not specific enough.
#' @param exp_hh_size Inputs a numeric value specifying the expected average household size. If no value is given, a
#' default of 5 is used It is highly recommended to use an actual value specific to your context as this default ratio
#' is likely not specific enough.
#'
#' @return Returns a dataframe with summary indicators on the mortality data.
#' @export
#'
#' @examples
#' \dontrun{create_mortality_quality_report(df)}
#'
#' @importFrom rlang .data
create_mortality_quality_report <- function(df, grouping = NULL, file_path = NULL, short_report = NULL, exp_sex_ratio = NULL, exp_ratio_0_4 = NULL, exp_ratio_2_5 = NULL, exp_ratio_5_10 = NULL, exp_hh_size = NULL) {

  options(warn=-1)

  if(length(setdiff(c("sex", "age_years", "join", "left", "birth", "death", "date_dc", "date_recall"), colnames(df)))>0) {
    stop("It does not appear that the dataset has been formatted yet by the format_mortality_current_census function, as it is missing at least one of the column names of sex, age_years, join, left, birth, death, date_dc, date_recall. Please standardize the data before using this function.")
  }

  if(is.null(short_report)) {short_report <- FALSE}

  if(!is.null(exp_sex_ratio)) {
    if(!is.numeric(exp_sex_ratio) & length(exp_sex_ratio)==1) {stop("Invalid input for exp_sex_ratio. Please put a single numeric value for the expected sex ratio in the population of male:female...or leave blank to assume a 1:1 male to female ratio (or 1).")}

    tot <- 100*exp_sex_ratio + 100
    left <- (100*exp_sex_ratio) / tot
    right <- (100) / tot

    sx_ratio <- c(left, right)

  } else {
    print("No expected sex ratio given, defaulting to 1:1 (male:female).")
    sx_ratio <- c(1,1)
  }

  if(!is.null(exp_ratio_0_4)) {
    if(!is.numeric(exp_ratio_0_4) & length(exp_ratio_0_4)==1) {stop("Invalid input for exp_ratio_0_4years Please put a single numeric value for the expected age ratio of <5years to >5years in the population...or leave blank to assume a 1:4 <5 years to >5 years ratio (that is, 20% of individuals are <5 years of age).")}

    tot <- 100*exp_ratio_0_4 + 100
    left <- (100*exp_ratio_0_4) / tot
    right <- (100) / tot

    age_under5_ratio <- c(left, right)

  } else {
    print("No expected age ratio given, defaulting to 0.25 (or 20% of individuals are <5 years of age.")
    age_under5_ratio <- c(.2,.8)
  }

  if(!is.null(exp_ratio_2_5)) {
    if(!is.numeric(exp_ratio_2_5) & length(exp_ratio_2_5)==1) {stop("Invalid input for exp_ratio_2_5 Please put a single numeric value for the expected age ratio of children 0-<2 years to children 2-<5years in the population...or leave blank to assume a 0-<2 years to 2-<5years ratio of 0.7 (that is, ~41% of individuals are <2 years of age out of under-5 children.).")}

    tot <- 100*exp_ratio_2_5 + 100
    left <- (100*exp_ratio_2_5) / tot
    right <- (100) / tot

    age_under2to5_ratio <- c(left, right)
  } else {
    print("No expected age ratio given, defaulting to 0.7 (or approx. 41% of individuals are <2 years of age of all children <5 years.")
    age_under2to5_ratio <- c(0.4118, 0.5882)
  }

  if(!is.null(exp_ratio_5_10)) {
    if(!is.numeric(exp_ratio_5_10) & length(exp_ratio_5_10)==1) {stop("Invalid input for exp_ratio_5_10 Please put a single numeric value for the expected age ratio of children 0-<2 years to children 2-<5years in the population...or leave blank to assume a 0-<5 years to 5-<10years ratio of 1.1 (that is, ~52% of individuals are <5 years of age out of under-10 children).")}

    tot <- 100*exp_ratio_5_10 + 100
    left <- (100*exp_ratio_5_10) / tot
    right <- (100) / tot

    age_under5to10_ratio <- c(left, right)
  } else {
    print("No expected age ratio given, defaulting to 1.1 (or approx. 52% of individuals are <5 years of age out all children <10 years.")
    age_under5to10_ratio <- c(0.5238, 0.4762)
  }

  if(!is.null(exp_hh_size)) {
    if(!is.numeric(exp_hh_size) & length(exp_hh_size)==1) {stop("Invalid input for exp_hh_size Please put a single numeric value for the expected age ratio of children 0-<2 years to children 2-<5years in the population...or leave blank to assume a 0-<5 years to 5-<10years ratio of 1.1 (that is, ~52% of individuals are <5 years of age out of under-10 children).")}

    expected_hh_size <- exp_hh_size

  } else {
    print("No expected household size given, defaulting to 5 people per household. Advised to use a value from secondary data instead.")
    expected_hh_size <- 5
  }

  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }

  if(c("age_years") %in% names(df)) {df <- df %>% dplyr::mutate(age_years = as.numeric(.data$age_years))}

  # need to add sex and age ratios, poisson p-values for deaths, proportion of HHs with under-5 child, Avg. household size per grouping

  # summarizing individual level indicators

  df2 <- df %>%
    dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::summarize(total_people = sum(!is.na(.data$person_time), na.rm = TRUE),
                     total_persontime = sum(.data$person_time, na.rm = TRUE),
                     avg.persontime = mean(.data$person_time, na.rm = TRUE),
                     total_under5 = sum(.data$under_5, na.rm = TRUE),
                     total_under5_persontime = sum(.data$under_5_pt, na.rm = TRUE),
                     avg.under5_persontime = mean(.data$under_5_pt, na.rm = TRUE),
                     joins = sum(!is.na(.data$join), na.rm = TRUE),
                     joins_under5 = sum(!is.na(.data$join_under5), na.rm = TRUE),
                     lefts = sum(!is.na(.data$left), na.rm = TRUE),
                     lefts_under5 = sum(!is.na(.data$left_under5), na.rm = TRUE),
                     births = sum(!is.na(.data$birth), na.rm = TRUE),
                     births_under5 = sum(!is.na(.data$birth_under5), na.rm = TRUE),
                     deaths = sum(!is.na(.data$death), na.rm = TRUE),
                     deaths_under5 = sum(!is.na(.data$death_under5), na.rm = TRUE),
                     sex_ratio = round(as.numeric(nipnTK::sexRatioTest(.data$sex, codes = c("1", "2"), pop = sx_ratio)[1]),3),
                     sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(.data$sex, codes = c("1", "2"), pop = sx_ratio)[5]),2),
                     age_ratio_0_5 = sum(!is.na(.data$age_0to5)) / sum(!is.na(.data$age_5plus)),
                     age_ratio_0_5.pvalue = stats::chisq.test(x = c(sum(!is.na(.data$age_0to5)), sum(!is.na(.data$age_5plus))), p = age_under5_ratio)[3],
                     age_ratio_2_5 = sum(!is.na(.data$age_0to2)) / sum(!is.na(.data$age_2to5)),
                     age_ratio_2_5.pvalue = stats::chisq.test(x = c(sum(!is.na(.data$age_0to2)), sum(!is.na(.data$age_2to5))), p = age_under2to5_ratio)[3],
                     age_ratio_5_10 = sum(!is.na(.data$under_5)) / sum(!is.na(.data$age_5to10)),
                     age_ratio_5_10.pvalue = stats::chisq.test(x = c(sum(!is.na(.data$under_5)), sum(!is.na(.data$age_5to10))), p = age_under5to10_ratio)[3]
    ) %>%
    dplyr::mutate(cdr = .data$deaths / (.data$total_persontime),
                  cdr_se = sqrt((.data$cdr * (1 - .data$cdr)) / .data$total_persontime),
                  cdr_lower_ci = round((.data$cdr - 1.96*.data$cdr_se)*10000,3),
                  cdr_upper_ci = round((.data$cdr + 1.96*.data$cdr_se)*10000,3),
                  u5dr = .data$deaths_under5 / (.data$total_under5_persontime),
                  u5dr_se = sqrt((.data$u5dr * (1 - .data$u5dr)) / .data$total_under5_persontime),
                  u5dr_lower_ci = round((.data$u5dr - 1.96*.data$u5dr_se)*10000,3),
                  u5dr_upper_ci = round((.data$u5dr + 1.96*.data$u5dr_se)*10000,3),
                  cdr = round(.data$cdr,6)*10000,
                  u5dr = round(.data$u5dr*10000,3),
                  cdr_ci = paste0(.data$cdr, " [", .data$cdr_lower_ci, " - ", .data$cdr_upper_ci, "]"),
                  u5dr_ci = paste0(.data$u5dr, " [", .data$u5dr_lower_ci, " - ", .data$u5dr_upper_ci, "]"),
                  prop_join_people = round((.data$joins / .data$total_people),2)*100,
                  prop_left_people = round((.data$lefts / .data$total_people),2)*100) %>%
    dplyr::select(.data$cdr_ci, .data$u5dr_ci, dplyr::everything())

  # summarizing household level indicators
  # average household size, % of households with a child under 5
  # of households

  df3 <- df %>%
    dplyr::group_by(!!rlang::sym(grouping), .data$hh_id) %>%
    dplyr::summarise(hh_size = sum(!is.na(.data$sex), na.rm = TRUE),
                     total_under5 = sum(!is.na(.data$under_5), na.rm = TRUE),
                     num_deaths = sum(!is.na(.data$death), na.rm = TRUE),
                     total_flag_deaths = sum(.data$flag_deaths, na.rm = TRUE)) %>%
    dplyr::mutate(is_hh = ifelse(is.na(.data$hh_id), NA, 1),
                  is_hh_under5 = ifelse(is.na(.data$hh_id), NA, ifelse(.data$total_under5 > 0, 1, 0)),
                  is_hh_flag_deaths = ifelse(is.na(.data$hh_id), NA, ifelse(.data$total_flag_deaths > 0, 1, 0))) %>%
    dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::summarise(mean_hh_size = mean(.data$hh_size, na.rm = TRUE),
                     mean_hh_size.pvalue = round(as.numeric(stats::t.test(x = .data$hh_size, mu = expected_hh_size, alternative = "two.sided")[3]),2),
                     mean_num_under5 = mean(.data$total_under5, na.rm = TRUE),
                     mean_deaths_per_hh = mean(.data$num_deaths, na.rm = TRUE),
                     n_hh = sum(.data$is_hh, na.rm = TRUE),
                     n_hh_under_5 = sum(.data$is_hh_under5, na.rm = TRUE),
                     n_hh_flag_deaths = sum(.data$is_hh_flag_deaths, na.rm = TRUE)
    ) %>%
    dplyr::mutate(prop_hh_under5 = round((.data$n_hh_under_5 / .data$n_hh),2),
                  prop_hh_flag_deaths = round((.data$n_hh_flag_deaths / .data$n_hh), 2))

  df4 <- merge(df3, df2, all.x = TRUE)

  # Poisson or clustering of extreme results

  if(length(setdiff(c("death", "cluster"), colnames(df)))==0) {

    df5 <- df %>% dplyr::mutate(death_numeric = ifelse(!is.na(.data$death), 1, 0))

    poisson_pvalues <- healthyr::calculate_poisson_pvalues(df5, strata = grouping, cluster = "cluster", case = "death_numeric")
    names(poisson_pvalues)[2] <- "poisson_pvalues.deaths"

    if(!exists("df4")) {df4 <- poisson_pvalues} else {df4 <-  merge(df4, poisson_pvalues, by = grouping)}

  }

  df4 <- df4 %>%
    dplyr::select(c(1, .data$cdr, .data$cdr_ci, .data$u5dr, .data$u5dr_ci, .data$deaths, .data$deaths_under5, .data$mean_deaths_per_hh, .data$n_hh_flag_deaths, .data$prop_hh_flag_deaths, .data$total_people, .data$mean_hh_size, .data$mean_hh_size.pvalue, .data$total_persontime, .data$total_under5, .data$mean_num_under5, .data$total_under5_persontime,
                    .data$n_hh, .data$n_hh_under_5, .data$sex_ratio, .data$sex_ratio.pvalue, .data$age_ratio_0_5, .data$age_ratio_0_5.pvalue, .data$age_ratio_2_5, .data$age_ratio_2_5.pvalue, .data$age_ratio_5_10, .data$age_ratio_5_10.pvalue,
                    .data$joins, .data$prop_join_people, .data$lefts, .data$prop_left_people, .data$births, .data$poisson_pvalues.deaths))

  df4 <- healthyr::calculate_plausibility_report(df4)

  if(short_report == TRUE) {

    df4 <- df4 %>%
      dplyr::select(1, .data$cdr_ci,.data$u5dr_ci,.data$deaths, .data$deaths_under5, .data$prop_hh_flag_deaths,
                    .data$sex_ratio.pvalue, .data$age_ratio_0_5.pvalue,.data$prop_join_people,.data$prop_left_people,
                    .data$poisson_pvalues.deaths, .data$mort_plaus_score, .data$mort_plaus_cat)
  }

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df4, file_path)}
  options(warn=0)
  return(df4)


}
