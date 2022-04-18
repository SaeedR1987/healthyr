# General analysis functions

#' Analyse Survey Results
#'
#' Generalizable function to analyse survey data for proportions, means, rates or ratios. Wraps srvyr functions.
#'
#' @param df Inputs a dataframe with survey data
#' @param file_path Optional input of a character file path which to save an excel sheet of the results.
#' @param sample_design Input to specify the survey design. Options include 'srs' for simple random sampling, 'two_stage_cluster',
#' 'two_stage_stratified', and 'two_stage_stratified_cluster'.
#' @param aggregation Optional input of character value specifying by which variable you want to aggregate or group the results.
#' @param strata Optional input for character value specifying the column name of the strata variable.
#' @param cluster Optional input for character value specifying the column name of the cluster variable.
#' @param svy_weights Optional input for character value specifying the column name of the svy_weights variable.
#' @param proportions Optional input for a character vector specifying all the column names to analyse as proportions.
#' If categorical variables are included, then dummy variables will be created for each option.
#' @param means Optional input for a character vector specifying all the column names to analyse as means
#' @param ratios_rates.numerators Optional input for character vector specifying all the column names of numerators for rates or ratios to analyse.
#' @param ratios_rates.denominators Optional input for character vector specifying all the column names of denominators for rates or ratios to analyse.
#' @param ratios_rates.multiplier Optional input for numeric value specifying a number to multiply the rate results by.
#'
#' @return Returns a dataframe of aggregated results, with point estimates and 95% confidence intervals within the same cell value,
#' by aggregation specified or overall.
#' @export
#'
#' @examples
#' \dontrun{analyse_survey_results(df = proc_mortality1, ratios_tates.numerators = c(deaths, under5_deaths),
#' ratios_rates.denominators = c(persontime, under5_persontime), ratios_rates_multiplier = 10000}
#' @importFrom rlang .data
#' @importFrom rlang :=
analyse_survey_results <- function(df, file_path = NULL,
                                   sample_design, aggregation = NULL, strata = NULL, cluster = NULL, svy_weights = NULL,
                                   proportions = NULL,
                                   means = NULL,
                                   ratios_rates.numerators = NULL,
                                   ratios_rates.denominators = NULL,
                                   ratios_rates.multiplier = NULL) {

  # Check if proportions coded as 0 / 1

  if(!is.null(proportions)) {

    proportion_values <- df %>% dplyr::select(proportions) %>% t %>% c %>% unique

    if(length(setdiff(proportion_values, c(1, 0, NA)))==0) {

      print("Good - proportion values are coded as 1/0 for yes/no")

    } else {

      proportions_list <- proportions

      for (i in 1:length(proportions_list)) {

        col_values <- df %>% dplyr::select(proportions_list[[i]]) %>% t %>% c %>% unique

        if(length(setdiff(col_values, c(1,0,NA)))==0) {

          print(paste0("Good, ", proportions_list[[i]], " are already coded as 1/0."))

        } else if(length(setdiff(col_values, c(1,2,9,NA)))==0) {

          print(paste0("Values for, ", proportions_list[[i]], " are 1/2/9. Recoding to 1/0."))

          df <- df %>%
            dplyr::mutate(dplyr::across(proportions_list[[i]], ~ (ifelse(. == 2, 0, . ))),
                   dplyr::across(proportions_list[[i]], ~ (ifelse(. == 9, 0, . ))))

        } else {

          print(paste0("Values for, ", proportions_list[[i]], " are neither 1/2/9 or 1/0. Recoding to 1/0 with dummy variables."))

          subset_df <- df %>% dplyr::select(proportions_list[[i]])

          a <- c(1,0,NA)

          subset_df <- subset_df[,sapply(subset_df, function(col) length(setdiff(a, unique(col)))!=0)]

          cn1 <- colnames(df)
          df <- fastDummies::dummy_cols(.data = df, select_columns = colnames(subset_df), remove_selected_columns = TRUE)
          cn2 <- colnames(df)

          proportions <- setdiff(proportions, colnames(subset_df))

          proportions <- c(proportions, setdiff(cn2, cn1))

        }

      }

      df[proportions] <- lapply(df[proportions], as.numeric)

    }

  }


  # Check if means columns are numeric

  if(!is.null(means)) {

    for (i in 1:length(means)) {

      b <- as.vector(df %>% dplyr::select(means[[i]])) %>% t %>% c %>% unique

      if(all(varhandle::check.numeric(b))) {

        print(paste0(means[[i]], " has all numeric values."))

      } else {stop(paste0(means[[i]], " has non-numeric values, please check your entry."))}
    }
  }

  # Check if ratio columns are numeric
  if(!is.null(ratios_rates.numerators)) {

    if(is.null(ratios_rates.denominators)) {stop("If calculating ratios or rates, you must include both the numerator and denominator arguments. Please revise your input.")}

    if(length(ratios_rates.numerators) != length(ratios_rates.denominators)) {stop("You have a different number of numerators than denominators input for ratio or rate calculations. There should be one denominator specified for each numerator, and input in the same order which to match them.")}

    if(length(ratios_rates.numerators)>1) {

      if(!is.null(ratios_rates.numerators)) {

        for (i in 1:length(ratios_rates.numerators)) {

          a <- ratios_rates.numerators[[i]]

          if(all(varhandle::check.numeric(df[,a]))) {} else {stop(paste0("There are non-numeric values in ", ratios_rates.numerators[[i]], ". Please check your input."))}

        }

      }
      if(!is.null(ratios_rates.denominators)) {

        for (i in 1:length(ratios_rates.denominators)) {

          a <- ratios_rates.denominators[[i]]

          if(all(varhandle::check.numeric(df[,a]))) {} else {stop(paste0("There are non-numeric values in ", ratios_rates.denominators[[i]], ". Please check your input."))}

        }

      }

    } else if(length(ratios_rates.numerators)==1) {

      if(!all(varhandle::check.numeric(df[,ratios_rates.numerators]))) {stop(paste0("There are non-numeric values in ", ratios_rates.numerators, ". Please check your input."))}
      if(!all(varhandle::check.numeric(df[,ratios_rates.denominators]))) {stop(paste0("There are non-numeric values in ", ratios_rates.denominators, ". Please check your input."))}

    }

    # df <- df %>% mutate_at(.vars = c(ratios_rates.numerators, ratios_rates.denominators))
    df <- df %>% dplyr::mutate(dplyr::across(c(ratios_rates.numerators, ratios_rates.denominators), ~replace_na(.x, 0)))

    df[ratios_rates.numerators] <- lapply(df[ratios_rates.numerators], as.numeric)
    df[ratios_rates.denominators] <- lapply(df[ratios_rates.denominators], as.numeric)

    # change all NA values to 0s


    #
    # df[ratios_rates.numerators] <- replace_na(df[ratios_rates.numerators], replace = 0)
    # df[ratios_rates.denominators] <- replace_na(df[ratios_rates.denominators], replace = 0)

  }



  # Create survey design object
  if(!is.null(cluster)) {
    df <- df %>% dplyr::rename(cluster = {{cluster}})
    clstr <- dplyr::quo(paste0("cluster"))
  }
  if(!is.null(strata)) {strt <- dplyr::quo(strata)} else {
    df <- df %>% dplyr::mutate(strata = "All")
    strata <- "strata"
    strt <- dplyr::quo(strata)
  }
  if(!is.null(aggregation)) {print(class(aggregation))} else {
    df <- df %>% dplyr::mutate(aggregation = "All")
    aggregation <- "aggregation"
  }
  if(!is.null(svy_weights)) {wts <- dplyr::quo(svy_weights)}
  if(sample_design == "two_stage_cluster") {

    if(is.null(cluster)) {stop(paste0("A sample_design of ", sample_design, " requires a cluster variable. Please set the cluster argument and try again."))}
    if(is.null(svy_weights)) {df_survey <- srvyr::as_survey_design(.data = df, ids = !!clstr)} else (df_survey <- srvyr::as_survey_design(.data = df, ids = !!clstr, weights = !!wts))

  } else if(sample_design == "srs") {

    df_survey <- srvyr::as_survey_design(.data = df, ids = 1, weight = 1)

  } else if(sample_design == "two_stage_stratified") {

    if(is.null(strata)) {stop(paste0("A sample_design of ", sample_design, " requires a strata variable. Please set the strata argument and try again."))}
    if(is.null(svy_weights)) {df_survey <- srvyr::as_survey_design(.data = df, strata = !!strt)} else {df_survey <- srvyr::as_survey_design(.data = df, strata = !!strt, weights = !!wts)}

  } else if(sample_design == "two_stage_stratified_cluster") {

    if(is.null(strata) | is.null(cluster)) {stop(paste0("A sample_design of ", sample_design, " requires both a cluster and a strata variable. Please set these arguments and try again."))}
    if(is.null(svy_weights)) {df_survey <- srvyr::as_survey_design(.data = df, ids = !!clstr, strata = !!strt, nest = TRUE)} else {df_survey <- srvyr::as_survey_design(.data = df, cluster = !!clstr, strata = !!strt, weights = !!wts, nest = TRUE)}

  } else {stop("What the heck is this sample design argument? Invalid. Please use either 'srs', 'two_stage_cluster', 'two_stage_stratified', or 'two_stage_stratified_cluster'.")}

  # Analyse proportions

  if(!is.null(proportions)) {

    print("You are calculating proportions")

    results2 <- df_survey %>%
      srvyr::group_by(!!rlang::sym(aggregation)) %>%
      srvyr::summarise_at(.vars = proportions, srvyr::survey_mean, na.rm =  TRUE, vartype = "ci")

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }

  # Analyse means

  if(!is.null(means)) {

    print("You are calculating means")

    results2 <- df_survey %>%
      srvyr::group_by(!!rlang::sym(aggregation)) %>%
      srvyr::summarise_at(.vars = means, srvyr::survey_mean, na.rm =  TRUE, vartype = "ci")

    if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

  }

  # Analyse ratios and rates

  if(!is.null(ratios_rates.numerators)) {

    print("You are calculating rates and ratios")

    for (i in 1:length(ratios_rates.numerators)) {

      column_varname <- paste0(ratios_rates.numerators[[i]], "_rate_ratio")
      numerator_varname <- ratios_rates.numerators[[i]]
      denominator_varname <- ratios_rates.denominators[[i]]

      results2 <- df_survey %>%
        srvyr::group_by(!!rlang::sym(aggregation)) %>%
        srvyr::summarise(!!column_varname := srvyr::survey_ratio(numerator = !!rlang::sym(numerator_varname), denominator = !!rlang::sym(denominator_varname), na.rm = TRUE, vartype = "ci"))

      if(!is.null(ratios_rates.multiplier)) {results2[,-1] <- results2[,-1]*ratios_rates.multiplier}

      if(!exists("results")) {results <- results2} else {results <- merge(results, results2)}

    }

  }

  if(!is.null(ratios_rates.numerators)) {
    ratio_rates_vars <- paste0(ratios_rates.numerators, "_rate_ratio")
    if(!is.null(means)) {not_prop_vars <- c(means, ratio_rates_vars)} else {not_prop_vars = ratio_rates_vars}
  } else {
    if(!is.null(means)) {
      not_prop_vars <- means
    } else {not_prop_vars = NULL}
  }

  results <- healthyr::format_results_long(results, vars = colnames(results[-1]), not_proportions = not_prop_vars)

  if(!is.null(file_path)) {writexl::write_xlsx(results, file_path)}

  return(results)
}
