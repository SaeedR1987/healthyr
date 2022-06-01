
#' Run Statistical Tests
#'
#' Summary function to run multiple hypothesis tests over a survey dataset in line with
#' IMPACT Quantitative Data Analysis Guidelines for Significance Testing
#'
#' @param df Inputs the dataset
#' @param aggregation Inputs a character value specifying the column name of the variable for the groups you want to compare.
#' @param svy_weights Inputs a character value specifying the column name of the survey weights column, if applicable.
#' @param strata Inputs a character value specifying the column name of the survey strata column, if applicable.
#' @param sample_design Inputs a character value specifying the overall survey design. Options include 'srs'
#' for simple random sampling, 'two_stage_cluster', 'two_stage_stratified', and 'two_stage_stratified_cluster'.
#' @param cluster Inputs a character value specifying the column name of hte survey cluster column, if applicable.
#' @param categorical_vars Inputs a character vector of the column names of all the binary or categorical variables
#' to test for differences across the specified group/aggregation.
#' @param numerical_vars Inputs a character vector of the column names of all the numerical variables
#' to test for differences across the specified group/aggregation.
#' @param regression_outcome Inputs a character value of the column name of the variable you want to treat as a regression outcome.
#' @param regression_predictors Inputs a character vector of the column names of the variables you want to treat as a regression predictors.
#'
#' @return Returns a summary dataframe of the comparisons made, tests run, and p-values.
#' @export
#'
#' @examples
#' \dontrun{run_statistical_tests(df = mydata, aggregation = "sex_hoh", sample_design = "srs",
#'  categorical_vars = c("hhs_severe", "improved_water_source"))}
run_statistical_tests <- function(df = df,
                                  aggregation = NULL,
                                  svy_weights = NULL,
                                  strata = NULL,
                                  sample_design = NULL,
                                  cluster = NULL,
                                  categorical_vars = NULL,
                                  numerical_vars = NULL,
                                  regression_outcome = NULL,
                                  regression_predictors = NULL) {

  if(dplyr::is_grouped_df(df)) {df <- df %>% dplyr::ungroup()}

  # Check if proportions coded as 0 / 1

  if(!is.null(categorical_vars)) {

    proportion_values <- df %>% dplyr::select(categorical_vars) %>% t %>% c %>% unique

    if(length(setdiff(proportion_values, c(1, 0)))==0) {

      print("Good - proportion values are coded as 1/0 for yes/no")

    } else {

      categorical_vars_list <- categorical_vars

      for (i in 1:length(categorical_vars_list)) {

        col_values <- df %>% dplyr::select(categorical_vars_list[[i]]) %>% t %>% c %>% unique
        print(col_values)


        if(length(setdiff(col_values, c(1,0,NA)))==0) {

          print(paste0("Good, ", categorical_vars_list[[i]], " are already coded as 1/0."))

        } else if(length(setdiff(col_values, c(1,2,9,NA)))==0) {

          print(paste0("Values for, ", categorical_vars_list[[i]], " are 1/2/9. Recoding to 1/0."))

          df <- df %>%
            dplyr::mutate(dplyr::across(categorical_vars_list[[i]], ~ (ifelse(. == 2, 0, . ))),
                          dplyr::across(categorical_vars_list[[i]], ~ (ifelse(. == 9, 0, . ))))

        } else {

          print(paste0("Values for, ", categorical_vars_list[[i]], " are neither 1/2/9 or 1/0. Recoding to 1/0 with dummy variables."))

          subset_df <- df %>% dplyr::select(categorical_vars_list[[i]])

          a <- c(1,0,NA)

          subset_df <- subset_df[,sapply(subset_df, function(col) length(setdiff(a, unique(col)))!=0)]

          cn1 <- colnames(df)
          df <- fastDummies::dummy_cols(.data = df, select_columns = colnames(subset_df), remove_selected_columns = TRUE)
          cn2 <- colnames(df)

          categorical_vars <- setdiff(categorical_vars, colnames(subset_df))

          categorical_vars <- c(categorical_vars, setdiff(cn2, cn1))

        }

      }

      df[categorical_vars] <- lapply(df[categorical_vars], as.numeric)

    }

  }

  # Check if means columns are numeric

  if(!is.null(numerical_vars)) {

    for (i in 1:length(numerical_vars)) {

      b <- as.vector(df %>% dplyr::select(numerical_vars[[i]])) %>% t %>% c %>% unique

      if(all(varhandle::check.numeric(b))) {

        print(paste0(numerical_vars[[i]], " has all numeric values."))

      } else {stop(paste0(numerical_vars[[i]], " has non-numeric values, please check your entry."))}
    }
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

  # Run Statistical Tests

  if(!is.null(categorical_vars)) {

    for (i in 1:length(categorical_vars)) {
      print(i)
      frml <- as.formula(paste0("~", aggregation, "+", categorical_vars[[i]]))

      count <- sum(!is.na(df[categorical_vars[[i]]]))

      difvals <- df %>% dplyr::select(categorical_vars[[i]]) %>% t %>% c %>% unique

      nms <- c("group", "variable", "test", "P-value")
      grp <- aggregation
      vrs <- c(categorical_vars[[i]])
      tst <- ifelse(count < 5 | length(difvals) <= 1, "test not run", srvyr::svychisq(formula = frml, design = df_survey)[4])
      pval <- ifelse(count < 5 | length(difvals) <= 1, NA, as.numeric(srvyr::svychisq(formula = frml, design = df_survey)[3]))

      stat_result <- data.frame(grp, vrs, tst, pval)
      names(stat_result) <- nms

      if(!exists("results")) {results <- stat_result} else {results <- merge(results, stat_result, all = TRUE)}

    }
  }

  if(!is.null(numerical_vars)) {

    for (i in 1:length(numerical_vars)) {

      frml <- as.formula(paste0(numerical_vars[[i]],"~", aggregation))

      nms <- c("group", "variable", "test", "P-value")
      grp <- aggregation
      vrs <- c(numerical_vars[[i]])
      tst <- ifelse(count < 5, "test not run", survey::svyttest(formula = frml, design = df_survey)[6])
      pval <- ifelse(count < 5, NA_real_, as.numeric(survey::svyttest(formula = frml, design = df_survey)[8]))

      #
      # survey::svyttest(formula = frml, design = df_survey)

      stat_result <- data.frame(grp, vrs, tst, pval)
      names(stat_result) <- nms

      if(!exists("results")) {results <- stat_result} else {results <- merge(results, stat_result, all = TRUE)}

    }

  }

  return(results)
}
