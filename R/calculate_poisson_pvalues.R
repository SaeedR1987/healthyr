#' Calculate Poisson Values
#'
#' Helper function checking how closely the distribution of cases across
#' clusters follows a poisson distribution. Returns a p-value with the null
#' hypothesis being that the distribution follows a poisson distribution. Dependency
#' with the nipnTK package, using the greensIndex function.
#'
#' @param df Inputs a dataframe.
#' @param strata Optional input of a character value specifying the strata or groupings to test whether the data
#' is different than poisson distribution.
#' @param cluster Input of a character value specifying the column name for cluster id.
#' @param case Input of a character value specifying the column name for the cases of interest.
#'
#' @return Returns a data frame with two columns for strata names and poisson p-values.
#' @export
#'
#' @examples
#' \dontrun{calculate_poisson_pvalues(df, strata = "admin2", cluster = "cluster_id",
#' case = "gam_wfhz_noflag")}
#'
#' @importFrom rlang .data
calculate_poisson_pvalues <- function(df, strata = NULL, cluster, case) {

  if(!is.null(strata)) {

    strata_list <- df[[strata]] %>% unique()

    df_poisson_results <- data.frame(matrix(ncol = 1, nrow = length(strata_list)))
    colnames(df_poisson_results) <- c("strata")
    df_poisson_results$strata <- strata_list
    poisson_pval <- (vector("double", length = length(strata_list)))

    for(i in seq_along(strata_list)) {

      strata_data <- df %>% dplyr::filter(!!rlang::sym(strata) == strata_list[i])
      sum_cases <- sum(strata_data[case], na.rm = TRUE)

      if(sum_cases==0 | nrow(strata_data) <30) {

        poisson_pval[i] <- NA

      } else {

        poisson_test <- df %>%
          dplyr::filter(!!rlang::sym(strata) == strata_list[i]) %>%
          dplyr::filter(!is.na(!!rlang::sym(case))) %>%
          nipnTK::greensIndex(psu = cluster, case = case)

        poisson_pval[i] <- mean(poisson_test[[5]])

      }

    }

  }

  df_poisson_results <- cbind(df_poisson_results, as.data.frame(poisson_pval))
  colnames(df_poisson_results)[colnames(df_poisson_results)=="strata"] <- strata
  return(df_poisson_results)

}
