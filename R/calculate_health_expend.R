#' Calculating Severe and Catastrophic Household Health Expenditures
#'
#' Independent function aimed to help calculate whether a household has severe (>10% of expenditures) or catastrophic (>25% of expenditures)
#' expenditures for health care.
#'
#' @param df Inputs a dataframe with monthly or period household expenditure information
#' @param monthly_expenditures Inputs a character vector of column names for columns of various household expenses in the previous 30 days.
#' @param period_expenditures Inputs a character vector of column names for columns of various household expenses in a previous, recall period, specified in the num_period_months paramater.
#' @param num_period_months Inputs a whole integer for the number of months of the period expenditures reported.
#' @param health_exp_col Inputs a character value for the name of the column specific for health household expenditures.
#'
#' @return Returns the dataframe with additional columns for severe and catastrophic health expenditures.
#' @export
#'
#' @examples
#' \dontrun{calculate_health_expend(df = myexpendituredata, monthly_expenditures =
#' c("food_exp", "gas_exp", water_exp", "health_exp"), health_exp_col = "health_exp"))}
#' \dontrun{calculate_health_expend(df = myexpendituredata, monthly_expenditures =
#' c("food_exp", "gas_exp", water_exp"), period_expenditures = c("shelter_repair_exp",
#' "education_exp", "health_exp"), num_period_months = 6, health_exp_col = "health_exp")}
#'
#' @importFrom rlang .data
calculate_health_expend <- function(df, monthly_expenditures, period_expenditures = NULL, num_period_months = NULL, health_exp_col) {

  if(is.null(health_exp_col)) {stop("Must specify a column for health expenses, to calculate health expenditure indicators.")}
  if(!is.vector(monthly_expenditures)) {stop("monthly_expenditures must be a vector of column names with monthly expenditures")}

  if(!is.null(period_expenditures)) {
    if(!is.vector(period_expenditures)) {stop("period_expenditures must be a vector of column names of expenditures over some period of time")}
    if(is.null(num_period_months)) {stop("If you are including a period_expenditures seperate from monthly expenditures, you must include numeric value for num_period of months that is the length of time in months for that period.")}

    df[monthly_expenditures] <- lapply(df[monthly_expenditures], as.numeric)
    df[period_expenditures] <- lapply(df[period_expenditures], as.numeric)

    df$month_exp1 <- rowSums(df[,monthly_expenditures], na.rm = TRUE)
    df$month_exp2 <- rowSums(df[,period_expenditures], na.rm = TRUE) / as.numeric(num_period_months)

    df$total_monthly_exp <- rowSums(df[,c("month_exp1", "month_exp2")])

    df <- df %>%
      dplyr::rename(health_exp = {{health_exp_col}}) %>%
      dplyr::mutate(health_exp = ifelse(is.na(.data$health_exp), 0, .data$health_exp / as.numeric(num_period_months)),
                    prop_health_exp = ifelse(is.na(.data$total_monthly_exp), NA , .data$health_exp / .data$total_monthly_exp) ,
                    severe_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp >= 0.10 & .data$prop_health_exp <=0.25, 1, 0)),
                    catastrophic_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp > 0.25, 1, 0)))

  } else {

    df[monthly_expenditures] <- lapply(df[monthly_expenditures], as.numeric)
    df$total_monthly_exp <- rowSums(df[,monthly_expenditures], na.rm = TRUE)

    df <- df %>%
      dplyr::rename(health_exp = {{health_exp_col}}) %>%
      dplyr::mutate(health_exp = ifelse(is.na(.data$health_exp), 0, .data$health_exp),
             prop_health_exp = ifelse(is.na(.data$total_monthly_exp), NA , .data$health_exp / .data$total_monthly_exp) ,
             severe_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp >= 0.10 & .data$prop_health_exp <=0.25, 1, 0)),
             catastrophic_health_exp =  ifelse(is.na(.data$prop_health_exp), NA, ifelse(.data$prop_health_exp > 0.25, 1, 0)))


  }

  return(df)

}
