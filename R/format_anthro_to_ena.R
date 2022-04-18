#' Format Anthropometric Data to ENA Format
#'
#' @param df Inputs a dataframe that has been already formatted by the format_nut_health_indicators function.
#'
#' @return Returns a dataframe which is formatted so it can be copy-pasted into the anthropometric section of the ENA software.
#' @export
#'
#' @examples
#' format_anthro_to_ena(df = proc_anthro1)
#'
#' @importFrom rlang .data
format_anthro_to_ena <- function(df) {

  if(length(setdiff(c("date_dc"), colnames(df)))>0) { df <- df %>% dplyr::mutate(date_dc = "")}
  if(length(setdiff(c("cluster"), colnames(df)))>0) { df <- df %>% dplyr::mutate(cluster = "")}
  if(length(setdiff(c("enum"), colnames(df)))>0) { df <- df %>% dplyr::mutate(enum = "")}
  if(length(setdiff(c("individual_id"), colnames(df)))>0) { df <- df %>% dplyr::mutate(individual_id = "")}
  if(length(setdiff(c("hh_id"), colnames(df)))>0) { df <- df %>% dplyr::mutate(hh_id = "")}
  if(length(setdiff(c("sex"), colnames(df)))>0) { df <- df %>% dplyr::mutate(sex = "")}
  if(length(setdiff(c("dob"), colnames(df)))>0) { df <- df %>% dplyr::mutate(dob = "")}
  if(length(setdiff(c("age_months"), colnames(df)))>0) { df <- df %>% dplyr::mutate(age_months = "")} else {df <- df %>% dplyr::mutate(age_months = ifelse(.data$age_months - floor(.data$age_months) >=0.96, as.character(ceiling(.data$age_months)), as.character(floor(.data$age_months))))}
  if(length(setdiff(c("weight"), colnames(df)))>0) { df <- df %>% dplyr::mutate(weight = "")}
  if(length(setdiff(c("height"), colnames(df)))>0) { df <- df %>% dplyr::mutate(height = "")}
  if(length(setdiff(c("oedema"), colnames(df)))>0) { df <- df %>% dplyr::mutate(oedema = "")} else {df <- df %>% dplyr::mutate(oedema = ifelse(is.na(.data$oedema), "", as.character(.data$oedema)))}
  if(length(setdiff(c("muac"), colnames(df)))>0) { df <- df %>% dplyr::mutate(muac = "")}

  # convert all relevant columns to character for easy copy pasting
  df[c("date_dc", "cluster", "enum", "individual_id", "hh_id", "sex", "dob", "age_months", "weight", "height", "oedema", "muac")] <- lapply(df[c("date_dc", "cluster", "enum", "individual_id", "hh_id", "sex", "dob", "age_months", "weight", "height", "oedema", "muac")], as.character)

  # reformat sex

  df <- df %>% dplyr::mutate(sex = ifelse(.data$sex == "1", "m", ifelse(.data$sex == "2", "f", NA)))

  # convert muac to mm

  df <- df %>% dplyr::mutate(muac = as.character(as.numeric(.data$muac)*10))

  #create sequential cluster IDs

  df <- df %>% dplyr::mutate(cluster = as.character(as.numeric(as.factor(.data$cluster))))

  # create enum id

  df <- df %>% dplyr::mutate(enum = as.character(as.numeric(as.factor(.data$enum))))

  #create sequential hh IDs

  df <- df %>% dplyr::group_by(.data$cluster) %>% dplyr::mutate(hh_id = as.character(as.numeric(as.factor(.data$hh_id)))) %>% dplyr::ungroup()

  #create sequential child IDs

  df <- df %>% dplyr::group_by(.data$cluster, .data$hh_id) %>%
    dplyr::mutate(individual_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  #select and order relevant columns

  df <- df %>% dplyr::select(.data$date_dc_char, .data$cluster, .data$enum, .data$individual_id, .data$hh_id, .data$sex, .data$dob_char, .data$age_months, .data$weight, .data$height, .data$oedema, .data$muac) %>%
    dplyr::arrange(as.numeric(.data$cluster), as.numeric(.data$hh_id), as.numeric(.data$individual_id))

  df[,names(df)] <- lapply(df[,names(df)], as.character)

  df[is.na(df)] <- ""

  return(df)

}
