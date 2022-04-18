#' Reformat Mortaltiy to ENA
#'
#' This function will take a standardized mortality dataset and reshape it for use in the ENA software.
#'
#' @param df Inputs a dataframe which has been standardized already by the format_mortality_current_census function.
#' @param file_path Inputs an optional character value for specifying a file path to save the output as an xlsx file.
#'
#' @return Returns a dataframe with wide mortality data, that is each row is a single household, which is formatted for ENA.
#' The resulting dataframe can be copy-pasted straight into the ENA program for analysis of mortality data.
#' @export
#'
#' @examples
#' format_mortality_to_ena(df = proc_mortality1)
#'
#' @importFrom rlang .data
format_mortality_to_ena <- function(df, file_path = NULL) {

  df <- df %>%
    dplyr::mutate(join = ifelse(is.na(.data$join), NA, ifelse(.data$join == "1", "y", .data$join)),
           left = ifelse(is.na(.data$left), NA, ifelse(.data$left == "1", "y", .data$left)),
           birth = ifelse(is.na(.data$birth), NA, ifelse(.data$birth == "1", "y", .data$birth)),
           death = ifelse(is.na(.data$death), NA, ifelse(.data$death == "1", "y", .data$death)),
    )


  #length(setdiff(c("sex", "oedema"), colnames(df)))>0

  if(length(setdiff(c("date_dc"), colnames(df)))>0) { df <- df %>% dplyr::mutate(date_dc = "")}
  if(length(setdiff(c("cluster"), colnames(df)))>0) { df <- df %>% dplyr::mutate(cluster = "")}
  if(length(setdiff(c("enum"), colnames(df)))>0) { df <- df %>% dplyr::mutate(enum = "")}
  if(length(setdiff(c("hh_id"), colnames(df)))>0) { df <- df %>% dplyr::mutate(hh_id = "")}
  if(length(setdiff(c("sex"), colnames(df)))>0) { df <- df %>% dplyr::mutate(sex = "")}

  # reformat sex

  df <- df %>% dplyr::mutate(sex = ifelse(.data$sex == "1", "m", ifelse(.data$sex == "2", "f", NA)))

  # create sequential cluster ID

  df <- df %>% dplyr::mutate(cluster = as.character(as.numeric(as.factor(.data$cluster))))

  # create numeric enumerator code

  df <- df %>% dplyr::mutate(enum = as.character(as.numeric(as.factor(.data$enum))))

  # create sequential HH ID

  df <- df %>% dplyr::group_by(.data$cluster) %>% dplyr::mutate(hh_id = as.character(as.numeric(as.factor(.data$hh_id)))) %>% dplyr::ungroup()

  # create sequential person ID

  df <- df %>%
    dplyr::group_by(.data$cluster, .data$hh_id) %>%
    dplyr::mutate(individual_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  #select and order relevant columns

  df <- df %>% dplyr::select(.data$date_dc_char, .data$cluster, .data$enum, .data$hh_id, .data$individual_id, .data$sex, .data$age_years, .data$join, .data$left, .data$birth, .data$death, .data$death_cause, .data$death_location) %>%
    dplyr::arrange(as.numeric(.data$cluster), as.numeric(.data$hh_id), as.numeric(.data$individual_id))

  # spread data

  df <- df %>%
    tidyr::gather(key = "key", value = "value", -c(.data$enum, .data$cluster,.data$hh_id, .data$individual_id, .data$date_dc_char), factor_key = TRUE) %>%
    dplyr::mutate(new_key = paste0(.data$individual_id, "_", .data$key),
           new_key = as.factor(.data$new_key),
           key = NULL, individual_id = NULL) %>%
    tidyr::spread(.data$new_key, .data$value) %>%
    dplyr::arrange(as.numeric(.data$cluster), as.numeric(.data$hh_id))

  col_order <- (c("admin1", "admin2", "date_dc_char", "cluster", "enum", "hh_id",
                  "1_sex", "1_age_years", "1_join", "1_left", "1_birth", "1_death", "1_death_cause", "1_death_location",
                  "2_sex", "2_age_years", "2_join", "2_left", "2_birth", "2_death", "2_death_cause", "2_death_location",
                  "3_sex", "3_age_years", "3_join", "3_left", "3_birth", "3_death", "3_death_cause", "3_death_location",
                  "4_sex", "4_age_years", "4_join", "4_left", "4_birth", "4_death", "4_death_cause", "4_death_location",
                  "5_sex", "5_age_years", "5_join", "5_left", "5_birth", "5_death", "5_death_cause", "5_death_location",
                  "6_sex", "6_age_years", "6_join", "6_left", "6_birth", "6_death", "6_death_cause", "6_death_location",
                  "7_sex", "7_age_years", "7_join", "7_left", "7_birth", "7_death", "7_death_cause", "7_death_location",
                  "8_sex", "8_age_years", "8_join", "8_left", "8_birth", "8_death", "8_death_cause", "8_death_location",
                  "9_sex", "9_age_years", "9_join", "9_left", "9_birth", "9_death", "9_death_cause", "9_death_location",
                  "10_sex", "10_age_years", "10_join", "10_left", "10_birth", "10_death", "10_death_cause", "10_death_location",
                  "11_sex", "11_age_years", "11_join", "11_left", "11_birth", "11_death", "11_death_cause", "11_death_location",
                  "12_sex", "12_age_years", "12_join", "12_left", "12_birth", "12_death", "12_death_cause", "12_death_location",
                  "13_sex", "13_age_years", "13_join", "13_left", "13_birth", "13_death", "13_death_cause", "13_death_location",
                  "14_sex", "14_age_years", "14_join", "14_left", "14_birth", "14_death", "14_death_cause", "14_death_location",
                  "15_sex", "15_age_years", "15_join", "15_left", "15_birth", "15_death", "15_death_cause", "15_death_location",
                  "16_sex", "16_age_years", "16_join", "16_left", "16_birth", "16_death", "16_death_cause", "16_death_location",
                  "17_sex", "17_age_years", "17_join", "17_left", "17_birth", "17_death", "17_death_cause", "17_death_location",
                  "18_sex", "18_age_years", "18_join", "18_left", "18_birth", "18_death", "18_death_cause", "18_death_location",
                  "19_sex", "19_age_years", "19_join", "19_left", "19_birth", "19_death", "19_death_cause", "19_death_location",
                  "20_sex", "20_age_years", "20_join", "20_left", "20_birth", "20_death", "20_death_cause", "20_death_location"))

  col_order <- intersect(col_order, colnames(df))

  df <- df[,col_order]

  df[,col_order] <- lapply(df[,col_order], as.character)

  df[is.na(df)] <- ""

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df, file_path)}

  return(df)

}
