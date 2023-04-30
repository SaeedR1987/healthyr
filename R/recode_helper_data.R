recode_helper_data <- function(df, column, old_val, new_val) {

  df <- df %>% dplyr::mutate_at(
    dplyr::vars(column),
    dplyr::funs(as.character))

  if(!is.na(old_val)){

    if(new_val == "NA") {

      df <- df %>%
        dplyr::mutate_at(
          dplyr::vars(column),
          dplyr::funs(dplyr::case_when(. == old_val ~ NA_character_,
                            TRUE ~ .)))


    } else {

      df <- df %>%
        dplyr::mutate_at(
          dplyr::vars(column),
          dplyr::funs(dplyr::case_when(. == old_val ~ new_val,
                                TRUE ~ .)))
    }
  }

  cat("\014") #clears the console
  print(paste("The input ", new_val, " is replacing ", old_val, " for the variable: ", column))

  return(df)

}


