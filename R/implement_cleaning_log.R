#' Implement Cleaning Log
#'
#' Function to read a REACH-style cleaning log and a dataframe and produce a cleaned
#' dataset based on the cleaning log.
#'
#' @param df Inputs a dataframe of raw data.
#' @param uuid_col Inputs a character value specifying the column name for uuid, or
#' whichever unique record level value which to target cleaning.
#' @param cleaning_log Inputs a dataframe that is the REACH-style cleaning log.
#'
#' @return Returns a dataframe of the clean dataset.
#' @export
#'
#' @examples
#' \dontrun{implement_cleaning_log(df = rawdata, uuid_col = "uuid",
#' cleaning_log = mycleaninglog)}
#'
#' @importFrom rlang .data
implement_cleaning_log <- function(df, uuid_col, cleaning_log) {

  df <- df %>% dplyr::rename(uuid_col = {{uuid_col}})

  if(nrow(cleaning_log) > 0) {

    cols_to_clean <- unique(cleaning_log$question.name)

    df[cols_to_clean] <- lapply(df[cols_to_clean], as.character)

    # cleaning households

    for(i in 1:nrow(cleaning_log)){
      if(cleaning_log[["changed"]][i]=="yes"& !is.na(cleaning_log[["changed"]][i])){
        uuid_i <- cleaning_log$uuid[i]
        var_i <- cleaning_log$question.name[i]
        old_i <- cleaning_log$old.value[i]
        new_i <- cleaning_log$new.value[i]

        if(is.na(new_i)) {

          df[df$uuid_col == uuid_i, var_i] <- NA_character_

        }else{

          df[df$uuid_col == uuid_i, var_i] <- new_i
          print(paste0("The variable ", var_i, " was changed from ", old_i, " to ", new_i, " for id: ", uuid_i))
        }
      }
    }

  }



  return(df)

}
