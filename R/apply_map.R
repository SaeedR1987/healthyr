
#' Apply Map
#'
#' @param df A dataframe object to recode using a map dataframe.
#' @param map A map dataframe matching new and old values for columns to recode.
#'
#' @return Returns a recoded dataframe in-line with the map file.
#' @export
#'
#' @examples
apply_map <- function(df, map) {

  # Check if map is applicable for the dataset.
  if(!check_map_applicable(df, map)) {
    stop("Map is not applicable for this dataframe.")
  }

  # first determine the list of variables that needs updating

  vars_to_recode <- intersect(names(df), map$variable)

  map <- map %>% dplyr::filter(map$variable %in% vars_to_recode)

  vars_only_map <- map %>% dplyr::select(variable, new_variable) %>% dplyr::distinct()

  # Loop through each variable to recode name and values
  for (i in 1:length(vars_to_recode)) {

    print(vars_to_recode[[i]])

    a <- vars_only_map %>% dplyr::filter(variable == vars_to_recode[[i]]) %>% dplyr::pull(.data$new_variable)

    print(a)

    names(df)[names(df) == vars_to_recode[[i]]] <- a

    print("variable has been renamed now")

    values_to_recode <- df[a] %>% t %>% c %>% unique

    print("recode values have been listed")

    # Loop through each value of each variable to recode
    for (m in 1:length(values_to_recode)) {

      print(values_to_recode[[m]])

      new_value_recode <- map %>% dplyr::filter(new_variable == a & value == values_to_recode[[m]]) %>% dplyr::pull(.data$new_value)

      print(new_value_recode)

      df <- healthyr::recode_helper_data(df = df, column = a, old_val = values_to_recode[[m]], new_val = new_value_recode)

    }

  }

  return(df)

}

