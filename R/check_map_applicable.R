#' Check Map Applicable
#'
#' Check if the map file is applicable for use with the associated dataset.
#' If not applicable, will throw an error explaining why.
#' If applicable, will return TRUE
#'
#' @param df A dataframe dataset to check against the map dataframe.
#' @param map A map dataframe to check against the dataset.
#'
#' @return Returns a Boolean TRUE if the map is applicable for the given dataset.
#' Otherwise will throw an error and stop.
#' @export
#'
#' @examples
check_map_applicable <- function(df, map) {

  # to check for the appropriate map format of the map file.

  if(length(setdiff(c("variable", "value", "new_variable", "new_value"), names(map))) !=0 ) {
    print(paste0("The following names are in the map: "))
    print(paste0(names(map)))
    stop("The map file should only have 4 columns: variable, value, new_variable, new_value. This is not the case, please check your input.")
  }
  print("OK - format of map dataframe is correct.")

  # to check for uniqueness of the rows in the map file.

  if(map %>% dplyr::mutate(test = paste0(.data$variable, .data$value, .data$new_variable, .data$new_value)) %>% dplyr::pull(.data$test) %>% duplicated() %>% sum() > 0) {
    stop("You have duplicate rows in your data map. Each row should be a unique combination, please check your input.")
  }
  print("OK - all map rows are unique.")


# are all variable names in the dataset
if(length(setdiff(unique(map$variable), names(df))) != 0 ) {
  stop(paste0("Not all variables in the data "))
}
print("OK - All variable names are in the dataset.")

# check for each mapped variable, if all the values in the dataset are also in the map.
if(length(unique(map$variable)) != 0) {

  vars <- unique(map$variable)

  for (i in 1:length(vars)) {

    a <- map %>% dplyr::filter(.data$variable == vars[[i]]) %>% dplyr::select(.data$value) %>% t %>% c %>% unique

    if(!all(df %>% dplyr::pull(vars[[i]]) %in% a)) {
      stop("For the variable ", vars[[i]], " there are values in the dataset that are not in your data map. Please run the full format_nut_health_indicators to re-code these and create a new data map.")
    }

  }

}
print("OK - all mapped values are present in the dataset.")


print("This map is applicable for this dataframe.")

return(TRUE)

}
