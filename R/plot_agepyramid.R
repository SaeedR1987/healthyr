#' Plot Age Pyramid
#'
#' This function plots an age pyramid based on specified age groupings and sex.
#'
#' @param df Inputs a dataframe that has been processed and standardized by either the
#' healthyr::format_mortality_current census or healthyr::format_nut_health_indicators
#' functions.
#' @param age_grouping Inputs an optional character value specifying a column name with
#' age groupings. If nothing specified, it will use the recommended default 'age_group'
#' which are 5-year groupings created by the healthyr functions.
#' @param filtering Inputs an optional character value specifying a variable to filter
#' the data for. This is useful if you want to only see the age pyramid for individuals
#' meeting a certain criteria, like disability status. The filtering variable should be
#' coded numerically as 0s and 1s, with the 1s being the population you want to plot.
#' @param file_path Inputs an optional character value specifying the file path location to save
#' an image of the plot. Recommended to specify as a .jpg or .png file.
#' @param wdth Inputs an optional numeric value for the width of the image.
#' @param hght Inputs an optional numeric value fo the height of the image.
#' @param title_name Inputs an optional character value for the title of the plot.
#'
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{plot_agepyramid(df, age_grouping = "age_group", filtering = "disability3")}
#' @importFrom rlang .data
plot_agepyramid <- function(df, age_grouping = NULL, filtering = NULL, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  if(is.null(age_grouping)) {age_grouping <- "age_group"}

  if(!is.null(filtering)) {
    print("Please note, if using filtering the variable must be coded numerically by 0s and 1s")
    df <- df %>%
      dplyr::filter(!is.na(.data$age_years)) %>%
      dplyr::filter(!!rlang::sym(filtering)==1) %>%
      dplyr::arrange(.data$sex)

    g <- apyramid::age_pyramid(data = df,
                               age_group = age_grouping,
                               proportional = TRUE) + ggplot2::ylab(paste0("Proportion of ", filtering))
  } else {

    df <- df %>%
      dplyr::arrange(.data$sex)

    g <- apyramid::age_pyramid(data = df,
                               age_group = age_grouping,
                               proportional = TRUE) + ggplot2::ylab(paste0("Proportion of Population "))

  }

  g <- g + ggplot2::scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("#08bcc4", "#ff746c"))

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)
}
