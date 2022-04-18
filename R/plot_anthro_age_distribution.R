#' Plot Age Distribution of Nutritional Status of Children
#'
#' @param df Inputs a dataframe with anthropometric data that has already been processed
#' and standardized by healthyr::format_nut_health_results function.
#' @param index Inputs a character value specifying which anthropometric index or result to plot.
#' Options include wfhz, hfaz, wfaz, mfaz, muac, or cgam.
#' @param file_path Inputs an optional character value specifying the file path location to save
#' an image of the plot. Recommended to specify as a .jpg or .png file.
#' @param wdth Inputs an optional numeric value for the width of the image.
#' @param hght Inputs an optional numeric value fo the height of the image.
#' @param title_name Inputs an optional character value for the title of the plot.
#'
#' @return Returns a ggplot object.
#' @export
#'
#' @examples
#' \dontrun{plot_anthro_age_distribution(df, index = "wfhz",
#' file_path = "myplots/wfhz_age_distribution.jpg")}
#' @importFrom rlang .data
plot_anthro_age_distribution <- function(df, index, file_path = NULL, wdth = NULL, hght = NULL, title_name = NULL) {

  if(length(setdiff(c("age_months"), colnames(df)))>0) { df <- df %>% dplyr::mutate(age_months = "")} else {
    df <- df %>% dplyr::mutate(age_months = ifelse(.data$age_months - floor(.data$age_months) >=0.96, as.numeric(ceiling(.data$age_months)), as.numeric(floor(.data$age_months))))
    }

  df <- df %>%
    dplyr::mutate(age_group = ifelse(is.na(.data$age_months), NA,
                              ifelse(.data$age_months < 6, "<6 months",
                                     ifelse(.data$age_months > 5 & .data$age_months <18, "6-17 months",
                                            ifelse(.data$age_months >17 & .data$age_months <30, "18-29 months",
                                                   ifelse(.data$age_months >29 & .data$age_months < 42, "30-41 months",
                                                          ifelse(.data$age_months > 41 & .data$age_months < 54, "42-53 months",
                                                                 ifelse(.data$age_months >53 & .data$age_months < 60, "54-59 months",
                                                                        ifelse(.data$age_months > 59, ">59 months", NA)))))))),
           age_group = as.factor(.data$age_group),
           age_group = factor(.data$age_group, levels = c("<6 months", "6-17 months", "18-29 months", "30-41 months", "42-53 months", "54-59 months")))

  if(index == "muac") {

    df <- df %>%
      dplyr::filter(.data$muac_flag != 1) %>%
      dplyr::mutate(cat = ifelse(is.na(.data$gam_muac_noflag), NA, ifelse(.data$sam_muac_noflag == "1", "SAM", ifelse(.data$mam_muac_noflag == "1", "MAM", ifelse(.data$gam_muac_noflag == "0", "Normal", NA)))),
             cat = as.factor(.data$cat), cat = factor(cat, levels = c("SAM", "MAM", "Normal")))

    title <- "Nutritional Status: MUAC by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c("SAM" = "indianred", "MAM" = "khaki", "Normal" = "palegreen4")

  } else if(index == "mfaz") {

    df <- df %>%
      dplyr::filter(.data$mfaz_smart_flag != 1) %>%
      dplyr::mutate(cat = ifelse(is.na(.data$global_mfaz_noflag), NA, ifelse(.data$severe_mfaz_noflag == "1", "SAM", ifelse(.data$moderate_mfaz_noflag == "1", "MAM", ifelse(.data$global_mfaz_noflag == "0", "Normal", NA)))),
             cat = as.factor(.data$cat), cat = factor(.data$cat, levels = c("SAM", "MAM", "Normal")))

    title <- "Nutritional Status: MUAC-for-Age by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c("SAM" = "indianred", "MAM" = "khaki", "Normal" = "palegreen4")

  } else if(index == "wfhz") {

    df <- df %>%
      dplyr::filter(.data$wfhz_smart_flag != 1) %>%
      dplyr::mutate(gam_wfhz_noflag = as.character(.data$gam_wfhz_noflag), mam_wfhz_noflag = as.character(.data$mam_wfhz_noflag), sam_wfhz_noflag = as.character(.data$sam_wfhz_noflag)) %>%
      dplyr::mutate(cat = ifelse(is.na(.data$gam_wfhz_noflag), NA, ifelse(.data$sam_wfhz_noflag == "1", "SAM", ifelse(.data$mam_wfhz_noflag == "1", "MAM", ifelse(.data$gam_wfhz_noflag == "0", "Normal", NA)))),
             cat = as.factor(.data$cat), cat = factor(.data$cat, levels = c("SAM", "MAM", "Normal")))

    title <- "Nutritional Status: Weight-for-Height by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c("SAM" = "indianred", "MAM" = "khaki", "Normal" = "palegreen4")

  } else if(index == "hfaz") {

    df <- df %>%
      dplyr::filter(.data$hfaz_smart_flag != 1) %>%
      dplyr::mutate(cat = ifelse(is.na(.data$global_stunting_noflag), NA, ifelse(.data$severe_stunting_noflag == "1", "Severe Stunting", ifelse(.data$moderate_stunting_noflag == "1", "Moderate Stunting", ifelse(.data$global_stunting_noflag == "0", "Normal", NA)))),
             cat = as.factor(.data$cat), cat = factor(.data$cat, levels = c("Severe Stunting", "Moderate Stunting", "Normal")))

    title <- "Nutritional Status: Stunting by Age Group"
    color_groups <- c("Severe Stunting", "Moderate Stunting", "Normal")
    color_palette <- c("Severe Stunting" = "indianred", "Moderate Stunting" = "khaki", "Normal" = "palegreen4")

  } else if(index == "wfaz") {

    df <- df %>%
      dplyr::filter(.data$wfaz_smart_flag != 1) %>%
      dplyr::mutate(cat = ifelse(is.na(.data$global_underweight_noflag), NA, ifelse(.data$severe_underweight_noflag == "1", "Severe Underweight", ifelse(.data$moderate_underweight_noflag == "1", "Moderate Underweight", ifelse(.data$global_underweight_noflag == "0", "Normal", NA)))),
             cat = as.factor(.data$cat), cat = factor(.data$cat, levels = c("Severe Underweight", "Moderate Underweight", "Normal")))

    title <- "Nutritional Status: Underweight by Age Group"
    color_groups <- c("Severe Underweight", "Moderate Underweight", "Normal")
    color_palette <- c("Severe Underweight" = "indianred", "Moderate Underweight" = "khaki", "Normal" = "palegreen4")

  } else if(index == "cgam") {

    df <- df %>%
      dplyr::filter(.data$wfhz_smart_flag != 1) %>%
      dplyr::mutate(cat = ifelse(is.na(.data$c_gam), NA, ifelse(.data$c_sam == "1", "SAM", ifelse(.data$c_mam == "1", "MAM", ifelse(.data$c_gam == "0", "Normal", NA)))),
             cat = as.factor(.data$cat), cat = factor(.data$cat, levels = c("SAM", "MAM", "Normal")))

    title <- "Nutritional Status: Combined GAM by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c("SAM" = "indianred", "MAM" = "khaki", "Normal" = "palegreen4")

  }

  df2 <- df %>%
    dplyr::group_by(.data$age_group, .data$cat) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(pct = .data$n / sum(.data$n)) %>%
    dplyr::rename(`Nutritional Status` = .data$cat)

  g <- ggplot2::ggplot(data = df2, ggplot2::aes(fill = .data$`Nutritional Status`, x = .data$age_group, y = .data$pct, order = .data$`Nutritional Status`)) +
  ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::scale_y_continuous(labels = scales::percent, name = "Nutrition Status") +
    ggplot2::scale_fill_manual(values = color_palette) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(.data$pct,3)*100, "% - (", .data$n, ")" )),
              position = ggplot2::position_stack(vjust = 0.5), size = 3)

  if(!is.null(title_name)) {g <- g + ggplot2::ggtitle(title_name)}

  if(is.null(wdth)) {wdth <- 5}
  if(is.null(hght)) {hght <- 5}

  if(!is.null(file_path)) {ggplot2::ggsave(filename = file_path, width = wdth, height = hght)}

  return(g)

}
