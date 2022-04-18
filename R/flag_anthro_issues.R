#' Flag Anthropometric Issues
#'
#' This function creates SMART and WHO flags in an anthropometric dataset, using the mean and sd within the grouping provided for SMART flags.
#'
#' @param df Inputs a data frame with anthropometric data that has already been processed by the format_nut_health_indicators and reformat_nut_health_indicators
#' functions.
#' @param grouping Inputs a character value specifying a column name to indicate the groupings for SMART flag creation.
#' @param file_path Inputs an optional character value with the file path for saving an xlsx file of the output
#'
#' @return Returns a data frame with additional columns for WHO and SMART flags
#' @export
#'
#' @examples
#' flag_anthro_issues(df = proc_anthro1, grouping = "county")
#'
#' @importFrom rlang .data
flag_anthro_issues <- function(df, grouping = NULL, file_path = NULL) {

  anthro_cols <- c("wfhz", "hfaz", "wfaz", "mfaz", "muac", "weight", "height", "age")

  if(length(setdiff(anthro_cols, colnames(df)))==0) {stop("No anthropometric columns were included, so no flags can be created. Please check your input.")}

  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }

  # Create z-score flags

    #WHZ flags
    if(c("wfhz") %in% colnames(df)) {

      df <- df %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_wfhz = round(mean(.data$wfhz, na.rm = TRUE),3),
               sd_wfhz = round(stats::sd(.data$wfhz, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(wfhz_smart_flag = ifelse(is.na(.data$wfhz), NA, ifelse(.data$oedema == "y", 0, ifelse(.data$wfhz < .data$mean_wfhz - 3 | .data$wfhz > .data$mean_wfhz + 3, 1, 0))),
               wfhz_who_flag = ifelse(is.na(.data$wfhz), NA, ifelse(.data$wfhz < -5 | .data$wfhz > 5, 1, 0)),
               wfhz_noflag = ifelse(is.na(.data$wfhz) | .data$wfhz_smart_flag == 1, NA, .data$wfhz),
               wfhz_noflag = ifelse(.data$oedema == "y", NA, .data$wfhz_noflag)) %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_wfhz_noflag = round(mean(.data$wfhz_noflag, na.rm = TRUE),3),
               sd_wfhz_noflag = round(stats::sd(.data$wfhz_noflag, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(gam_wfhz_noflag = ifelse(is.na(.data$gam_wfhz), NA, ifelse(is.na(.data$wfhz_smart_flag), .data$gam_wfhz, ifelse(.data$wfhz_smart_flag == 1, NA, .data$gam_wfhz))),
               mam_wfhz_noflag = ifelse(is.na(.data$mam_wfhz), NA, ifelse(is.na(.data$wfhz_smart_flag), .data$mam_wfhz, ifelse(.data$wfhz_smart_flag == 1, NA, .data$mam_wfhz))),
               sam_wfhz_noflag = ifelse(is.na(.data$sam_wfhz), NA, ifelse(is.na(.data$wfhz_smart_flag), .data$sam_wfhz, ifelse(.data$wfhz_smart_flag == 1, NA, .data$sam_wfhz))),

               gam_wfhz_noflag = ifelse(.data$oedema == "y", 1, .data$gam_wfhz_noflag),
               sam_wfhz_noflag = ifelse(.data$oedema == "y", 1, .data$sam_wfhz_noflag))
    }

    #HAZ flags
    if(c("hfaz") %in% colnames(df)) {

      df <- df %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_hfaz = round(mean(.data$hfaz, na.rm = TRUE),3),
               sd_hfaz = round(stats::sd(.data$hfaz, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(hfaz_smart_flag = ifelse(is.na(.data$hfaz), NA, ifelse(.data$hfaz < .data$mean_hfaz - 3 | .data$hfaz > .data$mean_hfaz + 3, 1, 0)),
               hfaz_who_flag = ifelse(is.na(.data$hfaz), NA, ifelse(.data$hfaz < -5 | .data$hfaz > 5, 1, 0)),
               hfaz_noflag = ifelse(is.na(.data$hfaz) | .data$hfaz_smart_flag == 1, NA, .data$hfaz)) %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_hfaz_noflag = round(mean(.data$hfaz_noflag, na.rm = TRUE),3),
               sd_hfaz_noflag = round(stats::sd(.data$hfaz_noflag, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(global_stunting_noflag = ifelse(is.na(.data$global_stunting), NA, ifelse(is.na(.data$hfaz_smart_flag), .data$global_stunting, ifelse(.data$hfaz_smart_flag == 1, NA, .data$global_stunting))),
               moderate_stunting_noflag = ifelse(is.na(.data$moderate_stunting), NA, ifelse(is.na(.data$hfaz_smart_flag), .data$moderate_stunting, ifelse(.data$hfaz_smart_flag == 1, NA, .data$moderate_stunting))),
               severe_stunting_noflag = ifelse(is.na(.data$severe_stunting), NA, ifelse(is.na(.data$hfaz_smart_flag), .data$severe_stunting, ifelse(.data$hfaz_smart_flag == 1, NA, .data$severe_stunting))))
    }

    #WFA flags
    if(c("wfaz") %in% colnames(df)) {

      df <- df %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_wfaz = round(mean(.data$wfaz, na.rm = TRUE),3),
               sd_wfaz = round(stats::sd(.data$wfaz, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(wfaz_smart_flag = ifelse(is.na(.data$wfaz), NA, ifelse(.data$wfaz < .data$mean_wfaz - 3 | .data$wfaz > .data$mean_wfaz + 3, 1, 0)),
               wfaz_who_flag = ifelse(is.na(.data$wfaz), NA, ifelse(.data$wfaz < -5 | .data$wfaz > 5, 1, 0)),
               wfaz_noflag = ifelse(is.na(.data$wfaz) | .data$wfaz_smart_flag == 1, NA, .data$wfaz)) %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_wfaz_noflag = round(mean(.data$wfaz_noflag, na.rm = TRUE),3),
               sd_wfaz_noflag = round(stats::sd(.data$wfaz_noflag, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(global_underweight_noflag = ifelse(is.na(.data$global_underweight), NA, ifelse(is.na(.data$wfaz_smart_flag), .data$global_underweight, ifelse(.data$wfaz_smart_flag == 1, NA, .data$global_underweight))),
               moderate_underweight_noflag = ifelse(is.na(.data$moderate_underweight), NA, ifelse(is.na(.data$wfaz_smart_flag), .data$moderate_underweight, ifelse(.data$wfaz_smart_flag == 1, NA, .data$moderate_underweight))),
               severe_underweight_noflag = ifelse(is.na(.data$severe_underweight), NA, ifelse(is.na(.data$wfaz_smart_flag), .data$severe_underweight, ifelse(.data$wfaz_smart_flag == 1, NA, .data$severe_underweight))))
    }

    #MFA flags
    if(c("mfaz") %in% colnames(df)) {

      df <- df %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_mfaz = round(mean(.data$mfaz, na.rm = TRUE),3),
               sd_mfaz = round(stats::sd(.data$mfaz, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(mfaz_smart_flag = ifelse(is.na(.data$mfaz), NA, ifelse(.data$mfaz < .data$mean_mfaz - 3 | .data$mfaz > .data$mean_mfaz + 3, 1, 0)),
               mfaz_who_flag = ifelse(is.na(.data$mfaz), NA, ifelse(.data$mfaz < -5 | .data$mfaz > 5, 1, 0)),
               mfaz_noflag = ifelse(is.na(.data$mfaz) | .data$mfaz_smart_flag == 1, NA, .data$mfaz)) %>%
        dplyr::group_by(!!rlang::sym(grouping)) %>%
        dplyr::mutate(mean_mfaz_noflag = round(mean(.data$mfaz_noflag, na.rm = TRUE),3),
               sd_mfaz_noflag = round(stats::sd(.data$mfaz_noflag, na.rm = TRUE),2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(global_mfaz_noflag = ifelse(is.na(.data$global_mfaz), NA, ifelse(is.na(.data$mfaz_smart_flag), .data$global_mfaz, ifelse(.data$mfaz_smart_flag == 1, NA, .data$global_mfaz))),
               moderate_mfaz_noflag = ifelse(is.na(.data$moderate_mfaz), NA, ifelse(is.na(.data$mfaz_smart_flag), .data$moderate_mfaz, ifelse(.data$mfaz_smart_flag == 1, NA, .data$moderate_mfaz))),
               severe_mfaz_noflag = ifelse(is.na(.data$severe_mfaz), NA, ifelse(is.na(.data$mfaz_smart_flag), .data$severe_mfaz, ifelse(.data$mfaz_smart_flag == 1, NA, .data$severe_mfaz))))
    }

    #MUAC flags
    if(c("muac") %in% colnames(df)) {

    df <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::mutate(muac_flag = ifelse(is.na(.data$muac), NA, ifelse(.data$muac< 8 | .data$muac > 22, 1, 0)),
             muac_noflag = ifelse(is.na(.data$muac), NA, ifelse(.data$muac_flag == 1, NA, .data$muac)),
             gam_muac_noflag = ifelse(is.na(.data$muac), NA, ifelse(.data$muac_flag == 1, NA, .data$gam_muac)),
             mam_muac_noflag = ifelse(is.na(.data$muac), NA, ifelse(.data$muac_flag == 1, NA, .data$mam_muac)),
             sam_muac_noflag = ifelse(is.na(.data$muac), NA, ifelse(.data$muac_flag == 1, NA, .data$sam_muac)))
  }

    # Create combined GAM if WHZ and MUAC are present

    if(length(setdiff(c("wfhz", "muac"), colnames(df)))==0) {

    df <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::mutate(c_gam = dplyr::case_when(
        .data$gam_wfhz_noflag == 0 & .data$gam_muac_noflag == 0 ~ 0,
        .data$gam_wfhz_noflag == 1 ~ 1,
        .data$gam_muac_noflag == 1 ~ 1,
        .data$oedema == "y" ~ 1,
        is.na(.data$gam_wfhz_noflag) & .data$gam_muac_noflag == 0 ~ 0,
        is.na(.data$gam_muac_noflag) & .data$gam_wfhz_noflag == 0 ~ 0,
        is.na(.data$gam_muac_noflag) & is.na(.data$gam_wfhz_noflag) ~ NA_real_),
        c_sam = dplyr::case_when(
          .data$sam_wfhz_noflag == 0 & .data$sam_muac_noflag == 0 ~ 0,
          .data$sam_wfhz_noflag == 1 ~ 1,
          .data$sam_muac_noflag == 1 ~ 1,
          .data$oedema == "y" ~ 1,
          is.na(.data$sam_wfhz_noflag) & .data$sam_muac_noflag == 0 ~ 0,
          is.na(.data$sam_muac_noflag) & .data$sam_wfhz_noflag == 0 ~ 0,
          is.na(.data$sam_muac_noflag) & is.na(.data$sam_wfhz_noflag) ~ NA_real_))






  }

  # create and print summary of flags
  if(c("wfhz") %in% colnames(df)) {
    num_wfhz_smart <- sum(df$wfhz_smart_flag, na.rm = TRUE)
    num_wfhz_who <- sum(df$wfhz_who_flag, na.rm = TRUE)
    print(cat(paste0("For WFHZ, there were ", num_wfhz_smart, " SMART flags and ", num_wfhz_who, " WHO flags identified. They will not be included in the final results. ")))
  }

  if(c("hfaz") %in% colnames(df)) {
    num_hfaz_smart <- sum(df$hfaz_smart_flag, na.rm = TRUE)
    num_hfaz_who <- sum(df$hfaz_who_flag, na.rm = TRUE)

    print(cat(paste0("For HFAZ, there were ", num_hfaz_smart, " SMART flags and ", num_hfaz_who, " WHO flags identified. They will not be included in the final results. ")))

  }

  if(c("wfaz") %in% colnames(df)) {
    num_wfaz_smart <- sum(df$wfaz_smart_flag, na.rm = TRUE)
    num_wfaz_who <- sum(df$wfaz_who_flag, na.rm = TRUE)

    print(cat(paste0("For WFAZ, there were ", num_wfaz_smart, " SMART flags and ", num_wfaz_who, " WHO flags identified. They will not be included in the final results. ")))

  }

  if(c("mfaz") %in% colnames(df)) {
    num_mfaz_smart <- sum(df$mfaz_smart_flag, na.rm = TRUE)
    num_mfaz_who <- sum(df$mfaz_who_flag, na.rm = TRUE)

    print(cat(paste0("For MFAZ, there were ", num_mfaz_smart, " SMART flags and ", num_mfaz_who, " WHO flags identified. They will not be included in the final results. ")))

  }

  if(c("muac") %in% colnames(df)) {
    num_muac_flags <- sum(df$muac_flag, na.rm = TRUE)

    print(cat(paste0("For MUAC alone, there were ", num_muac_flags, " extreme measurements (<80mm or >220mm). They will not be included in the final results.")))

  }

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df, file_path)}

  return(df)


}
