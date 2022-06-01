


plot_date_runner <- function(df,
                             quality_indicator,
                             grouping = NULL) {

  # CHECKING IF DATA DC IS IN DATA FRAME

  if(!(c("date_dc_date") %in% colnames(df))) {stop("You must have the standardized variable 'date_dc_date' in the data frame to run this function.")}

  # CHECKING QUALITY INDICATOR INPUT

  qual_indicators <- c(
    # anthro
    "mean_muac", "mean_wfhz", "sd_muac", "sd_wfhz", "dps_muac", "dps_weight", "dps_height",
    # fsl
    "mean_fcs_score", "sd_fcs_score", "mean_rcsi_score", "sd_rcsi_score", "mean_hhs_score", "mean_hdds_score", "corr.fcsi_rcsi", "corr.fcs_hhs", "corr.fcs_hdds", "corr.hhs_rcsi",
    # mortality
    "num_deaths", "num_under5_deaths", "age_ratio_5_10", "sex_ratio", "cdr", "u5dr", "mean_hh_size",
    # iycf
    "age_ratio_2_5", "sd_mdd_score", "sex_ratio")

  if(length(setdiff(quality_indicator, qual_indicators))==0) {
    print(qual_indicators)
    stop("quality_indicator should be one of the preselected options. Please use only one of the above values:")
  }

  # CHECKING GROUPING INPUT

  if(!(grouping %in% colnames(df))) {stop(paste0("The variable '", grouping, "' does not exist in the dataframe. Please check your input"))}

  a <- 0
  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
    a <- 1
  } else {
    df <- df %>% rename(group = {{grouping}})
  }

  if(quality_indicator == "sd_muac") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("muac") %in% colnames(df))) {
        stop("You must have the standardized column named 'muac' in your dataframe with MUAC measurements in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(groups == groups[i]) %>%
        filter(!is.na(muac)) %>%
        arrange(date_dc_date) %>%
        mutate(sd_muac = runner(x = muac, f = sd)) %>%
        dplyr::select(date_dc_date, group, sd) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(sd_muac = as.double(sd_muac),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = sd_muac, group = groups, color = groups)) + geom_line()
    g <- g + geom_line(aes(y = 15), color = "black", size = 1.2)


  }

  if(quality_indicator == "mean_muac") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("muac") %in% colnames(df))) {
        stop("You must have the standardized column named 'muac' in your dataframe with MUAC measurements in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(groups == groups[i]) %>%
        filter(!is.na(muac)) %>%
        arrange(date_dc_date) %>%
        mutate(mean_muac = runner(x = muac, f = mean)) %>%
        dplyr::select(date_dc_date, group, mean_muac) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(mean_muac = as.double(mean_muac),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = mean_muac, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "sd_wfhz") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("wfhz") %in% colnames(df))) {
        stop("You must have the standardized column named 'wfhz' in your dataframe in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(groups == groups[i]) %>%
        filter(!is.na(wfhz)) %>%
        arrange(date_dc_date) %>%
        mutate(sd_wfhz = runner(x = wfhz, f = sd)) %>%
        dplyr::select(date_dc_date, group, sd_wfhz) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(sd_wfhz = as.double(sd_wfhz),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = sd_wfhz, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "mean_wfhz") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("wfhz") %in% colnames(df))) {
        stop("You must have the standardized column named 'wfhz' in your dataframe in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(groups == groups[i]) %>%
        filter(!is.na(wfhz)) %>%
        arrange(date_dc_date) %>%
        mutate(mean_wfhz = runner(x = wfhz, f = mean)) %>%
        dplyr::select(date_dc_date, group, mean_wfhz) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(mean_wfhz = as.double(mean_wfhz),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = mean_wfhz, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "dps_weight") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("weight") %in% colnames(df))) {
        stop("You must have the standardized column named 'weight' in your dataframe in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(groups == groups[i]) %>%
        filter(!is.na(weight)) %>%
        arrange(date_dc_date) %>%
        mutate(dps_weight = runner(x = weight, f = helper_runner_dps)) %>%
        dplyr::select(date_dc_date, group, dps_weight) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(dps_weight = as.double(dps_weight),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = dps_weight, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "dps_height") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("height") %in% colnames(df))) {
        stop("You must have the standardized column named 'height' in your dataframe in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(groups == groups[i]) %>%
        filter(!is.na(height)) %>%
        arrange(date_dc_date) %>%
        mutate(dps_height = runner(x = height, f = helper_runner_dps)) %>%
        dplyr::select(date_dc_date, group, dps_height) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(dps_height = as.double(dps_height),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = dps_height, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "dps_muac") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("muac") %in% colnames(df))) {
        stop("You must have the standardized column named 'muac' in your dataframe with MUAC measurements in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(group == groups[i]) %>%
        filter(!is.na(muac)) %>%
        arrange(date_dc_date) %>%
        mutate(dps_muac = runner(x = muac, f = helper_runner_dps)) %>%
        dplyr::select(date_dc_date, group, dps_muac) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(dps_muac = as.double(dps_muac),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = dps_muac, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "mean_fcs_score") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("fcs_score") %in% colnames(df))) {
        stop("You must have the standardized column named 'fcs_score' in your dataframe in order to run this plot.")
      }


      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        filter(group == groups[i]) %>%
        filter(!is.na(fcs_score)) %>%
        arrange(date_dc_date) %>%
        mutate(mean_fcs_score = runner(x = fcs_score, f = mean)) %>%
        dplyr::select(date_dc_date, group, mean_fcs_score) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(mean_fcs_score = as.double(mean_fcs_score),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = mean_fcs_score, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "mean_rcsi_score") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("rcsi_score") %in% colnames(df))) {
        stop("You must have the standardized column named 'rcsi_score' in your dataframe in order to run this plot.")
      }

      df2 <- df %>%
        filter(group == groups[i]) %>%
        filter(!is.na(rcsi_score)) %>%
        arrange(date_dc_date) %>%
        mutate(mean_rcsi_score = runner(x = rcsi_score, f = mean)) %>%
        dplyr::select(date_dc_date, group, mean_rcsi_score) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(mean_rcsi_score = as.double(mean_rcsi_score),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = mean_rcsi_score, group = groups, color = groups)) + geom_line()
  }

  if(quality_indicator == "mean_hhs_score") {

    groups <- unique(df$group)

    for(i in 1:length(groups)) {

      if(!(c("hhs_score") %in% colnames(df))) {
        stop("You must have the standardized column named 'hhs_score' in your dataframe in order to run this plot.")
      }

      df2 <- df %>%
        filter(group == groups[i]) %>%
        filter(!is.na(hhs_score)) %>%
        arrange(date_dc_date) %>%
        mutate(mean_hhs_score = runner(x = hhs_score, f = mean)) %>%
        dplyr::select(date_dc_date, group, mean_hhs_score) %>%
        group_by(date_dc_date) %>%
        slice_tail() %>%
        mutate(mean_rcsi_score = as.double(mean_hhs_score),
               group = as.factor(group))

      if(i==1) {
        df3 <- df2
      } else {
        df3 <- rbind(df3, df2)
      }

    }

    # df2 <- df3

    g <- ggplot(df3, aes(x = date_dc_date, y = mean_hhs_score, group = groups, color = groups)) + geom_line()
  }



  return(g)
}
