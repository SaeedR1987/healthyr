#' Plot Cumulative Quality
#'
#' This function creates a plot of a numeric quality indicator over time so an analyst can
#' observe if an enumerator's work is improving over the course of data collection, primarily
#' with anthropometric data.
#'
#' @param df Inputs a dataframe which has an been processed and standandardized by
#' healthyr::format_nut_health_indicators function.
#' @param quality_indicator Inputs a character value specifying the quality indicator to plot.
#' Options include sd and dps.
#'
#' @return Returns a ggplot2 object.
#' @export
#'
#' @examples
#' \dontrun{plot_cumulative_quality(df, quality_indicator = sd)}
#' @importFrom rlang .data
plot_cumulative_quality <- function(df, quality_indicator) {

  if(quality_indicator != "sd" & quality_indicator != "dps") {stop("quality_indicator should be 'dps' or 'sd'. Please use only those values.")}

  if(quality_indicator == "sd") {

    enums <- unique(df$enum)

    for(i in 1:length(enums)) {
      print(paste0("This is enumerator: ", enums[i]))

      df2 <- df %>%
        dplyr::filter(.data$enum == enums[i]) %>%
        dplyr::filter(!is.na(.data$muac)) %>%
        dplyr::arrange(.data$date_dc) %>%
        dplyr::mutate(sd = runner::runner(x = .data$muac, f = stats::sd)) %>%
        dplyr::select(.data$date_dc, .data$enum, .data$sd) %>%
        dplyr::group_by(.data$date_dc) %>%
        dplyr::slice_tail() %>%
        dplyr::mutate(dps = as.double(.data$sd))

      if(i==1) {
        df3 <- df2
        print(paste0("Saved enumerator: ", unique(df3$enum)))
      } else {
        df3 <- rbind(df3, df2)
        print(paste0("Saved enumerator: ", unique(df3$enum)))
      }

    }

    df2 <- df3

    g <- ggplot2::ggplot(df2, ggplot2::aes(x = .data$date_dc, y = .data$sd, group = as.factor(.data$enum), color = as.factor(.data$enum))) + ggplot2::geom_line()
    g <- g + ggplot2::geom_line(ggplot2::aes(y = 15), color = "black", size = 1.2)

  } else if(quality_indicator == "dps") {

    enums <- unique(df$enum)

    for(i in 1:length(enums)) {
      print(paste0("This is enumerator: ", enums[i]))

      df2 <- df %>%
        #dplyr::select(date_dc, enum, muac) %>%
        dplyr::filter(.data$enum == enums[i]) %>%
        dplyr::filter(!is.na(.data$muac)) %>%
        dplyr::arrange(.data$date_dc) %>%
        dplyr::mutate(dps = runner::runner(x = .data$muac, f = healthyr::helper_muac_dps)) %>%
        dplyr::select(.data$date_dc, .data$enum, .data$dps) %>%
        dplyr::group_by(.data$date_dc) %>%
        dplyr::slice_tail() %>%
        dplyr::mutate(dps = as.double(.data$dps))

      if(i==1) {
        df3 <- df2
        print(paste0("Saved enumerator: ", unique(df3$enum)))
      } else {
        df3 <- rbind(df3, df2)
        print(paste0("Saved enumerator: ", unique(df3$enum)))
      }

    }

    df2 <- df3

    g <- ggplot2::ggplot(df2, ggplot2::aes(x = .data$date_dc, y = .data$dps, group = as.factor(.data$enum), color = as.factor(.data$enum))) + ggplot2::geom_line()
    g <- g + ggplot2::geom_line(ggplot2::aes(y = 20), color = "black", size = 1.2)

  }

  return(g)
}
