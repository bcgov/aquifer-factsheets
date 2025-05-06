#' Customized version of gwl_area_plot() from bcaquiferdata
gwl_area_plot_customized <- function(df, trend, intercept, trend_category,
                                 sig, showInterpolated = FALSE,
                                 show_stable_line = FALSE, save = FALSE,
                                 path = "./", mkperiod = "annual", opts = NULL) {

  if (showInterpolated) {
    # if there are no interpolated values, reset showInterpolated to FALSE
    if (nrow(df[df$nReadings == 0,]) == 0) {
      showInterpolated <- FALSE
    }
  } else {
    df <- df[df$nReadings > 0,]
  }

  df$Date <- lubridate::as_date(df$Date)

  minDate <- min(df$Date)
  maxDate <- max(df$Date)
  nYears <- as.numeric(difftime(maxDate, minDate, units = "days"))/365

  WellNum <- df$Well_Num[1]

  if (mkperiod == "monthly") {
    ## Slope is in m/month, have to convert to m/day to work with Date format
    slope <- -(trend/12/365)
  } else if (mkperiod == "annual") {
    slope <- -(trend/365)
  } else {
    stop("mkperiod must be either 'monthly' or 'annual'")
  }

  if (is.na(slope) || stringr::str_detect(tolower(trend_category), "stable")) {
    trendprint <- " "

  } else {
    trendpre <- ifelse(slope > 0, "(+", "(")
    trendprint <- paste0(trendpre,
                         paste0(format(slope * 365, digits = 2, nsmall = 2,
                                       scientific = FALSE), " m/year)"))

  }

  int.well <- intercept + slope * as.numeric(minDate)

  maxgwl <- max(df$med_GWL, na.rm = TRUE)
  mingwl <- min(df$med_GWL, na.rm = TRUE)
  gwlrange <- maxgwl - mingwl
  midgwl <- (maxgwl + mingwl)/2
  lims <- c(midgwl + gwlrange, midgwl - gwlrange)

  df$max_lims <- max(lims[1], max(df$med_GWL, na.rm = TRUE) + 5)

  plot.area <- ggplot(df, aes(x = Date)) +
    geom_ribbon(aes(ymin = .data[["med_GWL"]],
                    ymax = .data[["max_lims"]],
                    fill = "Groundwater Level"), alpha = 0.3) +
    labs(title = NULL, x = "Date",
         y = "Depth Below Ground (metres)",
         subtitle = paste0("Category: ", tools::toTitleCase(tolower(trend_category)),
                           " ",
                           trendprint)) +
    theme_minimal() +
    theme(
      text = element_text(colour = "black"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(colour="grey50"),
      legend.position = "right", legend.box =  "vertical",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 11),
      legend.margin = margin(
        0, # Add extra spacing if no interpolated values in legend
        if_else(any(df$nReadings == 0), 5.5, 45),
        0, 5.5),
      legend.spacing = unit(0, units = "mm")) +
    scale_y_reverse(expand = c(0,0)) + coord_cartesian(ylim = lims) +
    scale_x_date(labels = scales::label_date("%Y"),
                 breaks = scales::breaks_width(dplyr::if_else(nYears < 10,
                                                     "1 year",
                                                     "3 years")),
                 expand = c(0,0)) +
    scale_fill_manual(name = '', values = c('Groundwater Level' = "#1E90FF"))

  vals <- c("Long-term Trend" = 'orange', "Interpolated (Missing) Values" = 'grey60')
  #override_list <- list(colour = c("orange", "grey60"), shape = c(NA, 16), linetype = c(1, 0))

  if (showInterpolated) {
    plot.area <- plot.area +
      geom_point(data = df[df$nReadings == 0,],
                 aes(y = med_GWL, colour = "Interpolated (Missing) Values"),
                 size = 0.5)
  }

  if ((show_stable_line || !stringr::str_detect(tolower(trend_category), "stable")) && !is.na(slope)) {
    plot.area <- plot.area +
      geom_abline(aes(intercept = intercept, slope = slope, colour = "Long-term Trend"),
                  data = data.frame(intercept = -int.well, slope = slope), linewidth = 1)
  }

  plot.area <- plot.area +
    scale_colour_manual(name = '', values = vals) +

    opts

  if (save) {
    ggsave(filename = paste0(path, "trend_chart_well_", WellNum, ".pdf"),
           plot = plot.area)
  }

  return(plot.area)

}
