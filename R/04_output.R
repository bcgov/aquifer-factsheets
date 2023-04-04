#' # Copyright 2020 Province of British Columbia
#' #
#' # Licensed under the Apache License, Version 2.0 (the "License"); you may not
#' # use this file except in compliance with the License. You may obtain a copy of
#' # the License at
#' #
#' # http://www.apache.org/licenses/LICENSE-2.0
#' #
#' # Unless required by applicable law or agreed to in writing, software
#' # distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#' # WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#' # License for the specific language governing permissions and limitations under
#' # the License.
#'
#' #
#' # Create Plots and Figures
#' #
#'
#' # Setup -------------------------------------------------------------------
#'

#'
#'
#' # Load functions, packages and data
#' source("00_setup.R")
#' load("tmp/aquifer_factsheet_clean_data.RData")
#'

#'
#' # Retired aquifers ----------------
#'
#' # Remove retired aquifers with message
#' if(any(aquifers %in% aquifer_db$aquifer_id[aquifer_db$retired])) {
#'   a <- aquifers[aquifers %in% aquifer_db$aquifer_id[aquifer_db$retired]]
#'   message("Retired aquifers removed from run: ", paste0(a, collapse = ", "))
#'   aquifers <- aquifers[!aquifers %in% a]
#' }
#'
#'
#'
#' # Boxplots: Yield Boxplots ----------------------------------------------------------
clean_files <- function(aquifers) {
  # Remove old files (make sure no old files to interfere)
  if(delete_old) file.remove(list.files("./out/boxplots/", full.name = TRUE))
}


plot_no_data <- function(type = "none", x = 0, y = 5){

  if(type == "none") {
    t <- "No Data"
  } else if(type == "insufficient") {
    t <- "Insufficient data\navailable (n < 5)"
  }

  annotate("text", x = x, y = y, label = t, size = 3)
}


# Boxplots -------------------------------------------
plot_bx_base <- function(n, type, yield = 100) {

  max_lim <- case_when(n < 5 ~ 10,
                       all(yield == 0) ~ 1,
                       TRUE ~ as.numeric(NA))

  w <- data.frame(well_yield = 1,
                  well_depth = 1,
                  water_depth = 1)

  labs <- c(
    "well_yield" = "High             Reported Well Yields (L/s)             Low",
    "well_depth" = "Reported Well Depths Below Ground (m)",
    "water_depth" = "Reported Static Water Depths Below Ground (m)")


  g <- ggplot(data = w, aes(x = NA, y = .data[[type]])) +
    aq_theme() +
    bx_theme() +
    theme(axis.title.x = element_text(vjust = 1)) +
    scale_y_reverse(limits = c(max_lim, 0), expand = c(0.05, 0)) +
    annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
             label = paste0("n = ", n), size = ann_size) +
    labs(x = "\n",
         y = labs[[type]])
  if(n < 5) g <- g + scale_x_continuous(breaks = c(-1:1))

  g
}


# Create data frame with yields for particular aquifer
plot_bx_well_yield <- function(w) {

  n <- unique(w$n_well_yield)
  w <- filter(w, well_yield != 0)
  a <- w$aquifer_id[1]

  if(n > 0) {

    # Add current data
    well_yield <- plot_bx_base(n, "well_yield", w$well_yield) %+% w

    if(n < 5) {
      # Insufficient data
      well_yield <- well_yield + plot_no_data("insufficient")

    } else {
      # Sufficient data
      prod <- median(w$well_yield, na.rm = TRUE)

      # Boxplot for Yield
      well_yield <- well_yield +
        geom_boxplot(color = "navy", fill = "lightsteelblue3",
                     width = 0.5, na.rm = TRUE) +
        annotation_custom(
          y_gradient(), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        labs(x = paste0("Median well yield:\n", round(prod, 2), " L/s"))

      # Move gradient to back
      well_yield$layers <- append(well_yield$layers[[length(well_yield$layers)]],
                                  well_yield$layers[-length(well_yield$layers)])
    }

    ggsave(paste0("well_yield_", sprintf("%04d", a), ".jpg"),
           plot = well_yield, path = "out/boxplots/",
           width = bx_width, height = bx_height, dpi = dpi)
  }
}

plot_bx_well_depth <- function(w) {

  n <- unique(w$n_well_depth)
  a <- w$aquifer_id[1]

  if(n > 0) {

    # Add current data
    well_depth <- plot_bx_base(n, "well_depth") %+% w

    if(n < 5) {
      # Insufficient data
      well_depth <- well_depth + plot_no_data("insufficient")

    } else {
      # Sufficient data
      prod <- round(median(w$well_depth, na.rm = TRUE), 2)

      well_depth <- well_depth +
        geom_boxplot(color="darkgreen", fill="darkolivegreen1", width = 0.5,
                     na.rm = TRUE) +
        labs(x = paste0("Median well depth:\n", prod, " m"))
    }

    ggsave(paste0("well_depth_", sprintf("%04d", a), ".jpg"),
           plot = well_depth, path = "out/boxplots/",
           width = bx_width, height = bx_height, dpi = dpi)
  }
}


plot_bx_water_depth <- function(w) {

  n <- unique(w$n_water_depth)
  a <- w$aquifer_id[1]

  # Only plot if data
  if(n > 0) {

    # Add current data
    water_depth <- plot_bx_base(n, "water_depth") %+% w

    if(n < 5) {
      # Insufficient data
      water_depth <- water_depth + plot_no_data("insufficient")

    } else {
      # Sufficient data
      prod <- round(median(w$water_depth, na.rm = TRUE), 2)

      water_depth <- water_depth +
        geom_boxplot(color = "brown", fill = "navajowhite", width = 0.5,
                     na.rm = TRUE) +
        labs(x = paste0("Median water depth:\n", prod, " m"))
    }

    ggsave(paste0("water_depth_", sprintf("%04d", a), ".jpg"),
           plot = water_depth, path = "out/boxplots/",
           width = bx_width, height = bx_height, dpi = dpi)

  }
}


plot_bx_empty <- function() {
  ggsave(
    "well_yield_NA.jpg", plot = plot_bx_base(0, "well_yield") + plot_no_data(type = "none"),
    path = "out/boxplots/",
    width = bx_width, height = bx_height, dpi = dpi)

  ggsave(
    "well_depth_NA.jpg", plot = plot_bx_base(0, "well_depth") + plot_no_data(type = "none"),
    path = "out/boxplots/",
    width = bx_width, height = bx_height, dpi = dpi)

  ggsave(
    "water_depth_NA.jpg", plot = plot_bx_base(0, "water_depth") + plot_no_data(type = "none"),
    path = "out/boxplots/",
    width = bx_width, height = bx_height, dpi = dpi)
}





# Combo Water level / Precip ----------------------------------------------

plot_wl_ppt <- function(wl, ppt) {

  a <- wl$aquifer_id[1]
  o <- wl$ow[1]

  wl <- mutate(
    wl, across(
      .cols = c("median_median", "percentile_25", "percentile_75", "percentile_10",
                "percentile_90", "min_monthly_wl", "max_monthly_wl"),
      ~.x * -1))

  ppt <- mutate(
    ppt,
    precipitation = case_when(
      precipitation == "snow" ~ "Total snowfall\n(rainfall equivalent)",
      precipitation == "rain" ~ "Total rainfall (mm)"))

  climate_title <- tools::toTitleCase(as.character(ppt$climate_name[1]))


  # Only continue if we have sufficient data for precipitation
  if(any(!is.na(ppt$ppt_mm))) {

    # Calculate the scaling to get the water level data on the same plot,
    # but scaled differently
    # - proportion (p) is approximately 70% upper, 30% lower
    # - bandwidth is how spread apprt the data WILL be
    # - position is where we want it to be
    # - max_range is the spread of the data to start with
    # - mult is the multiplier to get the data spread even farther to match
    #   the bandwidth (needs to be negative to flip the figure)
    # - shift is the value to move the data by to get it at the right place

    ppt_overall <- group_by(ppt, month_abb) %>%
      summarize(total = sum(ppt_mm, na.rm = TRUE), .groups = "drop")

    p <- 0.7

    wl_shift <- wl %>%
      summarize(
        bandwidth = max(ppt_overall$total, na.rm = TRUE) * (p / (1 - p)),
        position = max(ppt_overall$total, na.rm = TRUE) * 1.15,
        max_range = max(min(max_monthly_wl, na.rm = TRUE) - max(min_monthly_wl, na.rm = TRUE)),
        mult = -(bandwidth/max_range),
        shift = -min(max_monthly_wl * mult) + position,
        .groups = "drop")

    # Breaks and labels for ppt axis
    breaks_ppt <- get_breaks(0, max(ppt_overall$total, na.rm = TRUE), length.out = 5)

    # Keep copy of original data for breaks later
    wl_orig <- wl

    wl <- wl %>%
      cbind(wl_shift) %>%
      mutate(median_median = median_median * mult + shift,
             percentile_10 = percentile_10 * mult + shift,
             percentile_90 = percentile_90 * mult + shift,
             percentile_25 = percentile_25 * mult + shift,
             percentile_75 = percentile_75 * mult + shift,
             min_monthly_wl = min_monthly_wl * mult + shift,
             max_monthly_wl = max_monthly_wl * mult + shift)

    # Get number of years so we can warn if less than 10
    num_yrs <- wl$num_yrs[1]

    # Complex title
    # - includes unicode (\u00B9 and \u00B2) to create superscripts 1 and 2
    # - \n creates new line between the two titles
    # - Includes number of years, date range, and the climate station
    # - Exact text also changes depending on how many years
    wl_title <- case_when(
      num_yrs < 5 ~ "No Monthly Water Level Summary (only ",
      num_yrs < 10 ~ "Preliminary Monthly Water Level Summary (",
      TRUE ~ "Full Monthly Water Level Summary (") %>%
      paste0(num_yrs, " years of data; ", wl$min_yr[1], "-", wl$max_yr[1], ")") %>%
      paste0("\u00B9 ", ., "\n",
             "\u00B2 Climate Normals Based on ",
             climate_title,
             " Environment Canada Weather Station (1981-2010)")

    wl <- filter(wl, num_yrs >= 5)

    # Add precipitation
    g <- ggplot() +
      aq_theme() +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 10)) +
      geom_bar(data = ppt,
               aes(x = month_abb, y = ppt_mm, fill = precipitation),
               stat = "identity", position = position_stack(reverse = TRUE),
               color = "black")

    # Add Water level if sufficient data
    if(nrow(wl) > 0) {

      # Breaks for water-level axis
      b <- get_breaks(max(wl_orig$min_monthly_wl, na.rm = TRUE),
                      min(wl_orig$max_monthly_wl, na.rm = TRUE),
                      length.out = 10)

      breaks_wl <- data.frame(breaks = b) %>%
        mutate(gridlines = breaks * wl_shift$mult[1] + wl_shift$shift[1]) %>%
        filter(gridlines > (max(breaks_ppt) * 1.2))

      dec_points <- str_length(str_extract(breaks_wl$breaks, "[^.]*$"))

      g <- g +
        # Add secondary axis
        scale_y_continuous(
          breaks = breaks_ppt, expand = c(0.02, 0),
          sec.axis = sec_axis(~ ((. - wl_shift$shift[1]) / (wl_shift$mult[1])),
                              name = "Depth to Groundwater (m below ground surface)",
                              breaks = breaks_wl$breaks,
                              labels = format(-breaks_wl$breaks,
                                              nsmall = dec_points))) +
        # Artificially add in grid lines
        geom_hline(aes(yintercept = breaks_wl$gridlines),
                   colour = aq_theme()$panel.grid.major$colour) +
        # Add data
        geom_ribbon(data = wl, aes(x = as.numeric(month_abb),
                                             ymin = percentile_10,
                                             ymax = percentile_90,
                                             fill = "10-90th Percentile"),
                    alpha = 0.8) +
        geom_ribbon(data = wl, aes(x = as.numeric(month_abb),
                                             ymin = percentile_25,
                                             ymax = percentile_75,
                                             fill = "25-75th Percentile"),
                    alpha = 0.8) +
        geom_line(data = wl, aes(
          x = as.numeric(month_abb),
          y = median_median, colour = "Median")) +
        geom_point(data = wl, aes(
          x = as.numeric(month_abb),
          y = min_monthly_wl, colour = "Extreme Maximum")) +
        geom_point(data = wl, aes(
          x = as.numeric(month_abb),
          y = max_monthly_wl, colour = "Extreme Minimum"))
    }

    g <- g +
      # Scales and Labels
      # These add specific colours to the lables assigned to the aes above
      scale_colour_manual(values = c("Extreme Maximum" = "slategray3",
                                     "Median" = "black",
                                     "Extreme Minimum" = "bisque3")) +
      scale_fill_manual(values = c("10-90th Percentile" = "lightskyblue2",
                                   "25-75th Percentile" = "steelblue1",
                                   "Total rainfall (mm)" = "lightcyan3",
                                   "Total snowfall\n(rainfall equivalent)" = "white")) +
      # Remove point from median line
      guides(colour = guide_legend(order = 1,
                                   override.aes = list(shape = c(19, NA, 19))),
             fill = guide_legend(order = 2)) +
      labs(x = "Month",
           y = paste0("Monthly Precipitation (mm) at\n", climate_title),
           title = wl_title)

    ggsave(filename = paste0("./out/gwl_ppt/combo_",
                             sprintf("%04d", a),"_OW",
                             sprintf("%04d", o),".png"),
           plot = g,
           height = combo_height, width = combo_width, dpi = dpi)

  } else {
    # Write an informative message to the console if there is no data for the ppt
    # message("AQUIFER ID: ", a, " OBS WELL: ", o, ", Water level data, ",
    #         "but no precipitation data\n(perhaps obs_wells_index is missing ",
    #         "CLIMATE ID for this aquifer)")
  }
}


# Groundwater level trend plot --------------------------------------------

plot_gwl <- function(gwl, gwl_trends) {

  a <- gwl$aquifer_id[1]
  o <- gwl$ow[1]

  gwl <- rename(gwl, "Well_Num" = "ow")
  gwl_trends <- rename(gwl_trends, "Well_Num" = "ow")

  # Skip plot if < 5 years of data
  if(gwl_trends$nYears >= 5) {

    g <- gwl_area_plot(data = gwl,
                       trend = gwl_trends$trend_line_slope,
                       intercept = gwl_trends$trend_line_int,
                       trend_category = gwl_trends$state,
                       sig = gwl_trends$sig,
                       showInterpolated = TRUE, save = FALSE,
                       mkperiod = "annual", show_stable_line = FALSE) +
      labs(title = NULL) +
      theme(legend.position = "right", legend.box = "vertical",
            legend.margin = margin(
              0, # Add extra spacing if no interpolated values in legend
              if_else(any(gwl$nReadings == 0), 5.5, 45),
              0, 5.5),
            legend.spacing = unit(0, units = "mm"))

    ggsave(plot = g,
           filename = paste0("./out/gwl_trends/trends_",
                             sprintf("%04d", as.numeric(a)),"_OW",
                             sprintf("%04d", as.numeric(o)),".png"),
           height = trend_height, width = trend_width, dpi = dpi)
  }
}

    }
  }
}
#'
#'
#' # Piper plots -----------------------------------------------------------------
#'
#' #' - Create all plots, but note in piper plot text if any shouldn't be used.
#'
#' # Remove old files (make sure no old files to interfere)
#' if(delete_old) file.remove(list.files("./out/piperplots/", full.name = TRUE))
#'
#' p <- progress::progress_bar$new(format = "  Piperplots [:bar] :percent eta: :eta",
#'                                 total = length(aquifers))
#' for (a in aquifers) {
#'   p$tick()
#'
#'   d <- filter(ems, aquifer_id == a)
#'
#'   for(o in unique(d$StationID)) {
#'
#'     d2 <- filter(d, StationID == o) %>%
#'       mutate(n = n_distinct(ems_id)) %>%
#'       assert(in_set(1), n) # Check that we don't have multiple ems ids per well
#'
#'     # Note: By default piper_plot() uses only valid (abs(charge_balance) <=10) data
#'     if(nrow(d2) >= 1) {
#'       # Make plot
#'       f <- paste0("./out/piperplots/piperplot_",
#'                   sprintf("%04d", as.numeric(a)), "_OW",
#'                   sprintf("%04d", as.numeric(o)), ".png")
#'
#'       # Only if it would plot...
#'       if(!is.null(piper_plot(d2, legend = FALSE, plot_data = TRUE))) {
#'         pp <- image_graph(width = 2000,
#'                           height = 2100, res = dpi)
#'         piper_plot(d2, legend = FALSE)
#'         dev.off()
#'
#'         #print(p)
#'
#'         # Trim plot and add in small border
#'         pp2 <- image_trim(pp) %>%
#'           image_border(color = "white")
#'         #print(p2)
#'
#'         # Save plot
#'         image_write(pp2, path = f)
#'       }
#'     }
#'   }
#' }
