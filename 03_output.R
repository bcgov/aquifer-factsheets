# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

##############################
# Create Plots and Figures
##############################

# Setup -------------------------------------------------------------------

# Monthly groundwater levels and precipitation plots are combined in combo plots
# Also create individual plots of each for use in other projects?
# (i.e. Arc Aquifer Dashboard?)
indiv_gwl_and_precip <- FALSE

# Check for aquifer numbers
if(!exists("aquifers")) {
  stop("Can't run 03_outpu.R without first specifying which aquifers to visualize", call. = FALSE)
} else if(length(aquifers) == 0) {
  stop("'aquifers' is empty, specify at least one aquifer to visualize", call. = FALSE)
}


# Load functions, packages and data
source("00_functions.R")
source("00_header.R")
load("tmp/Aquifer_Dash_clean.RData")

# # Specify which `aquifers` you want figures for in the 00_run.R script
# wells_db <- filter(wells_db, AQUIFER_ID %in% aquifers)
# wl_month_data <- filter(wl_month_data, AQUIFER_ID %in% aquifers)


# Figure defaults ---------------------------------------------------------
bx_height <- 5.4
bx_width <- 1.8

combo_height <- 3.85
combo_width <- 10

trend_height <- 3.85
trend_width <- 10

dpi <- 300

ann_size <- 2.75  # Annotation sizes for samples sizes in boxplots

aq_theme <- theme_bw() +
  theme(axis.title.y = element_text(margin = unit(c(0, 2, 0, 0), "mm")),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"))

bx_theme <- theme(axis.text.x = element_blank(),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm"), size = 10),
                  panel.grid.major = element_line(colour = "grey75"),
                  panel.grid.minor = element_line(colour = "grey75"))

# Boxplots: Yield Boxplots ----------------------------------------------------------

# Remove old files (make sure no old files to interfere)
if(delete_old) file.remove(list.files("./out/boxplots/", pattern = "yield", full.name = TRUE))

# Note: "No data" boxplots must be created after at least one other boxplot WITH
# data (otherwise you'll get an error)

# Create gradient background (same for each Yield Boxplot)
g <- grid::rasterGrob(c("#FFFFFF00", "#A2B5CD90"),
                      width = unit(1, "npc"),
                      height = unit(1, "npc"), interpolate = TRUE)

message("\nCreating Yield Boxplots")
p <- progress_estimated(length(aquifers))
for (a in aquifers) {
  p$tick()$print()

  # Create data frame with yields for particular aquifer
  AOI_wells <- filter(wells_db, AQUIFER_ID == a)

  n <- length(na.omit(AOI_wells$YIELD_VALUE_L_S))

  if(n > 0) {
    Yield_base <- ggplot(data = AOI_wells, aes(x = NA, y = YIELD_VALUE_L_S))+
      aq_theme +
      bx_theme +
      theme(axis.title.x = element_text(vjust = 1)) +
      ylab("High             Reported Well Yields (L/s)             Low")

    if(n < 5) {
      # Insufficient data
      Yield <- Yield_base +
        annotate("text", x = 0, y = 1.5, label = "Insufficient data\navailable (n < 5)", size = 3) +
        annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
                 label = paste0("n = ", n), size = ann_size) +
        xlab("\n") + # To match regular plots
        scale_y_reverse(limits = c(3, 0)) +
        scale_x_continuous(breaks = c(-1, 0, 1))
    } else {
      # Sufficient data
      prod <- median(AOI_wells$YIELD_VALUE_L_S, na.rm = TRUE)
      xlab <- paste0("Median well yield:\n", round(prod, 2), " L/s")

      prod_labs <- max(AOI_wells$YIELD_VALUE_L_S, na.rm = TRUE)
      prod_labs <- c("Low Productivity",
                     case_when(prod_labs > 3 ~ "High Productivity",
                               prod_labs >= 0.3 ~ "Medium Productivity",
                               prod_labs < 0.3 ~ ""))

      # Boxplot for Yield
      Yield <- Yield_base +
        annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        geom_boxplot(color = "navy", fill = "lightsteelblue3",
                     width = 0.5, na.rm = TRUE) +
        annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
                 label = paste0("n = ", n), size = ann_size) +
        xlab(xlab) +
        scale_y_reverse(limits = c(NA, 0), expand = c(0.05, 0))
    }

    ggsave(paste0("yield_", sprintf("%04d", a), ".jpg"),
           plot = Yield, path = "out/boxplots/",
           width = bx_width, height = bx_height, dpi = dpi)
  }
}

# Create empty boxplot
Yield_NA <- Yield_base +
  annotate("text", x = 0, y = 1.5, label = "No Data", size = 3) +
  annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
           label = "n = 0", size = ann_size) +
  xlab("\n") + # To match regular plots
  scale_y_reverse(limits = c(3, 0)) +
  scale_x_continuous(breaks = c(-1, 0, 1))
ggsave("yield_NA.jpg", plot = Yield_NA, path = "out/boxplots/",
       width = bx_width, height = bx_height, dpi = dpi)


# Boxplots: Well Depth ----------------------------------------------------------

# Remove old files (make sure no old files to interfere)
if(delete_old) file.remove(list.files("./out/boxplots/", pattern = "well_depth", full.name = TRUE))

# Note: "No data" boxplots must be created after at least one other boxplot WITH
# data (otherwise you'll get an error)

message("\nCreating Well Depth Boxplots")
p <- progress_estimated(length(aquifers))
for (a in aquifers) {
  p$tick()$print()

  # Create data frame with yields for particular aquifer
  AOI_wells <- filter(wells_db, AQUIFER_ID == a)

  # Sample size
  n <- length(na.omit(AOI_wells$DEPTH_WELL_DRILLED_M))

  if(n > 0) {

    #Boxplot for Depth Drilled
    depthdrilled_base <- ggplot(data = AOI_wells, aes(x = NA, y = DEPTH_WELL_DRILLED_M)) +
      aq_theme +
      bx_theme +
      ylab("Reported Well Depths Below Ground (m)")

    if(n < 5) {
      # Insufficient data
      depthdrilled <- depthdrilled_base +
        annotate("text", x = 1, y = 5, label = "Insufficient data\navailable (n < 5)", size = 3) +
        annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
                 label = paste0("n = ", n), size = ann_size) +
        xlab("\n") + # To match regular plots+
        scale_y_reverse(limits = c(10, 0)) +
        scale_x_continuous(breaks = c(-1, 0, 1))

    } else {
      #Sufficient data
      prod <- round(median(AOI_wells$DEPTH_WELL_DRILLED_M, na.rm = TRUE), 2)
      xlab <- paste0("Median well depth:\n", prod, " m")

      depthdrilled <- depthdrilled_base +
        geom_boxplot(color="darkgreen", fill="darkolivegreen1", width = 0.5, na.rm = TRUE) +
        annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
                 label = paste0("n = ", n), size = ann_size) +
        scale_y_reverse(limits = c(NA, 0)) +
        xlab(xlab)
    }

    ggsave(paste0("well_depth_", sprintf("%04d", a), ".jpg"),
           plot = depthdrilled, path = "out/boxplots/",
           width = bx_width, height = bx_height, dpi = dpi)
  }
}

# Create empty boxplot
depthdrilled_NA <- depthdrilled_base +
  annotate("text", x = 1, y = 5, label = "No Data", size = 3) +
  annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
           label = "n = 0", size = ann_size) +
  xlab("\n") + # To match regular plots +
  scale_y_reverse(limits = c(10, 0)) +
  scale_x_continuous(breaks = c(-1, 0, 1))
ggsave("well_depth_NA.jpg", plot = depthdrilled_NA, path = "out/boxplots/",
       width = bx_width, height = bx_height, dpi = dpi)



# Boxplots: Water Depth ----------------------------------------------------

# Remove old files (make sure no old files to interfere)
if(delete_old) file.remove(list.files("./out/boxplots/", pattern = "water_depth", full.name = TRUE))

# Note: "No data" boxplots must be created after at least one other boxplot WITH
# data (otherwise you'll get an error)

message("\nCreating Water Depth Boxplots")
p <- progress_estimated(length(aquifers))
for (a in aquifers) {
  p$tick()$print()

  # Create data frame with yields for particular aquifer
  AOI_wells <- filter(wells_db, AQUIFER_ID == a)

  # Sample size
  n <- length(na.omit(AOI_wells$WATER_DEPTH_M))

  # Only plot if data
  if(n > 0) {
    # Boxplot for waterdepth
    waterdepth_base <- ggplot(data = AOI_wells, aes(x = NA, y = WATER_DEPTH_M)) +
      aq_theme +
      bx_theme +
      ylab("Reported Static Water Depths Below Ground (m)")

    if(n < 5) {
      # Not enough data
      waterdepth <- waterdepth_base +
        annotate("text", x = 1, y = 5, label = "Insufficient data\navailable (n < 5)", size = 3) +
        annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
                 label = paste0("n = ", n), size = ann_size) +
        xlab("\n") + # To match regular plots+
        scale_y_reverse(limits = c(10, 0)) +
        scale_x_continuous(breaks = c(-1, 0, 1))

    } else {
      # Enough data
      prod <- round(median(AOI_wells$WATER_DEPTH_M, na.rm = TRUE), 2)
      xlab <- paste0("Median water depth:\n", prod, " m")

      waterdepth <- waterdepth_base +
        geom_boxplot(color = "brown", fill = "navajowhite", width = 0.5, na.rm = TRUE) +
        annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
                 label = paste0("n = ", n), size = ann_size) +
        scale_y_reverse(limits = c(NA, 0)) +
        xlab(xlab)
    }

    ggsave(paste0("water_depth_", sprintf("%04d", a), ".jpg"),
           plot = waterdepth, path = "out/boxplots/",
           width = bx_width, height = bx_height, dpi = dpi)

  }
}

# Create empty boxplot
waterdepth_NA <- waterdepth_base +
  annotate("text", x = 1, y = 5, label = "No Data", size = 3) +
  annotate(geom = "text", x = Inf, y = Inf, vjust = -0.5, hjust = 1.1,
           label = "n = 0", size = ann_size) +
  xlab("\n") + # To match regular plots +
  scale_y_reverse(limits = c(10, 0)) +
  scale_x_continuous(breaks = c(-1, 0, 1))

ggsave("water_depth_NA.jpg", plot = waterdepth_NA, path = "out/boxplots/",
       width = bx_width, height = bx_height, dpi = dpi)

# Monthly Water Level Plots -----------------------------------------------

# Right now these are created for potential use in Arc Dashboard, not used in
# aquifer factsheets

if(indiv_gwl_and_precip) {

  # Remove old files (make sure no old files to interfere)
  if(delete_old) file.remove(list.files("./out/gwl/",
                                        pattern = "groundwater",
                                        full.name = TRUE))

  message("\nCreating Monthly Water Level Plots")
  p <- progress_estimated(length(aquifers))
  for (a in aquifers) {
    p$tick()$print()

    aq_wl_month <- filter(wl_month_data, AQUIFER_ID == a)

    for(o in unique(aq_wl_month$OW)) {

      wl_month_sub <- filter(aq_wl_month, OW == o)

      # Will create figures for all wells with sufficent groundwater levels
      # Note: For combo plots (below), plots are only created if we ALSO have
      #       precipitation data.

      # Get number of years so we can warn if less than 10
      num_yrs <- wl_month_sub$num_yrs[1]

      # Includes number of years and date range
      # Exact text also changes depending on how many years
      wl_title <- case_when(num_yrs < 5 ~ "No Monthly Water Level Summary (only ",
                            num_yrs < 10 ~ "Preliminary Monthly Water Level Summary (",
                            TRUE ~ "Full Monthly Water Level Summary (") %>%
        paste0(., num_yrs, " years of data; ",
               wl_month_sub$min_yr[1], "-", wl_month_sub$max_yr[1], ")")

      # Plot
      g <- ggplot() +
        aq_theme +
        theme(legend.title = element_blank(),
              plot.title = element_text(size = 10)) +
        labs(x = "Month", y = "Depth to Groundwater (m below ground surface)",
             title = wl_title) +
        # Scales and Labels
        scale_colour_manual(values = c("Extreme Minimum" = "bisque3",
                                       "Median" = "black",
                                       "Extreme Maximum" = "slategray3")) +
        scale_fill_manual(values = c("10-90th Percentile" = "lightskyblue2",
                                     "25-75th Percentile" = "steelblue1")) +
        # Remove point from median line
        guides(colour = guide_legend(override.aes = list(shape = c(19, 19, NA))))

      # Add Water level if sufficient data
      if(num_yrs >= 5) {
        g <- g +
          geom_ribbon(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                               ymin = percentile_10,
                                               ymax = percentile_90,
                                               fill = "10-90th Percentile"),
                      alpha = 0.8) +
          geom_ribbon(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                               ymin = percentile_25,
                                               ymax = percentile_75,
                                               fill = "25-75th Percentile"),
                      alpha = 0.8) +
          geom_line(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                             y = median_median, colour = "Median")) +
          geom_point(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                              y = min_monthly_wl, colour = "Extreme Maximum")) +
          geom_point(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                              y = max_monthly_wl, colour = "Extreme Minimum"))# +
      } else {
        g <- g +
          ylim(0,1) +
          xlim(0,1) +
          annotate("text", x = 0.5, y = 0.5, size = 5,
                   label = "Insufficient Data")
      }

      ggsave(filename = paste0("./out/gwl/groundwater_",
                               sprintf("%04d", a),"_OW",
                               sprintf("%04d", o),".png"),
             plot = g,
             height = combo_height, width = combo_width, dpi = dpi)
    }
  }
}


# Monthly Precipitation Plots ----------------------------------------------
# Right now these are created for potential use in Arc Dashboard, not used in
# aquifer factsheets

if(indiv_gwl_and_precip) {
  # Remove old files (make sure no old files to interfere)
  if(delete_old) file.remove(list.files("./out/gwl/", pattern = "precip",
                                        full.name = TRUE))
  message("\nCreating Monthly Precipitation Plots")
  p <- progress_estimated(length(aquifers))
  for (a in aquifers) {
    p$tick()$print()

    ppt_sub <- filter(ppt_data, AQUIFER_ID == a)

    for(o in unique(ppt_sub$OW)) {

      ppt_sub <- filter(ppt_sub, OW == o) %>%
        mutate(precipitation = case_when(precipitation == "Total snowfall cm" ~
                                           "Total snowfall\n(rainfall equivalent)",
                                         precipitation == "Total rainfall mm" ~
                                           "Total rainfall (mm)"))

      climate_title <- tools::toTitleCase(as.character(ppt_sub$CLIMATE_NAME[1]))

      # Only continue if we have sufficient data for precipitation
      if(nrow(ppt_sub) > 0) {

        ppt_overall <- group_by(ppt_sub, month) %>%
          summarize(total = sum(ppt_mm, na.rm = TRUE))

        # Title with climate station
        ppt_title <- paste0("Climate Normals Based on ",
                            climate_title,
                            " Environment Canada Weather Station (1981-2010)")

        # Plot
        g <- ggplot(data = ppt_sub,
                    aes(x = month_abb, y = ppt_mm, fill = precipitation)) +
          aq_theme +
          theme(legend.title = element_blank(),
                plot.title = element_text(size = 10)) +
          geom_bar(stat = "identity", position = position_stack(reverse = TRUE),
                   color = "black") +
          scale_fill_manual(values = c("Total rainfall (mm)" = "lightcyan3",
                                       "Total snowfall\n(rainfall equivalent)" = "white")) +
          labs(x = "Month",
               y = paste0("Monthly Precipitation (mm) at\n", climate_title),
               title = ppt_title)

        ggsave(filename = paste0("./out/gwl/precip_",
                                 sprintf("%04d", a),"_OW",
                                 sprintf("%04d", o),".png"),
               plot = g,
               height = combo_height, width = combo_width, dpi = dpi)

      } else {
        # No blank figure, because we have no idea why there is no data
        # (perhaps just not downloaded yet)

        # Write an informative message to the console if there is no data for the ppt
        # message("AQUIFER_ID: ", a, " OBS WELL: ", o, ", Water level data, but ",
        # "no precipitation data\n(perhaps obs_wells_index is missing CLIMATE_ID ",
        # "for this aquifer)")
      }
    }
  }
}

# Combo Water level / Precip ----------------------------------------------

# Remove old files (make sure no old files to interfere)
if(delete_old) file.remove(list.files("./out/gwl/",
                                      pattern = "combo",
                                      full.name = TRUE))

message("\nCreating Monthly Combo Water Level/Precipitation Plots")
p <- progress_estimated(length(aquifers))
for (a in aquifers) {
  p$tick()$print()

  aq_wl_month <- filter(wl_month_data, AQUIFER_ID == a)

  for(o in unique(aq_wl_month$OW)) {

    wl_month_sub <- filter(aq_wl_month, OW == o)
    ppt_sub <- filter(ppt_data,
                      AQUIFER_ID == a,
                      OW == o) %>%
      mutate(precipitation = case_when(precipitation == "Total snowfall cm" ~
                                         "Total snowfall\n(rainfall equivalent)",
                                       precipitation == "Total rainfall mm" ~
                                         "Total rainfall (mm)"))

    climate_title <- tools::toTitleCase(as.character(ppt_sub$CLIMATE_NAME[1]))

    # Only continue if we have sufficient data for precipitation
    if(nrow(ppt_sub) > 0) {

      # Calculate the scaling to get the water level data on the same plot,
      # but scaled differently
      # - proportion is approximately 70% upper, 30% lower
      # - bandwidth is how spread appart the data WILL be
      # - position is where we want it to be
      # - max_range is the spread of the data to start with
      # - mult is the multiplier to get the data spread even farther to match
      #   the bandwidth (needs to be negative to flip the figure)
      # - shift is the value to move the data by to get it at the right place

      ppt_overall <- group_by(ppt_sub, month) %>%
        summarize(total = sum(ppt_mm, na.rm = TRUE))

      proportion <- 0.7

      wl_shift <- wl_month_sub %>%
        summarize(bandwidth = max(ppt_overall$total, na.rm = TRUE) * (proportion / (1 - proportion)),
                  position = max(ppt_overall$total, na.rm = TRUE) * 1.15,
                  max_range = max(max(max_monthly_wl, na.rm = TRUE) -
                                    min(min_monthly_wl, na.rm = TRUE)),
                  mult = -(bandwidth/max_range),
                  shift = -min(max_monthly_wl * mult) + position)

      # Breaks and labels for ppt axis
      breaks_ppt <- get_breaks(0, max(ppt_overall$total, na.rm = TRUE), length.out = 5)

      # Breaks for water-level axis
      breaks_wl <- data.frame(breaks = get_breaks(min(wl_month_sub$min_monthly_wl, na.rm = TRUE),
                                                  max(wl_month_sub$max_monthly_wl, na.rm = TRUE),
                                                  length.out = 10)) %>%
        mutate(gridlines = breaks * wl_shift$mult[1] + wl_shift$shift[1]) %>%
        filter(gridlines > (max(breaks_ppt) * 1.2))

      wl_month_sub <- wl_month_sub %>%
        cbind(wl_shift) %>%
        mutate(median_median = median_median * mult + shift,
               percentile_10 = percentile_10 * mult + shift,
               percentile_90 = percentile_90 * mult + shift,
               percentile_25 = percentile_25 * mult + shift,
               percentile_75 = percentile_75 * mult + shift,
               min_monthly_wl = min_monthly_wl * mult + shift,
               max_monthly_wl = max_monthly_wl * mult + shift)

      # Get number of years so we can warn if less than 10
      num_yrs <- wl_month_sub$num_yrs[1]

      # Complex title
      # - includes unicode (\u00B9 and \u00B2) to create superscripts 1 and 2
      # - \n creates new line between the two titles
      # - Includes number of years, date range, and the climate station
      # - Exact text also changes depending on how many years
      wl_title <- case_when(num_yrs < 5 ~ "No Monthly Water Level Summary (only ",
                            num_yrs < 10 ~ "Preliminary Monthly Water Level Summary (",
                            TRUE ~ "Full Monthly Water Level Summary (") %>%
        paste0(., num_yrs, " years of data; ",
               wl_month_sub$min_yr[1], "-", wl_month_sub$max_yr[1], ")") %>%
        paste0("\u00B9 ", ., "\n",
               "\u00B2 Climate Normals Based on ",
               climate_title,
               " Environment Canada Weather Station (1981-2010)")

      wl_month_sub <- filter(wl_month_sub, num_yrs >= 5)

      # Add precipitation
      g <- ggplot() +
        aq_theme +
        theme(legend.title = element_blank(),
              plot.title = element_text(size = 10)) +
        geom_bar(data = ppt_sub,
                 aes(x = month_abb, y = ppt_mm, fill = precipitation),
                 stat = "identity", position = position_stack(reverse = TRUE),
                 color = "black")

      # Add Water level if sufficient data
      if(nrow(wl_month_sub) > 0) {
        g <- g +
          # Add secondary axis
          scale_y_continuous(breaks = breaks_ppt, expand = c(0.02, 0),
                             sec.axis = sec_axis(~ ((. - wl_shift$shift[1]) / (wl_shift$mult[1])),
                                                 name = "Depth to Groundwater (m below ground surface)",
                                                 breaks = breaks_wl$breaks)) +
          # Artificially add in grid lines
          geom_hline(aes(yintercept = breaks_wl$gridlines),
                     colour = aq_theme$panel.grid.major$colour) +
          # Add data
          geom_ribbon(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                               ymin = percentile_10,
                                               ymax = percentile_90,
                                               fill = "10-90th Percentile"),
                      alpha = 0.8) +
          geom_ribbon(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                               ymin = percentile_25,
                                               ymax = percentile_75,
                                               fill = "25-75th Percentile"),
                      alpha = 0.8) +
          geom_line(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                             y = median_median, colour = "Median")) +
          geom_point(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                              y = min_monthly_wl, colour = "Extreme Maximum")) +
          geom_point(data = wl_month_sub, aes(x = as.numeric(month_abb),
                                              y = max_monthly_wl, colour = "Extreme Minimum"))
      }

      g <- g +
        # Scales and Labels
        # These add specific colours to the lables assigned to the aes above
        scale_colour_manual(values = c("Extreme Minimum" = "bisque3",
                                      "Median" = "black",
                                      "Extreme Maximum" = "slategray3")) +
        scale_fill_manual(values = c("Total rainfall (mm)" = "lightcyan3",
                                     "Total snowfall\n(rainfall equivalent)" = "white",
                                     "10-90th Percentile" = "lightskyblue2",
                                     "25-75th Percentile" = "steelblue1")) +
        # Remove point from median line
        guides(colour = guide_legend(override.aes = list(shape = c(19, 19, NA)))) +
        labs(x = "Month",
             y = paste0("Monthly Precipitation (mm) at\n", climate_title),
             title = wl_title)

      ggsave(filename = paste0("./out/gwl/combo_",
                               sprintf("%04d", a),"_OW",
                               sprintf("%04d", o),".png"),
             plot = g,
             height = combo_height, width = combo_width, dpi = dpi)

    } else {
      # Write an informative message to the console if there is no data for the ppt
      # message("AQUIFER_ID: ", a, " OBS WELL: ", o, ", Water level data, ",
      #         "but no precipitation data\n(perhaps obs_wells_index is missing ",
      #         "CLIMATE_ID for this aquifer)")
    }
  }
}


# Groundwater level trend plot --------------------------------------------

# Remove old files (make sure no old files to interfere)
if(delete_old) file.remove(list.files("./out/trends/", full.name = TRUE))

message("\nCreating Trend Plots")
p <- progress_estimated(length(aquifers))
for (a in aquifers) {
  p$tick()$print()

  d <- filter(ground_water, AQUIFER_ID == a)

  for(o in unique(d$OW)) {
    #message(a, "-", o)
    plotdata <- filter(ground_water, AQUIFER_ID == a, OW == o) %>%
      mutate(Well_Num = OW)

    well.attr <- filter(ground_water_trends, AQUIFER_ID == a, OW == o) %>%
      mutate(Well_Num = OW)

    # Skip plot if < 5 years of data
    if(well.attr$nYears < 5) next

    g <- gwl_area_plot(data = plotdata, trend = well.attr$trend_line_slope,
                       intercept = well.attr$trend_line_int,
                       trend_category = well.attr$state, sig = well.attr$sig,
                       showInterpolated = TRUE, save = FALSE,
                       mkperiod = "annual", show_stable_line = FALSE) +
      labs(title = NULL) +
      theme(legend.position = "right", legend.box = "vertical",
            legend.margin = margin(0,
                                   # Add extra spacing if no interpolated values in legend
                                   if_else(any(plotdata$nReadings == 0), 5.5, 45),
                                   0, 5.5),
            legend.spacing = unit(0, units = "mm"))

    ggsave(plot = g,
           filename = paste0("./out/trends/trends_",
                             sprintf("%04d", as.numeric(a)),"_OW",
                             sprintf("%04d", as.numeric(o)),".png"),
           height = trend_height, width = trend_width, dpi = dpi)
  }
}
