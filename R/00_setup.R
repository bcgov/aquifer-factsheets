# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


# Preparation -------------------------------------------

## Load required packages
# suppressMessages({

#   source("00_functions.R")
# })

# Setup the file locations
f <- c(
  "inputs" =               "1_inputs/",
  "inputs_archive" =       "1_inputs/archive/",

  "inputs_piperplots_text"="1_inputs/piper_text.xlsx",
  "inputs_extra" =         "1_inputs/extra/",
  "inputs_extra_docx" =    "1_inputs/extra/docx/",

  "inputs_figures" =       "1_inputs/figures/",
  "inputs_maps" =          "1_inputs/figures/maps/",

  "inputs_na_gwl_ppt" =    "1_inputs/figures/na/figure_missing_gwl_ppt.png",
  "inputs_na_gwl_trends" = "1_inputs/figures/na/figure_missing_gwl_trends.png",
  "inputs_na_piperplots" = "1_inputs/figures/na/figure_missing_piperplots.png",

  "inputs_templates" =     "1_inputs/templates/",
  "template_factsheet" =   "1_inputs/templates/factsheet_template.Rmd",

  "outputs" =              "2_outputs/",
  "outputs_archive" =      "2_outputs/archive/",
  "outputs_data_dl" =      "2_outputs/data_dl/",
  "outputs_wells"   =      "2_outputs/wells/",
  "outputs_final"  =       "2_outputs/final/",

  "outputs_extra_txt" =    "2_outputs/extra/txt/",
  "outputs_extra" =        "2_outputs/extra/",

  "outputs_figures" =      "2_outputs/figures/",
  "outputs_boxplots" =     "2_outputs/figures/boxplots/",
  "outputs_gwl_ppt" =      "2_outputs/figures/gwl_ppt/",
  "outputs_gwl_trends" =   "2_outputs/figures/gwl_trends/",
  "outputs_piperplots" =   "2_outputs/figures/piperplots/",

  "factsheets" =           "3_factsheets/",
  "logs" =                 "4_logs/"
  )

# Create folders if they do not already exist
f |>
  stringr::str_subset("\\.(.)+$", negate = TRUE) |>
  fs::dir_create()

# Types of page 3 extra content -------------------------------------------
# `type` is the key
# `heading` is the Pretty heading it will get in the output
# `order` is the order of pages if there are more than one (i.e. Cross-section preceeds Water Budget)
# `match` is the characters to match in the file name... so they should be consistent
e_types <- dplyr::tribble(
  ~type, ~heading, ~order, ~match,
  "cross_section", "Cross-Section", 1, "Cross Section",
  "water_budget", "Water Budget", 2, "Water Budget",
  "numerical_model", "Numerical Groundwater Flow Model", 3, "Numerical Model",
  "water_quality", "Water Quality", 4, "Water Quality Info")


# Figure defaults ---------------------------------------------------------
bx_height <- 5.4
bx_width <- 1.8

combo_height <- 3.85
combo_width <- 10

trend_height <- 3.85
trend_width <- 10

piper_height <- 3.85
piper_width <- 5

dpi <- 300

ann_size <- 2.75  # Annotation sizes for samples sizes in boxplots

# General factsheet plot theme
aq_theme <- function() {
  update_geom_defaults("text", list(family = "Helvetica"))
  update_geom_defaults("label", list(family = "Helvetica"))

  theme_bw(base_family = "Helvetica") +
    theme(axis.title.y.left = element_text(margin = unit(c(0, 2, 0, 0), "mm")),
          axis.title.y.right = element_text(margin = unit(c(0, 0, 0, 2), "mm")),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey90"))
}

# Specific to boxplots
bx_theme <- function() {
  theme(
    plot.margin = unit(c(1, 3, 1, 3), "mm"),
    axis.title.y = element_text(margin = unit(c(0, 0, 0, 0), "mm")),
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm"), size = 9,
                                vjust = 1),
    panel.grid.major = element_line(colour = "grey75"),
    panel.grid.minor = element_line(colour = "grey75"))
}

# Create gradient background (same for each Yield Boxplot)
y_gradient <- function() {
  grid::rasterGrob(c("#FFFFFF00", "#A2B5CD90"),
                   width = unit(1, "npc"),
                   height = unit(1, "npc"), interpolate = TRUE)
}


