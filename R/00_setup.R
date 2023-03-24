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


prep_dir <- function() {
  ## Create project directories
  if (!dir.exists("tmp")) dir.create("tmp")
  if (!dir.exists("data")) dir.create("data")
  if (!dir.exists("data_dl")) dir.create("data_dl")
  if (!dir.exists("out")) dir.create("out")
  if (!dir.exists("out/gwl")) dir.create("out/gwl")
  if (!dir.exists("out/boxplots")) dir.create("out/boxplots")
  if (!dir.exists("out/trends")) dir.create("out/trends")
  if (!dir.exists("out/piperplots")) dir.create("out/piperplots")
  if (!dir.exists("out/archive")) dir.create("out/archive")
  if (!dir.exists("factsheets")) dir.create("factsheets")
  if (!dir.exists("figures")) dir.create("figures")
  if (!dir.exists("figures/maps")) dir.create("figures/maps")
  if (!dir.exists("figures/piperplots")) dir.create("figures/piperplots")
  if (!dir.exists("figures/piperplots_trimmed")) dir.create("figures/piperplots_trimmed")
  if (!dir.exists("figures/extra")) dir.create("figures/extra")
  if (!dir.exists("figures/na")) dir.create("figures/na")
}

#' # Figure defaults ---------------------------------------------------------
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
  theme_bw() +
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
    axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm"), size = 9),
    panel.grid.major = element_line(colour = "grey75"),
    panel.grid.minor = element_line(colour = "grey75"))
}

# Create gradient background (same for each Yield Boxplot)
y_gradient <- function() {
  grid::rasterGrob(c("#FFFFFF00", "#A2B5CD90"),
                   width = unit(1, "npc"),
                   height = unit(1, "npc"), interpolate = TRUE)
}
