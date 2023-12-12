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
dirs <- c(
  fs::dir_create(c("1_inputs", "2_outputs", "3_factsheets")),
  fs::dir_create("1_inputs", c("archive", "data", "figures", "templates")),
  fs::dir_create("1_inputs", "figures", c("na", "maps", "extra")),
  fs::dir_create("2_outputs", c("archive", "data_dl", "figures")),
  fs::dir_create("2_outputs", "figures",
                 c("boxplots", "gwl_ppt", "gwl_trends", "piperplots"))
)

dirs <- paste0(stringr::str_extract(dirs, "(?<=(1|2)_)in|out|factsheets"), "_",
               stringr::str_extract(
                 dirs, paste0("archive|data_dl|data|templates|maps|extra|boxplots|",
                              "na|gwl_ppt|gwl_trends|piperplots|(figures$)|(puts$)"))) |>
  stringr::str_remove("_NA$|(_(?=puts))") |>
  setNames(dirs, nm = _)




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


