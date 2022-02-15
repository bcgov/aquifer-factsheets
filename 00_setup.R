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


##############################
# Preparation
##############################

# Use renv for reproducible package management
renv::restore()

## Load required packages
suppressMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(sf)
  library(stringr)
  library(lubridate)
  library(httr)

  library(ggplot2)
  library(scales)
  library(magick)

  library(knitr)
  library(rmarkdown)
  library(kableExtra)

  library(bcgroundwater)
  library(bcdata)
  library(rems2aquachem)

  library(weathercan)
  library(smwrBase)
  library(smwrGraphs)
})

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

source("functions.R")
