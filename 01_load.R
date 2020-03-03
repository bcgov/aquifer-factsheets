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
# Load/Acquire Data
##############################

# Load Packages and Functions --------------------------------------------------------
source("00_header.R")
source("00_functions.R")

# Read Local Data ---------------------------------------------------------------
# These data sets must be available in the 'data' folder

# None! Only aquifer map images, piperplots and piperplot text need to be provided.

# Read Downloaded Data ----------------------------------------------------
# These data sets must be available in the 'data_dl' folder
# Run the '00_download.R' script to automatically download and extract these files
aquifer_db_raw <- read_csv("data_dl/aquifers.csv")
aquifer_subtypes <- read_csv("data_dl/aquifer_subtypes.csv")
aquifer_map <- read_rds("data_dl/aquifer_map.rds")

wells_db_raw <- read_csv("data_dl/well.csv", guess_max = 200000)
wells_lithology <- read_csv("data_dl/lithology.csv", guess_max = 100000)

licenced_vol <- read_csv("data_dl/aquifer_licences.csv", guess_max = 10000)
stress_index <- read_excel("data_dl/uvic_stress_index.xlsx", sheet = "R0. Results")

obs_well <- read_csv("data_dl/obs_well_daily_mean.csv")


# Create small data frames ------------------------------------------------
# Hydraulic Connectivity
# (originally from "Hydraulic Connectivity Table.csv" of unknown origin)
hc <- tribble(~aquifer_subtype_code, ~hydraulic_connectivity,
              "1a", "Likely",
              "1b", "Likely",
              "1c", "Likely",
              "2", "Likely",
              "3", "Likely",
              "4a", "Likely",
              "4b", "Not Likely",
              "4c", "Not Likely",
              "5a", "Not Likely",
              "5b", "Likely",
              "6a", "Not Likely",
              "6b", "Not Likely",
              "UNK", "Unknown")


# TEMPORARY FIX UNTIL groundwater-levels-indicator SOE data has been finalized
# These RData files are the output from groundwater-levels-indicator
# files 01_clean.R and 02_analysis.R
# https://github.com/bcgov/groundwater-levels-indicator/

load("data/clean_well_data.RData")
ground_water <- monthlywells_ts

#load("data/analysis_data.RData")
#ground_water_trends <- results_out

#ground_water <- read_csv("./data_dl/groundwater.csv")
ground_water_trends <- read_csv("./data_dl/groundwater_trends.csv")

# Save all data -----------------------------------------------------------
save(hc, stress_index, licenced_vol,
     aquifer_db_raw, aquifer_map, aquifer_subtypes,
     wells_db_raw, wells_lithology, obs_well,
     ground_water, ground_water_trends,
     file = "tmp/aquifer_factsheet_data.RData")

