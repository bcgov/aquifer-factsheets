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
# Load/Aquire Data
##############################

# Load Packages and Functions --------------------------------------------------------
source("00_header.R")
source("00_functions.R")

# Read Local Data ---------------------------------------------------------------
# These data sets must be available in the 'data' folder

aquifer_table <- read_csv("data/GW_AQUIFER_ATTRS_DATA_TABLE.csv") # aquifer database
wells_database_RAW <- read_csv("data/WELLS_WELLS.csv", guess_max = 100000) # WELLS Database raw output
aquifer_subtype_descriptions <- read_csv("data/WELLS_AQUIFER_SUBTYPE_CODES_DATA_TABLE.csv") # Created
obswell_to_aquifer <- read_excel("data/Key_Aquifer_Obs well_EMS ID_climate ID.xlsx", sheet = 1)
wells_aquifer_connection <- read_csv("data/GW_AQUIFER_WELLS_DATA_TABLE.csv") # from wells - some errors
stress_index <- read_excel("./data/UVic stress index results.xlsx", sheet = 1)

hydraulic_connectivity <-read_csv("data/Hydraulic Connectivity Table.csv") # likelihood based on subtype
water_district <- read_csv("data/Aquifer_Water_District.csv")
aquifer_loc_region <- read_csv("data/Aquifer Location Description and Regions.csv")

ppt_data <- read_csv("data/All stations BC_CCN_rainfall_snowfall_precip.csv", guess_max = 2000)

licenced_vol <- read.csv("./data/licenced_vol_dec2017.csv")

# Download Remote Data --------------------------------------------------------
wl_data <- read_csv("http://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/ObservationWellDataAll_DailyMean.csv")

# TEMPORARY FIX UNTIL groundwater-levels-indicator SOE data has been finalized
# These RData files are the output from groundwater-levels-indicator
# files 01_clean.R and 02_analysis.R
# https://github.com/bcgov/groundwater-levels-indicator/tree/update-2018
load("data/raw_data.RData")
ground_water <- monthlywells_ts

load("data/analysis_data.RData")
ground_water_trends <- results_out


# Save all data -----------------------------------------------------------
save(hydraulic_connectivity, aquifer_loc_region, stress_index, licenced_vol,
     water_district, obswell_to_aquifer, aquifer_subtype_descriptions, wells_database_RAW,
     aquifer_table, wells_aquifer_connection, wl_data, ppt_data, ground_water, ground_water_trends,
     file = "tmp/aquifer_factsheet_data.RData")

