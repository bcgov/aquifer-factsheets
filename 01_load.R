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

# Aquifer data
aquifer_db_raw <- read_csv("data/GW_AQUIFER_ATTRS_DATA_TABLE.csv") # aquifer database

# Manually Collected data
climate_index <- read_excel("data/Key_Aquifer_Obs well_EMS ID_climate ID.xlsx", sheet = 1)
ppt_data <- read_csv("data/All stations BC_CCN_rainfall_snowfall_precip.csv", guess_max = 2000) # Manual Download
licenced_vol <- read_excel("data/WLS_WRL_P.xlsx") # https://catalogue.data.gov.bc.ca/dataset/5549cae0-c2b1-4b96-9777-529d9720803c

# Unknown sources (GIS?)
hydraulic_connectivity <- read_csv("data/Hydraulic Connectivity Table.csv") # likelihood based on subtype. Possibly determined from https://catalogue.data.gov.bc.ca/dataset/water-rights-licences-public (but more likely from GIS layers overlay)
water_district <- read_csv("data/Aquifer_Water_District.csv") # Possibly determined from https://catalogue.data.gov.bc.ca/dataset/water-management-districts by using GIS analysis of overlap
aquifer_loc_region <- read_csv("data/Aquifer Location Description and Regions.csv") # Manual GIS analysis?

# Read Downloaded Data ----------------------------------------------------
# These data sets must be available in the 'data_dl' folder
# Run the '00_download.R' script to automatically download and extract these files
aquifer_subtypes <- read_excel("data_dl/aquifer_subtypes.xlsx", sheet = 1)

wells_db_raw <- read_csv("data_dl/well.csv", guess_max = 100000)
wells_lithology <- read_csv("data_dl/lithology.csv", guess_max = 100000)

#licenced_vol <- read_csv("data_dl/licenced_vol.csv", guess_max = 50000)
stress_index <- read_excel("./data_dl/uvic_stress_index.xlsx", sheet = "R0. Results")

obs_well <- read_csv("data_dl/obs_well_daily_mean.csv")

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
     water_district, aquifer_subtypes, wells_db_raw, wells_lithology, obs_well,
     aquifer_db_raw, climate_index, ppt_data, ground_water, ground_water_trends,
     file = "tmp/aquifer_factsheet_data.RData")

