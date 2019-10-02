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

#
# Clean and Prepare Data
#

# Setup -------------------------------------------------------------------

# Check for aquifer numbers
if(!exists("aquifers")) {
  stop("Can't run 02_clean.R without first specifying which aquifers to summarize", call. = FALSE)
} else if(length(aquifers) == 0) {
  stop("'aquifers' is empty, specify at least one aquifer to summarize", call. = FALSE)
} else if(!is.vector(aquifers)) {
  stop("'aquifers' should be a vector of aquifer ids")
}

# Load functions, packages and data
source("00_functions.R")
source("00_header.R")
load("tmp/aquifer_factsheet_data.RData")

# Master Aquifer Data -----------------------------------------------------

# Set up a master aquifer data frame with pertinent information we can add to

# For when using downloaded data from here: https://catalogue.data.gov.bc.ca/dataset/ground-water-aquifers
# aquifer_db <- rename_all(aquifer_db_raw, tolower) %>%
#   select(aquifer_id = aq_tag, aquifer_subtype_code, aquifer_classification, aquifer_materials,
#          aquifer_name, vulnerability, productivity, descriptive_location, size_km2) %>%
#   # Check for encoded characters (i.e. \x92 for ' etc.)
#   mutate_if(is.character, funs(str_replace_all(., "\x92", "'"))) %>%
#   mutate(aquifer_id = as.numeric(aquifer_id))

aquifer_db <- rename_all(aquifer_db_raw, tolower) %>%
  select(aquifer_id, aquifer_subtype_code, aquifer_classification, aquifer_materials,
         aquifer_name, vulnerability, productivity, descriptive_location, size_km2) %>%
  # Check for encoded characters (i.e. \x92 for ' etc.)
  mutate_if(is.character, funs(str_replace_all(., "\x92", "'"))) %>%
  mutate(aquifer_id = as.numeric(aquifer_id))


# Set up Wells Database with pertinent information
# - Get max number of digits after decimal:
#   str_remove(wells_db_raw$longitude, "^[-0-9]+.") %>% nchar() %>% unique()
# - Use this to round (gets rid of weird differences in numbers
wells_db <- wells_db_raw %>%
  select(aquifer_id, well_tag_number, bcgs_id,
         well_yield,              # equivalent to YEILD_VALUE
         well_yield_unit_code,    # equivalent to YIELD_UNIT_CODE
         ow = observation_well_number, ow_status = obs_well_status_code,
         static_water_level,      # equivalent to WATER_DEPTH
         finished_well_depth,     # equivalent to DEPTH_WELL_DRILLED
         latitude,
         longitude) %>%
  mutate(ow = as.numeric(ow))

climate_index_orig <- climate_index %>%
  select(aquifer_id = `Aquifer No`,
         ow = `Obs well`,
         location = Location,
         climate_id = `Climate ID`,
         climate_name = `Nearest climate station`) %>%
  filter(!str_detect(aquifer_id, "(round)|(inactive)")) %>%  # Remove "round 1", etc. from end of file (also NAs)
  mutate(climate_id = as.character(climate_id),
         aquifer_id = as.numeric(aquifer_id))


obs_wells_index <- wells_db %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  select(aquifer_id, ow, well_tag_number, ow_status) %>%
  distinct() %>%
  arrange(aquifer_id, ow)


# Master - Number of Wells ---------------------------------------------------

# Calculate and add reported number of wells to Aquifer data
aquifer_db <- wells_db %>%
  group_by(aquifer_id) %>%
  summarize(reported_no_wells = n()) %>%
  left_join(aquifer_db, ., by = "aquifer_id") %>%
  # Fill NAs with zeros
  mutate(reported_no_wells = replace(reported_no_wells,
                                     is.na(reported_no_wells), 0))

# Calculate number of OBSERVATION wells (inactive and active wells)
n_obswells <- wells_db %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  select(aquifer_id, ow, ow_status) %>%
  distinct()

# Save index to file for factsheet_template.Rmd
write_csv(n_obswells, "./out/aquifer_ow.csv")

# Clean up
rm(n_obswells)

# DECISION - wells table has multiple cases where an obs well number is assigned but may not be an obs well?
# eg. obs well no. given for wells classified as UNK, DOM, NA, AND OBS - what should we use?



# Master - Hydraulic Connectivity ----------------------------------------------

# Format and Add Hydraulic Connectivity to Aquifer Data
aquifer_db <- hydraulic_connectivity %>%
  rename_all(tolower) %>%
  rename(aquifer_subtype_code = aquifer_subtype) %>%
  bind_rows(tibble(aquifer_subtype_code = "UNK", hydraulic_connectivity = "Unknown")) %>%
  left_join(aquifer_db, ., by = "aquifer_subtype_code")

# Master - Location/description -------------------------------------------

# Mapping Dates (Not Available)


# Location Description and Region
aquifer_db <- aquifer_loc_region %>%
  rename_all(tolower) %>%
  left_join(aquifer_db, ., by = "aquifer_id") %>%
  mutate(region = if_else(is.na(region), "Unknown", region))

# Water District
aquifer_db <- water_district %>%
  rename_all(tolower) %>%
  left_join(aquifer_db, ., by = "aquifer_id") %>%
  mutate(water_district = if_else(is.na(water_district),
                                  "Unknown",
                                  water_district))

# Aquifer Subtype Descriptions
aquifer_db <- aquifer_subtypes %>%
  rename_all(~str_replace_all(tolower(.), " ", "_")) %>%
  select(aquifer_subtype_code, description) %>%
  mutate(description = replace(description,
                               description == "Unkonwn",
                               "Unknown")) %>%
  left_join(aquifer_db, ., by = "aquifer_subtype_code")


# Master - Licences -------------------------------------------------------
# Prepare license data
# Get hydraulic connectivity and licensing from licencing data
aquifer_db <- licenced_vol %>%
  rename_all(tolower) %>%
  select(licence_number = lcnc_nmbr, pod_subtype = pd_sbtype, aquifer_id = source_nm) %>%
  filter(pod_subtype %in% c("PWD", "PG")) %>% # Groundwater-only licences
  mutate(aquifer_id = suppressWarnings(as.numeric(aquifer_id))) %>%
  filter(!is.na(aquifer_id)) %>%
  distinct() %>% # Some licences tied to more than one well within an aquifer, but still, just one licence
  group_by(aquifer_id) %>%
  summarize(n_licences = n()) %>%
  left_join(aquifer_db, ., by = "aquifer_id") %>%
  # Fill missing counts with zero
  mutate(n_licences = replace(n_licences, is.na(n_licences), 0))

# Master - Stress Indices ----------------------------------------------
aquifer_db <- stress_index %>%
  select(aquifer_id = AQ_NUM,
         aquifer_pumping_stress_index = Result) %>%
  left_join(aquifer_db, ., by = "aquifer_id")

# Data for boxplots ---------------------------------------------------

# Converting Yield from GPM to L/s and Feet to Metres

wells_db <- wells_db %>%
  mutate(well_yield = well_yield * 0.06309,
         well_yield_unit_code = "L/s",
         finished_well_depth_m = finished_well_depth * 0.3048,
         static_water_level_m = static_water_level * 0.3048) %>%
  select(-finished_well_depth, -static_water_level)


# Groundwater data --------------------------------------------------------
# Add Aquifer and OW ids to SOE data
ground_water <- select(obs_wells_index, aquifer_id, ow) %>%
  distinct() %>%
  right_join(ground_water, by = c("ow" = "Well_Num")) %>%
  filter(!is.na(aquifer_id),
         !is.na(ow))

ground_water_trends <- select(obs_wells_index, aquifer_id, ow) %>%
  distinct() %>%
  right_join(ground_water_trends, by = c("ow" = "Well_Num", "aquifer_id")) %>%
  filter(!is.na(aquifer_id),
         !is.na(ow))

# Climate index from weathercan -------------------------------------------
# Get three closest stations within 100km
locs <- wells_db %>%
  select(aquifer_id, ow, latitude, longitude) %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  # Round lat/lon because some OW are off by tiny amounts between observations
  # and we don't care (stations only has 2 decimal places)
  mutate(latitude = round(latitude, 4),
         longitude = round(longitude, 4)) %>%
  distinct() %>%
  mutate(stations = map2(latitude, longitude,
                         ~stations_search(coords = c(.x, .y), dist = 100,
                                          normals_only = TRUE) %>%
                           select(station_name, climate_id, lat_climate = lat,
                                  lon_climate = lon, elev_climate = elev,
                                  distance) %>%
                           mutate(climate_id = as.character(climate_id),
                                  n = 1:n()) %>%
                           slice(1:3))) %>%
  unnest(stations)

# Any without 3 stations?
locs %>%
  group_by(aquifer_id, ow) %>%
  count() %>%
  filter(n != 3)

# Climate Normals - Precipitation Data ----------------------------------------
ppt <- locs %>%
  pull(climate_id) %>%
  unique() %>%
  normals_dl() # Download the climate normals for all these stations

# Public data, so all codes must be D or better
# (therefore don't have to filter by code quality)
ppt_good <- unnest(ppt, normals) %>%
  select(climate_id, meets_wmo, month = period,
         rain, snow) %>%
  filter(month != "Year") %>%
  group_by(climate_id) %>%
  filter(sum(is.na(rain)) == 0, sum(is.na(snow)) == 0) %>%
  nest(normals = c(month, rain, snow)) %>%
  left_join(locs, ., by = "climate_id") %>%
  mutate(data = map_lgl(normals, is.data.frame))

# Get data from closest WMO station (within 15km), or from closest station
ppt_normals <- ppt_good %>%
  mutate(close_wmo = meets_wmo & distance <= 15 & data) %>%
  group_by(aquifer_id, ow) %>%
  mutate(keep = case_when(close_wmo ~ TRUE,
                          any(close_wmo) ~ FALSE,
                          data ~ TRUE,
                          TRUE ~ TRUE)) %>%
  filter(keep) %>%
  slice(1)

# Use stations as climate index
obs_wells_index_climate <- ppt_normals %>%
  select(aquifer_id, ow, climate_id, climate_name = station_name) %>%
  mutate(climate_name = tools::toTitleCase(tolower(climate_name)),
         climate_name = str_replace_all(climate_name,
                                        c(" a$" = " Airport",
                                          "Int'l" = "International")),
         climate_name = str_replace(climate_name,
                                    " (Cs|Cda|Rcs|Awos|Se)(?= |$)",
                                    toupper))

# Finalize ppt normals
ppt <- ppt_normals %>%
  select(aquifer_id, ow, normals) %>%
  unnest(normals) %>%
  gather(key = "precipitation", value = "ppt_mm", rain, snow)

# Water level data ----------------------------------------------------

# Read the data and format the dates and date columns
wl_all <- obs_well %>%
  rename(date = QualifiedTime, ow = myLocation, wl = Value) %>%
  filter(ow != "myLocation") %>%
  mutate(date = ymd(date),
         ow = as.numeric(str_extract(ow, "[0-9]*$")),
         wl = as.numeric(wl),
         year = year(date),
         month = month(date),
         month_text = month(date, label = TRUE),
         day = day(date),
         month_year = paste0(month_text, "-", year))

wl_month_extremes <- wl_all %>%
  group_by(ow, month) %>%
  summarise(min_monthly_wl = min(wl),
            max_monthly_wl = max(wl))

# Calculate median water level for each month/year then for each month
wl_month <- wl_all %>%
  group_by(ow, year, month) %>%
  summarize(mean_monthly_wl = mean(wl),
            min_monthly_wl = min(wl),
            max_monthly_wl = max(wl),
            median_monthly_wl = median(wl)) %>%
  group_by(ow, month) %>%
  summarise(median_median = median(median_monthly_wl),
            mean_median = mean(median_monthly_wl),
            percentile_25 = quantile(median_monthly_wl, 0.25),
            percentile_75 = quantile(median_monthly_wl, 0.75),
            percentile_10 = quantile(median_monthly_wl, 0.10),
            percentile_90 = quantile(median_monthly_wl, 0.90)) %>%
  ungroup() %>%
  mutate(month_abb = month(month, label = TRUE))

# Summarize years to see max, min and number of years of data
wl_summary <- wl_all %>%
  group_by(ow) %>%
  summarize(min_yr = min(year),
            max_yr = max(year),
            num_yrs = max_yr - min_yr)

wl_month <- wl_month %>%
  left_join(wl_month_extremes, by = c("ow", "month")) %>%
  left_join(wl_summary, by = "ow") %>%
  left_join(distinct(select(obs_wells_index_climate, aquifer_id, ow)), by = "ow")


# Save Data ---------------------------------------------------------------

# Save RDS files
save(aquifer_db,
     wells_db,
     wl_month,
     wl_all,
     ppt,
     ppt_normals,
     ground_water,
     ground_water_trends,
     file = "tmp/aquifer_factsheet_clean_data.RData")

# Save .csv files to pull in by aquifer factsheets
write_csv(aquifer_db, path = "./out/aquifer_table.csv")
write_csv(wells_db, path = "./out/wells_table.csv")
