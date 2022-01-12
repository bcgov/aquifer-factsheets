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

#
# Clean and Prepare Data
#

# Setup -------------------------------------------------------------------

# Check for aquifer numbers
if(!exists("aquifers")) {
  stop("Can't run 03_clean.R without first specifying which aquifers to summarize", call. = FALSE)
} else if(length(aquifers) == 0) {
  stop("'aquifers' is empty, specify at least one aquifer to summarize", call. = FALSE)
} else if(!is.vector(aquifers)) {
  stop("'aquifers' should be a vector of aquifer ids")
}

# Load functions, packages and data
source("functions.R")
source("header.R")
load("tmp/aquifer_factsheet_data.RData")

# Master Aquifer Data -----------------------------------------------------

# Set up a master aquifer data frame with pertinent information we can add to
aquifer_db <- aquifer_db_raw %>%
  select(aquifer_id, aquifer_name, descriptive_location = location_description,
         aquifer_materials = material, aquifer_subtype_code = subtype,
         vulnerability, productivity, demand, size_km2 = area, mapping_year) %>%
  mutate_at(c("vulnerability", "productivity", "demand"),
            ~str_extract(., "High|Moderate|Low")) %>%
  mutate(aquifer_classification =
           case_when(vulnerability == "High" & demand == "High" ~ "IA",
                     vulnerability == "High" & demand == "Moderate" ~ "IIA",
                     vulnerability == "High" & demand == "Low" ~ "IIIA",
                     vulnerability == "Moderate" & demand == "High" ~ "IB",
                     vulnerability == "Moderate" & demand == "Moderate" ~ "IIB",
                     vulnerability == "Moderate" & demand == "Low" ~ "IIIB",
                     vulnerability == "Low" & demand == "High" ~ "IC",
                     vulnerability == "Low" & demand == "Moderate" ~ "IIC",
                     vulnerability == "Low" & demand == "Low" ~ "IIIC"),
         aquifer_subtype_code = str_extract(aquifer_subtype_code, "^[^- ]*"),
         retired = str_detect(tolower(aquifer_name), "retired|merged"),
         retired = if_else(is.na(retired), FALSE, retired))


# Remove retired aquifers with message
if(any(aquifers %in% aquifer_db$aquifer_id[aquifer_db$retired])) {
  a <- aquifers[aquifers %in% aquifer_db$aquifer_id[aquifer_db$retired]]
  message("Retired aquifers removed from run: ", paste0(a, collapse = ", "))
  aquifers <- aquifers[!aquifers %in% a]
}

# Set up Wells Database with pertinent information
# - Get max number of digits after decimal:
#   str_remove(wells_db_raw$longitude, "^[-0-9]+.") %>% nchar() %>% unique()
# - Use this to round (gets rid of weird differences in numbers
wells_db <- wells_db_raw %>%
  select("aquifer_id", "well_tag_number",
         "well_yield" = "well_yield_usgpm",
         "well_yield_unit_code",
         "ow" = "observation_well_number",
         "ow_status" = "obs_well_status_code",
         "static_water_level" = "static_water_level_ft-btoc",
         "finished_well_depth" = "finished_well_depth_ft-bgl",
         "latitude" = "latitude_Decdeg",
         "longitude" = "longitude_Decdeg",
         "licenced_status_code",
         "conductivity" = "hydraulic_conductivity_m/s",
         "transmissivity" = "transmissivity_m^2/s",
         "storativity") %>%
  mutate(ow = as.numeric(ow))

obs_wells_index <- wells_db %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  select(aquifer_id, ow, well_tag_number, ow_status) %>%
  distinct() %>%
  arrange(aquifer_id, ow)


# Master - Number of Wells ---------------------------------------------------
message("  Number of wells")
# Calculate and add reported number of wells to Aquifer data
aquifer_db <- wells_db %>%
  group_by(aquifer_id) %>%
  summarize(reported_no_wells = n(), .groups = "drop") %>%
  left_join(aquifer_db, ., by = "aquifer_id") %>%
  # Fill NAs with zeros
  mutate(reported_no_wells = replace_na(reported_no_wells, 0))

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

# Master - Hydraulic properties ---------------------------------------------------
message("  Hydraulic properties")
# Calculate and add hydraulic properties wells to Aquifer data

min_na <- function(x) if(!all(is.na(x))) min(x, na.rm = TRUE) else NA_real_
max_na <- function(x) if(!all(is.na(x))) max(x, na.rm = TRUE) else NA_real_

aquifer_db <- wells_db %>%
  group_by(aquifer_id) %>%
  summarize(across(
    .cols = c("conductivity", "transmissivity", "storativity"),
    .fns = list(min = min_na, max = max_na, n = ~sum(!is.na(.x))))) %>%
  left_join(aquifer_db, ., by = "aquifer_id")

# Master - (GIS) Aquifer Water Districts ---------------------------
message("  Water Districts")
# Overlap aquifer maps on water_district map
wd <- st_intersection(select(bcmaps::water_districts(), water_district = DISTRICT_NAME),
                      select(aquifer_map, aquifer_id)) %>%
  # Calculate area of overlap
  mutate(area = as.numeric(st_area(.))) %>%
  # Calculate dominant water district (max area)
  group_by(aquifer_id) %>%
  mutate(parea = area / sum(area) * 100,
         n = n()) %>%
  arrange(desc(parea)) %>%
  mutate(water_district = if_else(n > 1 & all(parea < 95),
                                  paste0(water_district, collapse = " and "),
                                  water_district)) %>%
  filter(parea == max(parea)) %>%
  ungroup() %>%
  select(-area) %>%
  # Remove spatialness
  st_set_geometry(NULL)

# Temporary fix for #496
wd <- mutate(wd, water_district = if_else(aquifer_id == 496, "Vernon", water_district))

# Add to aquifers
aquifer_db <- select(wd, water_district, aquifer_id) %>%
  left_join(aquifer_db, ., by = "aquifer_id")

# Master - (GIS) Region  -------------------------------------------
message("  Regions")
aquifer_db <- select(bcmaps::nr_regions(), region = REGION_NAME) %>%
  mutate(region = str_remove(region, " Natural Resource Region"),
         region = str_remove(region, "-Boundary")) %>%
  st_intersection(select(aquifer_map, aquifer_id)) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  group_by(aquifer_id) %>%
  mutate(parea = area / sum(area) * 100,
         n = n()) %>%
  arrange(desc(parea)) %>%
  mutate(region = if_else(n > 1 & all(parea < 95),
                          paste0(region, collapse = " and "),
                          region)) %>%
  filter(parea == max(parea)) %>%
  ungroup() %>%
  st_set_geometry(NULL) %>%
  left_join(aquifer_db, ., by = "aquifer_id")

# Master - Hydraulic Connectivity ----------------------------------------------
# Add Hydraulic Connectivity to Aquifer Data
message("  Hydraulic Connectivity")
aquifer_db <- left_join(aquifer_db, hc, by = "aquifer_subtype_code")

# Master - Licences -------------------------------------------------------
# Prepare licence data
message("  Licences")
licenced_vol <- licenced_vol %>%
  rename_all(tolower) %>%
  select(licence_number, pod_subtype, aquifer_id = source_name) %>%
  filter(pod_subtype %in% c("PWD", "PG")) %>% # Groundwater-only licences
  mutate(aquifer_id = suppressWarnings(as.numeric(aquifer_id))) %>%
  filter(!is.na(aquifer_id)) %>%
  distinct() # Some licences tied to more than one well within an aquifer, but still, just one licence

# Add to aquifer db
aquifer_db <- licenced_vol %>%
  group_by(aquifer_id) %>%
  summarize(n_licences = n(), .groups = "drop") %>%
  left_join(aquifer_db, ., by = "aquifer_id") %>%
  # Fill missing counts with zero
  mutate(n_licences = replace(n_licences, is.na(n_licences), 0))

# Mapping Dates (Available in Aquifers)

# Aquifer Subtype Descriptions
aquifer_db <- aquifer_subtypes %>%
  rename_all(~str_replace_all(tolower(.), " ", "_")) %>%
  select(aquifer_subtype_code, description) %>%
  mutate(description = replace(description,
                               description == "Unkonwn",
                               "Unknown")) %>%
  left_join(aquifer_db, ., by = "aquifer_subtype_code")

# Master - Stress Indices ----------------------------------------------
# message("  Stress Indices")
# aquifer_db <- stress_index %>%
#   select(aquifer_id = AQ_NUM,
#          aquifer_pumping_stress_index = Result) %>%
#   left_join(aquifer_db, ., by = "aquifer_id")

# Data for boxplots ---------------------------------------------------
message("  Boxplot Data")
# Converting Yield from GPM to L/s and Feet to Metres

wells_db <- wells_db %>%
  mutate(well_yield = well_yield * 0.06309,
         well_yield_unit_code = "L/s",
         finished_well_depth_m = finished_well_depth * 0.3048,
         static_water_level_m = static_water_level * 0.3048) %>%
  select(-finished_well_depth, -static_water_level)


# Groundwater data --------------------------------------------------------
message("  Groundwater data")
# Add Aquifer and OW ids to SOE data
ground_water <- select(obs_wells_index, aquifer_id, ow) %>%
  distinct() %>%
  right_join(ground_water, by = c("ow" = "Well_Num")) %>%
  filter(!is.na(aquifer_id),
         !is.na(ow))

ground_water_trends <- select(obs_wells_index, aquifer_id, ow) %>%
  distinct() %>%
  right_join(ground_water_trends, by = c("ow" = "Well_Num"),
             suffix = c("", "_gwtrends")) %>%
  filter(!is.na(aquifer_id),
         !is.na(ow))

# Climate index from weathercan -------------------------------------------
message("  Climate index")
stations_dl(quiet = TRUE)
# Get 10 closest stations within 100km (not all will have data)
locs <- wells_db %>%
  select(aquifer_id, ow, latitude, longitude) %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  # Round lat/lon because some OW are off by tiny amounts between observations
  # and we don't care (stations only has 2 decimal places)
  mutate(latitude = round(latitude, 4),
         longitude = round(longitude, 4)) %>%
  distinct() %>%
  mutate(stations = map2(latitude, longitude,
                         ~stations_search(coords = c(.x, .y), dist = 150,
                                          normals_years = "current") %>%
                           select(station_name, climate_id, lat_climate = lat,
                                  lon_climate = lon, elev_climate = elev,
                                  distance) %>%
                           mutate(climate_id = as.character(climate_id),
                                  n = 1:n()) %>%
                           slice(1:5))) %>%
  unnest(stations)

# Climate Normals - Precipitation Data ----------------------------------------
message("  Climate Normals")
ppt <- locs %>%
  pull(climate_id) %>%
  unique() %>%
  normals_dl() # Download the climate normals for all these stations

# Public data, so all codes must be D or better
# (therefore don't have to filter by code quality)
ppt_good <- unnest(ppt, normals) %>%
  select(climate_id, month_abb = period, rain, snow, code = snow_code) %>%
  filter(month_abb != "Year") %>%
  group_by(climate_id) %>%
  filter(sum(is.na(rain)) == 0, sum(is.na(snow)) == 0) %>%
  nest(normals = c(month_abb, rain, snow)) %>%
  left_join(locs, ., by = "climate_id") %>%
  mutate(data = map_lgl(normals, is.data.frame))

# Get data from station with best quality data within 15km of closest station
ppt_normals <- ppt_good %>%
  group_by(aquifer_id, ow) %>%
  mutate(min_dist = min(distance[data]),
         close = distance <= min_dist + 15) %>%
  arrange(aquifer_id, ow, desc(data), desc(close), code) %>%
  slice(1) %>%
  select(-n, -data, -min_dist, -close)

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
  select(aquifer_id, ow, climate_name = station_name, normals) %>%
  unnest(normals) %>%
  gather(key = "precipitation", value = "ppt_mm", rain, snow)

# Water level data ----------------------------------------------------
message("  Water level")
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
         month_year = paste0(month_text, "-", year)) %>%
  # Check for extreme outliers?
  group_by(ow) %>%
  mutate(iqr = IQR(wl),
         outlier_min = quantile(wl, 0.25) - (iqr * 20),
         outlier_max = quantile(wl, 0.75) + (iqr * 20),
         outlier =  wl <= outlier_min | wl >= outlier_max,
         n_outliers = sum(outlier),
         outlier = outlier & n_outliers == 1,
         outlier_value = if_else(outlier, wl, as.numeric(NA)),
         wl = replace(wl, outlier, as.numeric(NA)))

wl_month_extremes <- wl_all %>%
  group_by(ow, month) %>%
  summarize(min_monthly_wl = min(wl, na.rm = TRUE),
            max_monthly_wl = max(wl, na.rm = TRUE),
            .groups = "drop")

# Calculate median water level for each month/year then for each month
wl_month <- wl_all %>%
  group_by(ow, year, month) %>%
  summarize(mean_monthly_wl = mean(wl, na.rm = TRUE),
            min_monthly_wl = min(wl, na.rm = TRUE),
            max_monthly_wl = max(wl, na.rm = TRUE),
            median_monthly_wl = median(wl, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(ow, month) %>%
  summarize(median_median = median(median_monthly_wl, na.rm = TRUE),
            mean_median = mean(median_monthly_wl, na.rm = TRUE),
            percentile_25 = quantile(median_monthly_wl, 0.25, na.rm = TRUE),
            percentile_75 = quantile(median_monthly_wl, 0.75, na.rm = TRUE),
            percentile_10 = quantile(median_monthly_wl, 0.10, na.rm = TRUE),
            percentile_90 = quantile(median_monthly_wl, 0.90, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(month_abb = month(month, label = TRUE))

# Summarize years to see max, min and number of years of data
wl_summary <- wl_all %>%
  group_by(ow) %>%
  summarize(min_yr = min(year),
            max_yr = max(year),
            num_yrs = max_yr - min_yr,
            .groups = "drop")

wl_month <- wl_month %>%
  left_join(wl_month_extremes, by = c("ow", "month")) %>%
  left_join(wl_summary, by = "ow") %>%
  left_join(distinct(select(obs_wells_index_climate, aquifer_id, ow)), by = "ow")


# Save Data ---------------------------------------------------------------
message("  Saving data")
# Save RDS files
save(aquifer_db,
     wells_db,
     wl_month,
     wl_all,
     ppt,
     ppt_normals,
     ppt_good,
     ground_water,
     ground_water_trends,
     file = "tmp/aquifer_factsheet_clean_data.RData")

# Save .csv files to pull in by aquifer factsheets
write_csv(aquifer_db, "./out/aquifer_table.csv")
write_csv(wells_db, "./out/wells_table.csv")
