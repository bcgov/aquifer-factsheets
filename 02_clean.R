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
# Clean and Prepare Data
##############################

# Setup -------------------------------------------------------------------

# Check for aquifer numbers
if(!exists("aquifers")) {
  stop("Can't run 02_clean.R without first specifying which aquifers to summarize", call. = FALSE)
} else if(length(aquifers) == 0) {
  stop("'aquifers' is empty, specify at least one aquifer to summarize", call. = FALSE)
}

# Load functions, packages and data
source("00_functions.R")
source("00_header.R")
load("tmp/aquifer_factsheet_data.RData")


# Master Aquifer Data -----------------------------------------------------

# Set up a master aquifer data frame with pertinent information we can add to


aquifer_factsheet <- select(aquifer_table,
                           AQUIFER_ID, AQUIFER_SUBTYPE_CODE,
                           AQUIFER_CLASSIFICATION, AQUIFER_MATERIALS,
                           AQUIFER_NAME, VULNERABILITY, PRODUCTIVITY,
                           DESCRIPTIVE_LOCATION, SIZE_KM2) %>%
  # Check for encoded characters (i.e. \x92 for ' etc.)
  mutate_if(is.character, funs(str_replace_all(., "\x92", "'")))


# Set up Wells Database with pertinent information
# OW = Obs well number

# Get link between aquifer number and well number
wells_aq_index <- wells_aquifer_connection %>%
  select(AQUIFER_ID, WELL_ID) %>%
  distinct() # Omit duplicates (~150)

wells_db <- select(wells_database_RAW,
                   WELL_ID, WELL_TAG_NUMBER,
                   BCGS_ID,
                   SEQUENCE_NO,
                   LATITUDE, LONGITUDE,
                   DEPTH_WELL_DRILLED, WATER_DEPTH,
                   AQUIFER_LITHOLOGY_CODE,
                   YIELD_VALUE, YIELD_UNIT_CODE,
                   WELL_USE_CODE,
                   OW = OBSERVATION_WELL_NUMBER,
                   OBSERVATION_WELL_STATUS = MINISTRY_OBSERVATION_WELL_STAT,
                   DEPTH_WELL_DRILLED_2, WATER_DEPTH_2,
                   YIELD_VALUE_2, TOTAL_DEPTH_DRILLED) %>%
  mutate(OW = as.numeric(OW)) %>%
  # creates duplicates because some wells connected to >1 aquifer
  left_join(wells_aq_index, by = "WELL_ID")

obs_wells_index_climate <- obswell_to_aquifer %>%
  select(AQUIFER_ID = `Aquifer No`,
         OW = `Obs well`,
         #WELL_TAG_NUMBER = Well.Tag.No,
         LOCATION = Location,
         CLIMATE_ID = `Climate ID`,
         CLIMATE_NAME = `Nearest climate station`) %>%
  mutate(CLIMATE_ID = as.character(CLIMATE_ID))

obs_wells_index <- wells_db %>%
  filter(!is.na(OW)) %>%
  select(AQUIFER_ID, OW, WELL_ID, OBSERVATION_WELL_STATUS) %>%
  full_join(obs_wells_index_climate, by = c("AQUIFER_ID", "OW")) %>%
  filter(!is.na(OW), !is.na(AQUIFER_ID)) %>%
  distinct() %>%
  arrange(AQUIFER_ID, OW)


# Check for conflicting information ---------------------------------------------

# Get relevant well info
p_wells <- filter(wells_db, AQUIFER_ID %in% aquifers, !is.na(OW)) %>%
  select(AQUIFER_ID, WELL_ID, WELL_OW_STATUS = OBSERVATION_WELL_STATUS, OW) %>%
  mutate(type = "WELL_OW")

# Get relevant climate/well info
p_climate <- filter(obs_wells_index_climate, AQUIFER_ID %in% aquifers) %>%
  select(AQUIFER_ID, OW, CLIMATE_LOCATION = LOCATION, CLIMATE_ID, CLIMATE_NAME) %>%
  mutate(type = "CLIMATE_OW")

# Get duplicate wells (for later decisions)
p_wells_dup <- select(p_wells, WELL_ID, OW) %>%
  distinct() %>%
  group_by(OW) %>%
  mutate(n = n()) %>%
  filter(n > 1)

p_climate_dup <- select(p_climate, OW, CLIMATE_ID, CLIMATE_LOCATION) %>%
  distinct() %>%
  group_by(OW) %>%
  mutate(n = n()) %>%
  filter(n > 1)

# Remove duplicate wells from main lists (just for now)
p_wells <- filter(p_wells, !OW %in% p_wells_dup$OW)
p_climate <- filter(p_climate, !OW %in% p_climate_dup$OW)

problems <- bind_rows(select(p_wells, AQUIFER_ID, OW, type),
                       select(p_climate, AQUIFER_ID, OW, type)) %>%
  arrange(AQUIFER_ID) %>%
  mutate(n = OW) %>%
  spread(type, OW) %>%
  rename(OW = n) %>%
  left_join(select(p_wells, -type), by = c("AQUIFER_ID", "WELL_OW" = "OW")) %>%
  left_join(select(p_climate, -type), by = c("AQUIFER_ID", "CLIMATE_OW" = "OW")) %>%
  select(AQUIFER_ID, OW, WELL_OW, CLIMATE_OW, everything()) %>%
  mutate(problem = case_when(is.na(WELL_OW) ~ "Linking file doesn't match AQUIFER_ID to this WELL_ID",
                             is.na(CLIMATE_OW) ~ "OW not in Climate Index for this AQUIFER_ID",
                             is.na(CLIMATE_ID) ~ "OW in Climate Index, but missing CLIMATE_ID"),
         link_note = "") %>%
  filter(!is.na(problem))

for(i in 1:nrow(problems)) {
  if(!is.na(problems$problem[i]) &
     str_detect(problems$problem[i], "Linking file doesn't match")) {
    l <- filter(wells_aquifer_connection, problems$WELL_ID[i] %in% WELL_ID)
    w <- filter(wells_db, OW %in% problems$OW[i])
    if(nrow(l) == 0){
      if(nrow(w) == 0) {
        problems$link_note[i] <- "OW not in wells database"
      } else if(all(is.na(w$AQUIFER_ID))) {
        problems$link_note[i] <- paste0("WELL_ID (", unique(w$WELL_ID), ") not in linking file")
      } else if(nrow(w) > 0 & all(w$AQUIFER_ID != problems$AQUIFER_ID[i])) {
        problems$link_note[i] <- paste0("WELL_ID (", unique(w$WELL_ID), ") linked to AQUIFER ID(s): ", paste0(w$AQUIFER_ID, collapse = ", "))
      }
    }
  }
}

write.csv(problems, paste0("./out/LOG_PROBLEMS_WIDE_", Sys.Date(), ".csv"),
          row.names = FALSE)

dups <- bind_rows(mutate(p_wells_dup, type = "wells"),
                  mutate(p_climate_dup, type = "climate")) %>%
  select(type, OW, WELL_ID, CLIMATE_ID, CLIMATE_LOCATION)
write.csv(dups, paste0("./out/LOG_PROBLEMS_DUPLICATES_", Sys.Date(), ".csv"),
          row.names = FALSE)


# Master - Number of Wells ---------------------------------------------------

# Numbers reported total and observation

n_wells <- wells_aq_index %>%
  group_by(AQUIFER_ID) %>%
  summarize(REPORTED_NUMBER_OF_WELLS = length(AQUIFER_ID))

# Add to Aquifer factsheet
aquifer_factsheet <- left_join(aquifer_factsheet, n_wells, by = "AQUIFER_ID") %>%
  mutate(REPORTED_NUMBER_OF_WELLS = if_else(is.na(REPORTED_NUMBER_OF_WELLS), 0L,
                                            REPORTED_NUMBER_OF_WELLS))

# No. Obs wells from data (all wells, not just active wells)
n_obswells <- wells_db %>%
  filter(!is.na(OW), !is.na(AQUIFER_ID)) %>%
  select(AQUIFER_ID, OW, OBSERVATION_WELL_STATUS) %>%
  distinct()

# Save index to file
write.csv(n_obswells, "./out/aquifer_ow.csv", row.names = FALSE)

# Clean up
rm(wells_aq_index, n_wells, n_obswells)

# DECISION - wells table has multiple cases where an obs well number is assigned but may not be an obs well?
# eg. obs well no. given for wells classified as UNK, DOM, NA, AND OBS - what should we use?



# Master - Hydraulic Connectivity --------------------------------------------------

# Adding Hydraulic Connectivity to aquifer_factsheet
hydraulic_connectivity <- rename(hydraulic_connectivity,
                                 AQUIFER_SUBTYPE_CODE = Aquifer_Subtype,
                                 HYDRAULIC_CONNECTIVITY = Hydraulic_Connectivity) %>%
  mutate(AQUIFER_SUBTYPE_CODE = as.character(AQUIFER_SUBTYPE_CODE),
         HYDRAULIC_CONNECTIVITY = as.character(HYDRAULIC_CONNECTIVITY)) %>%
  bind_rows(tibble(AQUIFER_SUBTYPE_CODE = "UNK", HYDRAULIC_CONNECTIVITY = "Unknown"))

aquifer_factsheet <- left_join(aquifer_factsheet, hydraulic_connectivity,
                               by = "AQUIFER_SUBTYPE_CODE")

# Master - Location/description -------------------------------------------

# Mapping Dates (Not now - need updated file)
# aquifer_factsheet <- left_join(aquifer_factsheet, aquifer_mapping_date,
#                                by="AQUIFER_ID")

# Location Description and Region
# (Unknown source - manual GIS analysis?)

aquifer_loc_region <- aquifer_loc_region %>%
  rename(REGION = Region) %>%
  mutate(REGION = as.character(REGION))

aquifer_factsheet <- left_join(aquifer_factsheet, aquifer_loc_region,
                               by = "AQUIFER_ID") %>%
  mutate(REGION = if_else(is.na(REGION), "Unknown", REGION))


# Water District
# (Unknown source)

names(water_district) <- c("AQUIFER_ID", "WATER_DISTRICT" )
water_district$WATER_DISTRICT <- as.character(water_district$WATER_DISTRICT)

aquifer_factsheet <- left_join(aquifer_factsheet, water_district,
                               by = "AQUIFER_ID") %>%
  mutate(WATER_DISTRICT = if_else(is.na(WATER_DISTRICT), "Unknown", WATER_DISTRICT))

# Aquifer Subtype Descriptions
aquifer_subtype_descriptions <- select(aquifer_subtype_descriptions,
                                       AQUIFER_SUBTYPE_CODE, DESCRIPTION)

aquifer_factsheet <- left_join(aquifer_factsheet, aquifer_subtype_descriptions,
                               by = "AQUIFER_SUBTYPE_CODE")


# Master - Licences -------------------------------------------------------

# Prepare license data
licences <- licenced_vol %>%
  rename(AQUIFER_ID = AQUIFER_NM) %>%
  # Test if AQUIFER_NM can be converted to a number (we'll keep only those)
  mutate(keep = map(AQUIFER_ID, ~is.numeric(type.convert(as.character(.x))))) %>%
  # Only keep licenses which are numeric
  filter(keep == TRUE) %>%
  mutate(AQUIFER_ID = as.numeric(as.character(AQUIFER_ID))) %>%
  group_by(AQUIFER_ID) %>%
  summarize(NUMBER_OF_LICENCES = n())

# Add to master
aquifer_factsheet <- left_join(aquifer_factsheet, licences, by = "AQUIFER_ID") %>%
  mutate(NUMBER_OF_LICENCES = if_else(is.na(NUMBER_OF_LICENCES), 0L,
                                      NUMBER_OF_LICENCES))


# Master - Stress Indices ----------------------------------------------
aquifer_factsheet <- left_join(aquifer_factsheet,
                               select(stress_index,
                                      AQUIFER_ID = AQ_NUM,
                                      AQUIFER_PUMPING_STRESS_INDEX = Result),
                               by = "AQUIFER_ID")

# Data for boxplots ---------------------------------------------------

# Converting Yield from GPM to L/s and Feet to Metres

wells_db <- wells_db %>%
  mutate(YIELD_VALUE_L_S = YIELD_VALUE * 0.06309,
         WATER_DEPTH_M = WATER_DEPTH * 0.3048,
         DEPTH_WELL_DRILLED_M = DEPTH_WELL_DRILLED * 0.3048,
         YIELD_UNIT_CODE = "L/s") %>%
  select(-YIELD_VALUE, -WATER_DEPTH, -DEPTH_WELL_DRILLED)

# Water level data ----------------------------------------------------

# Read the data and format the dates and date columns
wl_all_data <- wl_data %>%
  rename(Date = QualifiedTime, OW = myLocation, WL = Value) %>%
  filter(OW != "myLocation") %>%
  mutate(Date = as.Date(Date, format= "%Y-%m-%d"),
         OW = as.numeric(substring(OW, 3)),
         WL = as.numeric(WL),
         Year = as.numeric(format(Date, format = "%Y")),
         Month = as.numeric(format(Date, format = "%m")),
         Month_Text = as.character(format(Date, format = "%b")),
         Day = as.numeric(format(Date, format = "%d")),
         Month_year = as.character(format(Date, format = "%b-%Y")))

wl_month_extremes <- wl_all_data %>%
  group_by(OW, Month) %>%
  summarise(min_monthly_wl = min(WL),
            max_monthly_wl = max(WL))

# Calculate median water level for each month/year then for each month
wl_month_data <- wl_all_data %>%
  group_by(OW, Year, Month) %>%
  summarize(mean_monthly_wl = mean(WL),
            min_monthly_wl = min(WL),
            max_monthly_wl = max(WL),
            median_monthly_wl = median(WL)) %>%
  group_by(OW, Month) %>%
  summarise(median_median = median(median_monthly_wl),
            mean_median = mean(median_monthly_wl),
            percentile_25 = quantile(median_monthly_wl, 0.25),
            percentile_75 = quantile(median_monthly_wl, 0.75),
            percentile_10 = quantile(median_monthly_wl, 0.10),
            percentile_90 = quantile(median_monthly_wl, 0.90)) %>%
  ungroup() %>%
  mutate(month_abb = month(Month, label = TRUE))

# Summarize years to see max, min and number of years of data
obs_summary <- wl_all_data %>%
  group_by(OW) %>%
  summarize(min_yr = min(Year),
            max_yr = max(Year),
            num_yrs = max_yr-min_yr)

wl_month_data <- wl_month_data %>%
  left_join(wl_month_extremes, by = c("OW", "Month")) %>%
  left_join(obs_summary, by = "OW") %>%
  left_join(distinct(select(obs_wells_index, AQUIFER_ID, OW)), by = "OW")


# Precipitation Data ------------------------------------------------------

# Read the ppt data and convert rows to columns
ppt_data <- ppt_data %>%
  select(STATION_NAME,
         CLIMATE_ID,
         month = Month,
         ppt_mm = VALUE,
         code = MONTHLY_NORMAL_CODE,
         precipitation = NORMAL_ELEMENT_NAME) %>%
  #remove month 13 (probably totals) and total ppt values
  filter(precipitation != "Total precipitation mm",
         month != 13) %>%
  mutate(month_abb = month(month, label = TRUE))

# Merge with ID values
ppt_data <- left_join(ppt_data, obs_wells_index, by = "CLIMATE_ID")

# Filter data by quality
ppt_data <- ppt_data %>%
  filter(code %in% c("A", "B", "C", "D"))


# Groundwater data --------------------------------------------------------
# Add Aquifer and OW ids to SOE data
ground_water <- select(obs_wells_index, AQUIFER_ID, OW) %>%
  distinct() %>%
  right_join(ground_water, by = c("OW" = "Well_Num")) %>%
  mutate(OW = as.character(OW)) %>%
  filter(!is.na(AQUIFER_ID),
         !is.na(OW))

ground_water_trends <- select(obs_wells_index, AQUIFER_ID, OW) %>%
  distinct() %>%
  right_join(ground_water_trends, by = c("OW" = "Well_Num")) %>%
  mutate(OW = as.character(OW)) %>%
  filter(!is.na(AQUIFER_ID),
         !is.na(OW))


# Save Data ---------------------------------------------------------------

# Save RDS files
save(aquifer_factsheet,
     wells_db,
     wl_month_data,
     wl_all_data,
     ppt_data,
     ground_water,
     ground_water_trends,
     file = "tmp/Aquifer_Dash_clean.RData")

# Save .csv files to pull in by aquifer factsheets
write.csv(aquifer_factsheet, file = "./out/aquifer_table.csv", row.names = FALSE)
write.csv(wells_db, file = "./out/wells_table.csv", row.names = FALSE)
