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
load("tmp/aquifer_factsheet_clean_data.RData")

# Move all old logs to the archives
old <- list.files("./out/", pattern = "LOG", full.names = TRUE)
file.copy(old, "./out/archive")
file.remove(old)


# Master - Number of Wells ---------------------------------------------------

# Which aquifers in our list are missing in GWELLS?
a <- filter(aquifer_db, reported_no_wells == 0)
if(nrow(a) > 0) write_csv(a, "./out/LOG_AQUIFERS_MISSING_IN_GWELLS.csv")


# Master - Licences -------------------------------------------------------
# Which aquifers in our list are missing Licences?
a <- filter(aquifer_db, n_licences == 0)
if(nrow(a) > 0) write_csv(a, "./out/LOG_AQUIFERS_MISSING_IN_LICENCES.csv")


# Climate Normals - Precipitation Data ----------------------------------------
# How many / which OWs do not have any climate normals available?
m <- ppt_normals %>%
  group_by(aquifer_id, ow) %>%
  filter(!data) %>%
  mutate(str = paste0("Aq ", aquifer_id, ", ow ", ow)) %>%
  pull(str) %>%
  paste0(., collapse = "\n")
if(m != "") message("Not all Obs Wells have climate normals: ", m)


# Climate index original
climate_orig <- climate_index_orig %>%
  select(aquifer_id = `Aquifer No`,
         ow = `Obs well`,
         location = Location,
         climate_id_orig = `Climate ID`,
         climate_name_orig = `Nearest climate station`) %>%
  filter(!str_detect(aquifer_id, "(round)|(inactive)")) %>%  # Remove "round 1", etc. from end of file (also NAs)
  mutate(climate_id_orig = as.character(climate_id_orig),
         aquifer_id = as.numeric(aquifer_id))

# Compare to original data
compare <- left_join(climate_orig,
                     ppt_normals %>%
                       select(aquifer_id, ow,
                              climate_name_weathercan = station_name, climate_id_weathercan = climate_id,
                              distance_weathercan = distance, elev_weathercan = elev_climate),
                     by = c("aquifer_id", "ow"))

# How many were unassigned in original?
filter(compare, !is.na(location), is.na(climate_id_orig)) %>% count()
# How many new, that there weren't before?
filter(compare, !is.na(location), is.na(climate_id_orig), !is.na(climate_id_weathercan)) %>% count()
# How many remained unassigned?
filter(compare, !is.na(location), is.na(climate_id_orig), is.na(climate_id_weathercan)) %>% count()

# Remember that some mismatches because the normals data was thrown out for being poor quality
m <- filter(compare,
            climate_id_orig != climate_id_weathercan |
              is.na(climate_id_weathercan),
            !(is.na(climate_id_orig) & is.na(climate_id_weathercan)))

if(nrow(m) > 0) write_csv(m, paste0("./out/LOG_PROBLEMS_CLIMATE_MISMATCH_", Sys.Date(), ".csv"))

# Which had a climate_id in original file, but don't now?
missing <- filter(compare, !is.na(climate_id_orig) & is.na(climate_id_weathercan)) %>%
  select(aquifer_id, ow, climate_id_orig, climate_name_orig)

# Find out why missing. How is the link wrong?
p <- wells_db %>%
  select(aquifer_id, ow) %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  right_join(missing, by = "ow", suffix = c("_wells", "_climate")) %>%
  mutate(problem = case_when(
    aquifer_id_wells == aquifer_id_climate ~
      "No climate station with good data close enough",
    is.na(aquifer_id_wells) ~ "OW in original climate index, but missing from GWELLS",
    aquifer_id_wells != aquifer_id_climate ~
      "OW linked to different aquifer in GWELLS than in original climate index"))

# Check
if(nrow(p) > 0) write_csv(p, paste0("./out/LOG_PROBLEMS_CLIMATE_MISSING_GWELLS_",
                                    Sys.Date(), ".csv"))

# Which really far?
filter(ppt_normals, distance >= 20) %>%
  select(-normals) %>%
  mutate(distance = round(distance, 3)) %>%
  write_csv(paste0("./out/LOG_PROBLEMS_CLIMATE_DISTANCE_", Sys.Date(), ".csv"))

# Check for conflicting information ---------------------------------------------
# This compares the ORIGINAL, hand-created climate index with GWELLS

# Get relevant well info
p_wells <- filter(wells_db, aquifer_id %in% aquifers, !is.na(ow)) %>%
  select(aquifer_id, well_tag_number, ow_status, ow) %>%
  mutate(type = "well_ow")

# Get relevant climate/well info
p_climate <- filter(climate_orig, aquifer_id %in% aquifers) %>%
  select(aquifer_id, ow, climate_id_orig, climate_name_orig) %>%
  mutate(type = "climate_ow")

# Get duplicate wells (for later decisions)
p_wells_dup <- select(p_wells, well_tag_number, ow) %>%
  distinct() %>%
  group_by(ow) %>%
  mutate(n = n()) %>%
  filter(n > 1)

p_climate_dup <- select(p_climate, ow, climate_id_orig) %>%
  distinct() %>%
  group_by(ow) %>%
  mutate(n = n()) %>%
  filter(n > 1)

# Remove duplicate wells from main lists (just for now)
p_wells <- filter(p_wells, !ow %in% c(p_wells_dup$ow, p_climate_dup$ow)) %>%
  select(aquifer_id_wells = aquifer_id, ow)
p_climate <- filter(p_climate, !ow %in% c(p_climate_dup$ow, p_wells_dup$ow)) %>%
  select(aquifer_id_climate = aquifer_id, ow)

p <- full_join(p_wells, p_climate, by = "ow") %>%
  select(ow, aquifer_id_wells, aquifer_id_climate) %>%
  filter(!is.na(aquifer_id_climate),
         aquifer_id_wells != aquifer_id_climate | is.na(aquifer_id_wells))

if(nrow(p) > 0) write_csv(p, paste0("./out/LOG_PROBLEMS_GWELLS_", Sys.Date(), ".csv"))

dups <- bind_rows(mutate(p_wells_dup, database = "wells"),
                  mutate(p_climate_dup, database = "climate")) %>%
  select(database, ow, well_tag_number, climate_id_orig)

if(nrow(dups) > 0) write_csv(dups, paste0("./out/LOG_PROBLEMS_DUPLICATES_", Sys.Date(), ".csv"))


