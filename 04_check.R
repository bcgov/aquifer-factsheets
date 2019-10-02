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
load("tmp/aquifer_factsheet_clean_data.RData")



# Master - Number of Wells ---------------------------------------------------

# Which aquifers in our list are missing in GWELLS?
write_csv(filter(aquifer_db, reported_no_wells == 0),
          "./out/LOG_AQUIFERS_MISSING_IN_GWELLS.csv")


# Master - Licences -------------------------------------------------------
# Which aquifers in our list are missing Licences?
write_csv(filter(aquifer_db, n_licences == 0),
          "./out/LOG_AQUIFERS_MISSING_IN_LICENCES.csv")


# Climate Normals - Precipitation Data ----------------------------------------
# How many / which OWs do not have any climate normals available?
ppt_normals %>%
  group_by(aquifer_id, ow) %>%
  filter(!data)

# Compare to original data
compare <- climate_index_orig %>%
  select(aquifer_id, ow, location,
         climate_id_orig = climate_id, climate_name_orig = climate_name) %>%
  left_join(ppt_normals %>%
              select(aquifer_id, ow,
                     climate_name_weathercan = station_name, climate_id_weathercan = climate_id,
                     distance_weathercan = distance, elev_weathercan = elev_climate),
            by = c("aquifer_id", "ow"))

# How many were unassigned in original?
filter(compare, !is.na(location), is.na(climate_id_orig)) %>% count()
# How many new, that there weren't before?
filter(compare, !is.na(location), is.na(climate_id_orig), !is.na(climate_id_weathercan))
# How many remained unassigned?
filter(compare, !is.na(location), is.na(climate_id_orig), is.na(climate_id_weathercan))

# Remember that some mismatches because the normals data was thrown out for being poor quality
compare %>%
  filter(climate_id_orig != climate_id_weathercan,
         !is.na(climate_id_weathercan), !is.na(climate_id_orig)) %>%
  write_csv(paste0("./out/LOG_PROBLEMS_CLIMATE_MISMATCH_", Sys.Date(), ".csv"))

# Which had a climate_id in original file, but don't now?
missing <- filter(compare, !is.na(climate_id_orig) & is.na(climate_id_weathercan)) %>%
  select(aquifer_id, ow)

# How is the link wrong?
p <- wells_db %>%
  select(aquifer_id, ow) %>%
  filter(!is.na(ow), !is.na(aquifer_id)) %>%
  full_join(missing, by = "ow", suffix = c("_wells", "_climate")) %>%
  filter(!(!is.na(aquifer_id_wells) & is.na(aquifer_id_climate))) %>%
  mutate(problem = case_when(
    aquifer_id_wells == aquifer_id_climate ~
      "No climate station with good data close enough",
    is.na(aquifer_id_wells) ~ "OW in original climate index, but missing from GWELLS",
    aquifer_id_wells != aquifer_id_climate ~
      "OW linked to different aquifer in GWELLS than in original climate index"))

# Check
write_csv(p, paste0("./out/LOG_PROBLEMS_CLIMATE_MISSING_GWELLS_",
                    Sys.Date(), ".csv"))

# Which really far?
filter(ppt_normals, distance >= 20) %>%
  select(-n, -normals) %>%
  mutate(distance = round(distance, 3)) %>%
  write_csv(paste0("./out/LOG_PROBLEMS_CLIMATE_DISTANCE_", Sys.Date(), ".csv"))

# Check for conflicting information ---------------------------------------------
# This compares the ORIGINAL, hand-created climate index with GWELLS

# Get relevant well info
p_wells <- filter(wells_db, aquifer_id %in% aquifers, !is.na(ow)) %>%
  select(aquifer_id, well_tag_number, ow_status, ow) %>%
  mutate(type = "well_ow")

# Get relevant climate/well info
p_climate <- filter(climate_index_orig, aquifer_id %in% aquifers) %>%
  select(aquifer_id, ow, climate_id, climate_name) %>%
  mutate(type = "climate_ow")

# Get duplicate wells (for later decisions)
p_wells_dup <- select(p_wells, well_tag_number, ow) %>%
  distinct() %>%
  group_by(ow) %>%
  mutate(n = n()) %>%
  filter(n > 1)

p_climate_dup <- select(p_climate, ow, climate_id) %>%
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

write_csv(p, paste0("./out/LOG_PROBLEMS_GWELLS_", Sys.Date(), ".csv"))

# problems <- bind_rows(select(p_wells, aquifer_id, ow, type),
#                       select(p_climate, aquifer_id, ow, type)) %>%
#   arrange(aquifer_id) %>%
#   mutate(n = aquifer_id) %>%
#   spread(type, aquifer_id) %>%
#   rename(aquifer_id = n) %>%
#   arrange(ow) %>%
#   filter(climate_ow != well_ow | is.na(climate_ow) | is.na(well_ow))
#   left_join(select(p_wells, -type), by = c("aquifer_id", "well_ow" = "ow")) %>%
#   left_join(select(p_climate, -type), by = c("aquifer_id", "climate_ow" = "ow")) %>%
#   select(aquifer_id, ow, well_ow, climate_ow, everything()) %>%
#   mutate(problem = case_when(is.na(well_ow) ~ paste0("Climate Index matches Aquifer ", aquifer_id, " to Obs well ", climate_ow, " but GWELLS does not"),
#                              is.na(climate_ow) & is.na(aquifer_id) ~ paste0("GWELLS missing aquifer id for Obs Well ", ow),
#                              is.na(climate_ow) ~ paste0("GWELLS matches Aquifer ", aquifer_id, " to Obs Well ", ow, " but Climate Index does not"),
#                              is.na(climate_id) ~ paste0("Obs Well ", ow, " is in Climate Index, but missing 'climate_id'")),
#          link_note = "") %>%
#   filter(!is.na(problem))
#
# # Now the only problems are that We don't have aquifer_ids for inactive Obs Wells
#
# write.csv(problems, paste0("./out/LOG_PROBLEMS_WIDE_", Sys.Date(), ".csv"),
#           row.names = FALSE)

dups <- bind_rows(mutate(p_wells_dup, database = "wells"),
                  mutate(p_climate_dup, database = "climate")) %>%
  select(database, ow, well_tag_number, climate_id)

write_csv(dups, paste0("./out/LOG_PROBLEMS_DUPLICATES_", Sys.Date(), ".csv"))


