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

# Load Packages and Functions --------------------------------------------------------
source("00_header.R")


# Download Remote Data --------------------------------------------------------

# Obs Well Data
download.file(paste0("http://www.env.gov.bc.ca/wsd/data_searches/obswell/",
                     "map/data/ObservationWellDataAll_DailyMean.csv"),
              destfile = "./data_dl/obs_well_daily_mean.csv")

# GWells Data
# Link from https://apps.nrs.gov.bc.ca/gwells/
download.file("https://s3.ca-central-1.amazonaws.com/gwells-export/export/gwells.zip",
              destfile = "./data_dl/gwells.zip")
unzip("./data_dl/gwells.zip", exdir = "./data_dl/",
      files = c("well.csv", "lithology.csv"), overwrite = TRUE)
file.remove("./data_dl/gwells.zip")


# University of Victoria Stress Tests
url <- bcdc_get_record("17ffdf71-28f3-4a65-bba2-134622b50e8f")$resources[[1]]$url
download.file(url, destfile = "./data_dl/uvic_stress_index.xlsx")

# Licences
#bcdc_get_record("5549cae0-c2b1-4b96-9777-529d9720803c")
bcdc_get_data(record = '5549cae0-c2b1-4b96-9777-529d9720803c',
              resource = 'b0f89bdf-2793-4854-a921-b34fd84bcf03') %>%
  sf::st_set_geometry(NULL) %>%
  write_csv("./data_dl/aquifer_licences.csv")

# Aquifer Subtype Codes
bcdc_get_record("099d69c5-1401-484d-9e19-c121ccb7977c")
bcdc_get_data(record = "099d69c5-1401-484d-9e19-c121ccb7977c",
              resource = "ad2f8db4-b357-42c2-9aa5-a0a987bf33c7") %>%
  write_csv("./data_dl/aquifer_subtypes.csv")


# Aquifer Map
bcdc_get_record("099d69c5-1401-484d-9e19-c121ccb7977c")
m <- bcdc_get_data(record = '099d69c5-1401-484d-9e19-c121ccb7977c',
                   resource = '8f421e3a-ccd3-4fab-8198-53ad6e9e2af2') %>%
  rename(aquifer_id = AQ_TAG) %>%
  mutate(aquifer_id = as.numeric(aquifer_id))
write_rds(m, "./data_dl/aquifer_map.rds")

# Groundwater trends
#bcdc_search("groundwater")
bcdc_get_record("a74f1b97-17f7-499b-84e7-6455e169e425")
bcdc_get_data(record = 'a74f1b97-17f7-499b-84e7-6455e169e425',
              resource = 'a8933793-eadb-4a9c-992c-da4f6ac8ca51') %>%
  write_csv("./data_dl/groundwater_trends.csv")

# Aquifer Data
# # https://catalogue.data.gov.bc.ca/dataset/ground-water-aquifers#edc-pow
# # Custom download: Lat/Lon, CSV, No area
# # "./data/BCGW_7113060B_1570223462950_1792.zip"
# unzip("./data/BCGW_7113060B_1570223462950_1792.zip", exdir = "./data/",
#       files = c("GW_AQUIFERS_CLASSIFICATION_SVW/GW_AQUIFER.csv"), junkpaths = TRUE, overwrite = TRUE)
#

# Aquifer data
# bcdc_get_geodata("ground-water-aquifers") %>%
#   as_tibble() %>%
#   rename_all(tolower) %>%
#   write_csv("./data_dl/aquifers.csv")

# Get Licenced data
# bcdc_get_geodata("water-rights-applications-public") %>%
#   as_tibble() %>%
#   rename_all(tolower) %>%
#   write_csv("./data_dl/licenced_vol.csv")
