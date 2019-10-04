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
url <- bcdc_get_record("university-of-victoria-aquifer-stress-evaluation")$resources[[1]]$url
download.file(url, destfile = "./data_dl/uvic_stress_index.xlsx")

# Aquifer Data
# https://catalogue.data.gov.bc.ca/dataset/ground-water-aquifers#edc-pow
# Custom download: Lat/Lon, CSV, No area
# "./data/BCGW_7113060B_1570223462950_1792.zip"
unzip("./data/BCGW_7113060B_1570223462950_1792.zip", exdir = "./data/",
      files = c("GW_AQUIFERS_CLASSIFICATION_SVW/GW_AQUIFER.csv"), junkpaths = TRUE, overwrite = TRUE)

# Aquifer Subtype Codes
subtype <- b %>%
  filter(str_detect(name, "subtype code")) %>%
  pull(url)
download.file(subtype, destfile = paste0("./data_dl/aquifer_subtypes",
                                         str_extract(subtype, ".[a-z]*$")))

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
