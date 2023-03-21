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

# Download Remote Data --------------------------------------------------------

aq_urls <- function(update = TRUE, dir = "data_dl") {
  p <- fs::path(dir, "u_dl.rds")

  if(update || !file.exists(p)) {
    u <- tibble::tribble(
      ~name, ~url, ~file,
      "aquifers",
      "https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv",
      "aquifers.csv",

      "gwells",
      "https://s3.ca-central-1.amazonaws.com/gwells-export/export/v2/gwells.zip",
      "gwells.zip",

      "ow",
      "http://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/ObservationWellDataAll_DailyMean.csv",
      "obs_well_daily_mean.csv",

      # University of Victoria Stress Tests
      # bcdata::bcdc_tidy_resources("17ffdf71-28f3-4a65-bba2-134622b50e8f")
      "stress",
      aq_bcdata_url("17ffdf71-28f3-4a65-bba2-134622b50e8f", "Results_gwF_20180613"),
      "uvic_stress_index.xlsx",

      # Aquifer Subtype Codes
      # bcdata::bcdc_tidy_resources("099d69c5-1401-484d-9e19-c121ccb7977c")
      "subtypes",
      aq_bcdata_url("099d69c5-1401-484d-9e19-c121ccb7977c",
                    "Aquifer subtype code descriptions"),
      "aquifer_subtypes.csv",

      # Groundwater trends
      # bcdata::bcdc_search("long term groundwater")
      # bcdata::bcdc_tidy_resources("a74f1b97-17f7-499b-84e7-6455e169e425")
      "gw",
      aq_bcdata_url("a74f1b97-17f7-499b-84e7-6455e169e425",
                    "Groundwater Observation Well Water-Level Trends"),
      "groundwater_trends.csv"
    ) |>
      dplyr::mutate(path = fs::path(.env$dir, .data$file))
    u <- split(u, u$name)
    readr::write_rds(u, p)
  } else {
    u <- readr::read_rds(p)
  }
  u
}

aq_urls_bcdata <- function(update = TRUE, dir = "data_dl") {
  p <- fs::path(dir, "u_bc.rds")

  if(update || !file.exists(p)) {
    u <- tibble::tribble(
      ~name, ~record, ~resource, ~file,

      # Licences
      # bcdata::bcdc_tidy_resources('5549cae0-c2b1-4b96-9777-529d9720803c')
      "licences",
      "5549cae0-c2b1-4b96-9777-529d9720803c",
      "b0f89bdf-2793-4854-a921-b34fd84bcf03",
      "licences.fst",

      # Aquifer Map
      #bcdata::bcdc_tidy_resources("099d69c5-1401-484d-9e19-c121ccb7977c")
      "aquifer_map",
      "099d69c5-1401-484d-9e19-c121ccb7977c",
      "8f421e3a-ccd3-4fab-8198-53ad6e9e2af2",
      "aquifer_map.rds"
    ) |>
      dplyr::mutate(path = fs::path(.env$dir, .data$file))
    u <- split(u, u$name)
    readr::write_rds(u, p)
  } else {
    u <- readr::read_rds(p)
  }
  u
}



  #
  # # EMS data
  # rems::download_historic_data(ask = FALSE)
  # rems::get_ems_data(which = "2yr", ask = FALSE, check_only = TRUE)
