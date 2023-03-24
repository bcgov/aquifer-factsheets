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
      "aquifer_subtypes.xlsx",

      # Groundwater level trends
      # bcdata::bcdc_search("long term groundwater")
      # bcdata::bcdc_tidy_resources("a74f1b97-17f7-499b-84e7-6455e169e425")
      "gwl_trends",
      aq_bcdata_url("a74f1b97-17f7-499b-84e7-6455e169e425",
                    "Groundwater Observation Well Water-Level Trends"),
      "gwl_trends.csv",

      # Groundwater level monthly
      # bcdata::bcdc_search("long term groundwater")
      # bcdata::bcdc_tidy_resources("84c06668-8a1e-4629-90a3-051bba903f22")
      "gwl_monthly",
      aq_bcdata_url("84c06668-8a1e-4629-90a3-051bba903f22",
                    "GWL_monthly.csv"),
      "gwl_monthly.csv"

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


#' Download EMS data bases
#'
#' Checks and updates/downloads EMS databases if `update = TRUE` **or** if the
#' local data base is more than a year out-of-date (includes missing db as
#' well). Returns `TRUE` which passed to `fmt_ems()`'s `update` argument. This
#' ensures that `dl_ems()` runs before `fmt_ems()`.
#'
#' @param update Force an update (targets cannot detect when this is out of date)
#'
#' @examples
dl_ems <- function(update = TRUE) {

  update_2yr <- update ||
    rems::get_cache_date(which = "2yr") < Sys.Date() - lubridate::years(1)

  update_hist <- update ||
    rems::get_cache_date(which = "historic") < Sys.Date() - lubridate::years(1)

  rems::download_historic_data(ask = FALSE, dont_update = !update_hist)
  rems::get_ems_data(ask = FALSE, dont_update = !update_2yr)

  TRUE
}


#' Download precipitation normals
#'
#' Using `wells` data, finds a list of nearby stations. Gets the nearest
#' station within 150km with normals precipitation data and downloads it.
#'
#' @param wells Data frame output by `fmt_wells()`
#' @param normals_yrs Which set of normals should be downloaded?
#' @param update Force an update (targets cannot detect when this is out of date)

dl_ppt_normals <- function(ow_index, normals_yrs = "1981-2010", update = TRUE) {

  # Update stations list
  weathercan::stations_dl(quiet = TRUE)

  # Get 5 closest stations within 150km (not all will have data)
  locs <- ow_index %>%
    select(aquifer_id, ow, latitude, longitude) %>%
    # Round lat/lon because some OW are off by tiny amounts between observations
    # and we don't care (stations only has 2 decimal places)
    mutate(latitude = round(latitude, 4),
           longitude = round(longitude, 4)) %>%
    distinct() %>%
    mutate(
      stations = map2(
        latitude, longitude,
        ~weathercan::stations_search(coords = c(.x, .y), dist = 150,
                                     normals_years = normals_yrs) %>%
          select(station_name, climate_id, lat_climate = lat,
                 lon_climate = lon, elev_climate = elev,
                 distance) %>%
          mutate(climate_id = as.character(climate_id),
                 n = 1:n()) %>%
          slice(1:5))) %>%
    unnest(stations)

  # Download the climate normals for all these stations
  locs %>%
    pull(climate_id) %>%
    unique() %>%
    weathercan::normals_dl() |>
    select(-"frost") |>
    unnest("normals") |>
    left_join(select(locs, "climate_id", "ow", "aquifer_id", "distance"),
              by = "climate_id")
}
