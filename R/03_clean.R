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

fmt_aquifers <- function(aquifers_file, aq_ids, wells,
                         regions, licences, subtypes, stress) {

  a <- aquifers_file |>
    aq_read() %>%
    filter(aquifer_id %in% aq_ids) |>
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

  # Add Wells
  # - Calculate and add hydraulic properties wells to Aquifer data
  # - Calculate and add reported number of wells to Aquifer data
  a <- wells %>%
    group_by(aquifer_id) %>%
    summarize(across(
      .cols = c("conductivity", "transmissivity", "storativity"),
      .fns = list(min = min_na, max = max_na, n = ~sum(!is.na(.x)))),
      reported_no_wells = n(), .groups = "drop") %>%
    left_join(a, ., by = "aquifer_id") %>%
    # Fill NAs with zeros
    mutate(reported_no_wells = replace_na(reported_no_wells, 0))

  # Add Regions
   a <- left_join(a, regions, by = "aquifer_id")

  # Add Hydraulic Connectivity
  a <- left_join(a, aq_hc(), by = "aquifer_subtype_code")

  # Add licences
  a <- left_join(a, licences, by = "aquifer_id") |>
    mutate(n_licences = replace_na(n_licences, 0))

  # Mapping Dates (Available in Aquifers)
  #
  # Add Subtype Descriptions
   a <- left_join(a, subtypes, by = "aquifer_subtype_code")

  # Add Stress tests
   a <- left_join(a, stress, by = "aquifer_id")

  a
}

# Set up Wells Database with pertinent information
# - Get max number of digits after decimal:
#   str_remove(wells_db_raw$longitude, "^[-0-9]+.") %>% nchar() %>% unique()
# - Use this to round (gets rid of weird differences in numbers
fmt_wells <- function(wells_file, aq_ids, omit_ow) {
  wells_file |>
    aq_read() |>
    filter(aquifer_id %in% aq_ids) |>
    select("aquifer_id", "well_tag_number",
           "well_yield" = "well_yield_usgpm",
           "well_yield_unit_code",
           "ow" = "observation_well_number",
           "ow_status" = "obs_well_status_code",
           "water_depth" = "static_water_level_ft_btoc",
           "well_depth" = "finished_well_depth_ft_bgl",
           "latitude" = "latitude_decdeg",
           "longitude" = "longitude_decdeg",
           "licenced_status_code",
           "conductivity" = "hydraulic_conductivity_m_s",
           "transmissivity" = "transmissivity_m_2_s",
           "storativity",
           "ems") %>%
    mutate(ow = str_remove_all(ow, "OW"),
           ow = as.numeric(ow)) |>

    # Data for boxplots
    # - Converting Yield from GPM to L/s and Feet to Metres
    mutate(well_yield = well_yield * 0.06309,
           well_yield_unit_code = "L/s",
           well_depth = well_depth * 0.3048,
           water_depth = water_depth * 0.3048) |>
    group_by(aquifer_id) |>
    mutate(n_well_yield = sum(!is.na(well_yield) & well_yield != 0),
           n_well_depth = sum(!is.na(well_depth)),
           n_water_depth = sum(!is.na(water_depth))) |>
    ungroup() |>

    # Omit wells
    # - incorrectly placed in aquifer (etc.)
    # - but keep wells missing observation well numbers (i.e. keep NAs in ow)
    filter(is.na(ow) | !ow %in% omit_ow)

}

fmt_ow_index <- function(wells) {
  wells %>%
    filter(!is.na(ow), !is.na(aquifer_id)) %>%
    select(aquifer_id, ow, well_tag_number, ow_status, ems_id = ems,
           latitude, longitude) %>%
    distinct() %>%
    arrange(aquifer_id, ow)
}

# Fetch water districts
# - Overlay aquifer maps on water_district map
fmt_water_districts <- function(water_district_map, aquifer_map) {
  # TODO: Check if using correct map (resolution okay?)
  wd <- water_district_map |>
    st_set_agr("constant") |>
    select(water_district = DISTRICT_NAME) |>
    st_intersection(select(aquifer_map, aquifer_id)) %>%
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
    select(aquifer_id, water_district) %>%
    # Remove spatialness
    st_set_geometry(NULL)

  # TODO: Temporary fix for #496
  wd <- mutate(wd, water_district = if_else(aquifer_id == 496, "Vernon", water_district))

  wd
}

fmt_regions <- function(regions_map, aquifer_map) {
    regions_map |>
    st_set_agr("constant") |>
    select(region = REGION_NAME) |>
    mutate(region = str_remove(region, " Natural Resource Region"),
           region = str_remove(region, "-Boundary")) |>
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
    st_set_geometry(NULL)
}

fmt_licences <- function(licences_file) {
  licences_file |>
    aq_read() |>
    rename_all(tolower) |>
    select(licence_number, pod_subtype, aquifer_id = source_name) |>
    filter(pod_subtype %in% c("PWD", "PG")) |> # Groundwater-only licences
    mutate(aquifer_id = suppressWarnings(as.numeric(aquifer_id))) |>
    filter(!is.na(aquifer_id)) |>
    distinct() |> # Some licences tied to more than one well within an aquifer, but still, just one licence
    group_by(aquifer_id) |>
    summarize(n_licences = n(), .groups = "drop")
}


fmt_subtypes <- function(subtypes_file) {
  subtypes_file |>
    aq_read() |>
    mutate(
      description = replace(description, description == "Unkonwn", "Unknown"))
}

fmt_stress <- function(stress_file) {
  stress_file |>
    aq_read(sheet = "R0. Results") |>
    select(aquifer_id = aq_num,
           aquifer_pumping_stress_index = result)
}

fmt_gwl <- function(gwl, ow_index) {
  gwl |>
    aq_read(clean = FALSE) |>
    rename("ow" = "Well_Num") |>
    inner_join(ow_index, by = "ow", suffix = c("_gwl", ""))
}


fmt_ppt_normals <- function(ppt_normals_raw) {

  # Public data, so all codes must be D or better
  # (therefore don't have to filter by code quality)
  ppt_good <- ppt_normals_raw |>
    select(aquifer_id, ow, distance,
           prov, station_name, climate_id, normals_years,
           month_abb = period, rain, snow, code = snow_code) %>%
    filter(month_abb != "Year") %>%
    group_by(climate_id) %>%
    filter(sum(is.na(rain)) == 0, sum(is.na(snow)) == 0) %>%
    nest(normals = c(month_abb, rain, snow)) %>%
    #left_join(ppt_normals_raw, ., by = "climate_id") %>%
    mutate(data = map_lgl(normals, is.data.frame)) |>
    ungroup()

  # Get data from station with best quality data within 15km of closest station
  ppt_normals <- ppt_good %>%
    group_by(aquifer_id, ow) %>%
    mutate(min_dist = min(distance[data]),
           close = distance <= min_dist + 15) %>%
    arrange(aquifer_id, ow, desc(data), desc(close), code) %>%
    slice(1) %>%
    select(-data, -min_dist, -close) |>
    ungroup()

  # # Use stations as climate index
  # TODO: is this needed for plot names?

  # obs_wells_index_climate <- ppt_normals %>%
  #   select(aquifer_id, ow, climate_id, climate_name = station_name) %>%
  #   mutate(climate_name = tools::toTitleCase(tolower(climate_name)),
  #          climate_name = str_replace_all(climate_name,
  #                                         c(" a$" = " Airport",
  #                                           "Int'l" = "International")),
  #          climate_name = str_replace(climate_name,
  #                                     " (Cs|Cda|Rcs|Awos|Se)(?= |$)",
  #                                     toupper))

  # Finalize ppt normals
  ppt_normals %>%
    select(aquifer_id, ow, climate_name = station_name, normals) %>%
    unnest("normals") %>%
    pivot_longer(cols = c("rain", "snow"),
                 names_to = "precipitation", values_to = "ppt_mm")
}

fmt_water_levels <- function(ow_file, ow_index) {
  wl_all <- ow_file |>
    aq_read() |>
    rename(date = qualified_time, ow = my_location, wl = value) %>%
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

  # TODO: Require obs_wells_index_climate for names?

  wl_month %>%
    left_join(wl_month_extremes, by = c("ow", "month")) %>%
    left_join(wl_summary, by = "ow")# %>%
    #left_join(ow_index, by = "ow")
}

fmt_ems <- function(ow_index, omit_ems, update = TRUE) {
  # Get ids
  ids <- ow_index |>
    filter(ow_status != "Inactive", !ems_id %in% omit_ems) |>
    select(aquifer_id, ow, ems_id)

  # Get data formatted for piperplots
  rems_to_aquachem(ids$ems_id, interactive = FALSE, save = FALSE) %>%
    units_remove() %>%
    select(-StationID) %>%
    mutate(ems_id = str_extract(SampleID, "^[[:alnum:]]+")) %>%
    left_join(ids, by = "ems_id") %>%
    rename(StationID = ow)
}
