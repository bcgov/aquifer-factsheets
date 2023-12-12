# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
library(future.callr)

# Set target options:
tar_option_set(
  packages = c("tibble", "readr", "readxl", "fst", "fs",
               "dplyr", "tidyr", "purrr", "stringr", "lubridate", "ggplot2",
               "sf", "assertr", "magick", "scales",  "httr",
               "bcgroundwater", "bcdata", "rems2aquachem",
               "weathercan", "smwrBase", "smwrGraphs"),
  format = "fst"  # Need to use RDS when non-dataframes
)

# tar_make_future() configuration (okay to leave alone):
plan(callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Details ----------------

# When to trigger older downloads to update
# Targets can't detect changes in some data sources, so specify when to update manually

update_time <- as.difftime(2, unit = "weeks") # Will update every 2 weeks

# First URLs from where to download (because tar_download needs urls declared at the start)
update <- FALSE  # only update if things seem to have changed
u_dl <- aq_urls(update)
u_bc <- aq_urls_bcdata(update)

# Normals to use
# TODO: Update to the next set?
normals_yrs <- "1981-2010" # Available: https://climate.weather.gc.ca/climate_normals/index_e.html

# Wells / Aquifers to omit
omit_ow <- 433        # Not in Aquifer 217, but below
omit_ems <- "E290173" # Manually measured well

# Aquifer ids subset
sub_aq <- c()

# Freeze the run (i.e. no updating)
#tar_options_set(cue = tar_cue(mode = "never"))


# See run.R for runs and troubleshooting


# Targets -------------------------
list(

  # Specify aquifers
  tar_target(
    aq_ids, {
      fix_names(filename = "Aquifer_Map", ext = "pdf") # Check/fix map names
      as.numeric(str_extract(list.files(f("maps")), "[0-9]{4}"))  #ALL Aquifers
    }, format = "rds"),

  # Subset for targeted factsheet re-runs
  tar_target(aq_ids_sub,
             if(length(sub_aq) > 0) aq_ids[aq_ids %in% sub_aq] else aq_ids),

  # Downloads - Alternate methods -------------------------------------
  # ONLY UPDATE if older than `update_time`

  tar_age( # File served via API
    aquifers_file,
    aq_dl(u_dl[["aquifers"]]),
    age = update_time,
    format = "file"),

  tar_age( # Downloaded via bcdata
    licences_file,
    aq_dl(u_bc[["licences"]], remove_sf = TRUE),
    age = update_time,
    format = "file"),

  tar_age(# Downloaded via bcdata
    aquifer_map_file,
    aq_dl(u_bc[["aquifer_map"]]),
    age = update_time,
    format = "file"),

  tar_age(# Update the weathercan::stations() list every `update_time`
    ppt_stations_updated,
    dl_ppt_stations(),
    priority = 1, # So updated before the rest run
    age = update_time),

  tar_age( # IDentify stations near the active OWs
    ppt_stations_index,
    id_ppt_stations(filter(ow_index, ow_status == "Active")),
    age = update_time),

  tar_target( # Get only unique climate_ids
    ppt_climate_ids,
    unique(ppt_stations_index$climate_id)),

  tar_age( # Download climate normals for stations IDed above
    ppt_normals_raw,
    dl_ppt_normals(ppt_climate_ids, normals_yrs),
    pattern = map(ppt_climate_ids),
    age = update_time),

  tar_age(
    ems_updated,
    dl_ems(),
    age = update_time),

  # Downloads - Direct from url -------------------
  tar_download(
    ow_file,
    urls = u_dl[["ow"]]$url,
    path = u_dl[["ow"]]$path),

  tar_download(
    gwells_zip,
    urls = u_dl[["gwells"]]$url,
    paths = u_dl[["gwells"]]$path),

  tar_download(
    stress_file,
    urls = u_dl[["stress"]]$url,
    paths = u_dl[["stress"]]$path),

  tar_download(
    subtypes_file,
    urls = u_dl[["subtypes"]]$url,
    paths = u_dl[["subtypes"]]$path),

  tar_download(
    gwl_trends_file,
    urls = u_dl[["gwl_trends"]]$url,
    paths = u_dl[["gwl_trends"]]$path),

  tar_download(
    gwl_monthly_file,
    urls = u_dl[["gwl_monthly"]]$url,
    paths = u_dl[["gwl_monthly"]]$path),

  # Extract zip files
  tar_target(gwells_files,
             aq_unzip(gwells_zip, f("out_data"), files = c("well.csv")), #"lithology.csv")),
             format = "file"),
  #tar_target(wells_file, gwells_files[["well.csv"]], format = "file"),

  # Load Common Data ---------------------------------------------
  tar_target(aquifer_map,
             aq_read(aquifer_map_file) |> st_set_agr("constant"),
             format = "rds"),

  # Clean data -------------------------------------------
  tar_target(licences,    fmt_licences(licences_file)),
  tar_target(subtypes,    fmt_subtypes(subtypes_file)),
  tar_target(stress,      fmt_stress(stress_file)),

  tar_target(wd,          fmt_water_districts(bcmaps::water_districts(), aquifer_map)),
  tar_target(regions,     fmt_regions(bcmaps::nr_regions(), aquifer_map)),

  tar_target(wells,       fmt_wells(gwells_files, aq_ids, omit_ow), cue = tar_cue(mode = "never")),
  tar_target(ow_index,    fmt_ow_index(wells)),

  tar_target(wl,          fmt_water_levels(ow_file, ow_index)),
  tar_target(ems,         fmt_ems(ow_index, omit_ems, update = ems_updated)),

  # Only keep ow in both gwls because if none we don't plot anyway
  tar_target(gwl_trends_pre,  fmt_gwl(gwl_trends_file, ow_index)),
  tar_target(gwl_monthly,
             fmt_gwl(gwl_monthly_file, ow_index) |>
               semi_join(gwl_trends_pre, by = "ow")),
  tar_target(gwl_trends, semi_join(gwl_trends_pre, gwl_monthly, by = "ow")),

  # Only keep OW also in wl, but ensure all OW in wl are in ppt_normals
  tar_target(ppt_normals,
             fmt_ppt_normals(ppt_normals_raw, ppt_stations_index) |>
               semi_join(wl, by = "ow") |>
               complete(ow = unique(wl$ow))),

  tar_target(
    aquifers,
    fmt_aquifers(aquifers_file, aq_ids, wells, regions, licences, subtypes, stress, wd)),

  tar_target(aq_ids_final, aquifers[["aquifer_id"]], format = "rds"),

  # Create plots --------------------------------------------

  ## Boxpots -----------

  # - Create "No Data" plots
  tar_target(bx_empty, plot_bx_empty(), format = "file"),

  # - Group by aquifer_id (one plot per aquifer)
  tar_group_by(wells_by_aquifer, wells, aquifer_id),
  tar_target(bx_well_yield,
             plot_bx_well_yield(wells_by_aquifer, bx_empty),
             pattern = map(wells_by_aquifer), format = "file"),
  tar_target(bx_well_depth,
             plot_bx_well_depth(wells_by_aquifer, bx_empty),
             pattern = map(wells_by_aquifer), format = "file"),
  tar_target(bx_water_depth,
             plot_bx_water_depth(wells_by_aquifer, bx_empty),
             pattern = map(wells_by_aquifer), format = "file"),

  ## Water-level / Precipitation Plots -------------
  # - Group by OW (one plot per observation well)
  tar_group_by(wl_by_ow, wl, ow),
  tar_group_by(ppt_by_ow, ppt_normals, ow),
  tar_target(pl_wl_ppt,
             plot_wl_ppt(wl_by_ow, ppt_by_ow),
             pattern = map(wl_by_ow, ppt_by_ow),
             format = "file", priority = 0),

  # Ground water levels ----------------
  # - Group by ow (one plot per observation well)
  tar_group_by(gwl_by_ow, gwl_monthly, ow),
  tar_group_by(gwl_trends_by_ow, gwl_trends, ow),
  tar_target(pl_gwl,
             plot_gwl(gwl_by_ow, gwl_trends_by_ow),
             pattern = map(gwl_by_ow, gwl_trends_by_ow),
             format = "file", priority = 0),

  # Piperplots -----------------
  # - Group by StationID (one plot per observation well, called StationID in EMS)
  tar_group_by(ems_by_ow, ems, StationID),
  tar_target(pl_piper,
             plot_piper(ems_by_ow),
             pattern = map(ems_by_ow),
             format = "file", priority = 0),

  # Lists of plots ---------------------
  # Files for Extra
  tar_file(extra_images_file, f("extra", f = "extra_page_images.csv")),
  tar_file(extra_index_file, f("extra", f = "extra_page_index.xlsx")),

  # Plots
  tar_target(figs_p1, fs_figs_p1(aq_ids_final, bx_water_depth, bx_well_depth, bx_well_yield)),
  tar_target(figs_p2, fs_figs_p2(aq_ids_final, pl_wl_ppt, pl_gwl, pl_piper)),
  tar_target(figs_p3, fs_figs_p3(aq_ids_final, extra_images_file, extra_index_file),
             priority = 1),

  # TODO: Checks for missing pipertext, why IDs in maps but not elsewhere,
  # Or elsewhere but not in maps, etc.

  # Factsheets -------------------

  # Prep aquifer and obs well details overall (must have same aquifer_ids)
  tar_target(fs_aquifers, fs_aq_details(aquifers, ow_index)),
  tar_target(fs_ow, fs_ow_details(ow_index) |> complete(aquifer_id = aq_ids_final)),

  # - Group by aquifer id (one factsheet per aquifer)
  tar_group_by(aq_by_aquifer, fs_aquifers, aquifer_id),
  tar_group_by(ow_by_aquifer, fs_ow, aquifer_id),
  tar_group_by(figs_p1_by_aquifer, figs_p1, aquifer_id),
  tar_group_by(figs_p2_by_aquifer, figs_p2, aquifer_id),
  tar_group_by(figs_p3_by_aquifer, figs_p3, aquifer_id),

  tar_target(
    factsheets,
    factsheet(aq_by_aquifer, ow_by_aquifer,
              figs_p1_by_aquifer, figs_p2_by_aquifer, figs_p3_by_aquifer),
    pattern = map(aq_by_aquifer, ow_by_aquifer, figs_p1_by_aquifer, figs_p2_by_aquifer, figs_p3_by_aquifer))

)
