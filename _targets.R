# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
library(future.callr)

draft <- TRUE

# Set target options:
tar_option_set(
  packages = c("arrow", "tibble", "readr", "readxl", "fst", "fs",
               "dplyr", "tidyr", "purrr", "stringr", "lubridate", "ggplot2",
               "sf", "assertr", "magick", "scales",  "httr",
               "bcgroundwater", "bcdata", "bcgwcat",
               "weathercan", "smwrBase", "smwrGraphs", "pandoc",
               "lutz"),
  format = "fst",  # Need to use RDS when non-dataframes
  ## For debugging a specific target
  #debug = "pl_gwl_ppt_21bc70738765bf57",
  #cue = tar_cue(mode = "never"),
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
update <- FALSE  # only update URLs if things seem to have changed
u_dl <- aq_urls(update)
u_bc <- aq_urls_bcdata(update)

# Normals to use
# TODO: Update to the next set?
#   - 1991-2020 are expected to be completed summer 2024 https://climate.weather.gc.ca/climate_normals/faq_e.html
normals_yrs <- "1981-2010" # Available: https://climate.weather.gc.ca/climate_normals/index_e.html

# Wells / Aquifers to omit
omit_ow <- 433        # Not in Aquifer 217, but below
omit_ems <- "E290173" # Manually measured well

# Freeze the run (i.e. no updating)
#tar_options_set(cue = tar_cue(mode = "never"))


options(timeout = max(300, getOption("timeout"))) # because some of the downloads are long!

# See run.R for runs and troubleshooting

# Targets -------------------------
list(

  # Specify aquifers
  tar_target(aq_maps, f["inputs_maps"], format = "file"),  # Track file changes
  tar_target(
    aq_ids, {
      fix_names(filename = "Aquifer_Map", ext = "pdf") # Check/fix map names
      as.numeric(str_extract(list.files(aq_maps), "[0-9]{4}"))  #ALL Aquifers
    }, format = "rds"),

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

  tar_age( # Identify stations near the active OWs
    ppt_stations_index,
    id_ppt_stations(ow_index_active),
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
    path = u_dl[["ow"]]$path,
    method = "libcurl"),

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

  tar_download(
    gwl_meta_file,
    urls = u_dl[["gwl_meta"]]$url,
    paths = u_dl[["gwl_meta"]]$path),

  # Extract zip files
  tar_target(gwells_files,
             aq_unzip(gwells_zip, f["outputs_data_dl"], files = c("well.csv")), #"lithology.csv")),
             format = "file"),
  #tar_target(wells_file, gwells_files[["well.csv"]], format = "file"),

  # Load Common Data ---------------------------------------------
  tar_target(aquifer_map,
             aq_read(aquifer_map_file) |> st_set_agr("constant"),
             format = "rds"),

  # Index for Extra Page 3 content
  tar_files(extra_files, fs::dir_ls(f["inputs_extra"])),  # Track file changes
  tar_target(extra_index_file, fmt_extra_page_index(extra_files)), # Create index file

  # Clean data -------------------------------------------
  tar_target(licences,    fmt_licences(licences_file)),
  tar_target(subtypes,    fmt_subtypes(subtypes_file)),
  tar_target(stress,      fmt_stress(stress_file)),

  tar_target(wd,          fmt_water_districts(bcmaps::water_districts(), aquifer_map)),
  tar_target(regions,     fmt_regions(bcmaps::nr_regions(), aquifer_map)),

  tar_target(wells,       fmt_wells(gwells_files, aq_ids, omit_ow)),
  tar_target(ow_index,    fmt_ow_index(wells)),
  tar_target(ow_index_active,    filter(ow_index, ow_status == "Active")),

  tar_target(wl,          fmt_water_levels(ow_file, ow_index_active)),
  tar_target(ems,         fmt_ems(ow_index_active, omit_ems, update = ems_updated)),

  # Only keep ow in both gwls because if none we don't plot anyway
  tar_target(gwl_trends_pre,  fmt_gwl_trends(gwl_trends_file, gwl_meta_file, ow_index_active)),
  tar_target(gwl_monthly,
             fmt_gwl_monthly(gwl_monthly_file, ow_index_active) |>
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

  # Batch work  ---------------------------------------------

  # Get batches of 10 aquifers at a time for more efficient dynamic branching
  # tar_target(aq_batch, {
  #   batches <- ntile(aq_ids, n = floor(length(aq_ids)/10))
  #   map(unique(batches), \(x) aq_ids[batches == x])
  # }, format = "rds"),

  # Create separate well targets to avoid re-running all boxplots if one row changes
  #tar_target(wells_indiv, filter(wells, aquifer_id %in% aq_batch[[1]]), pattern = map(aq_batch)),

  # Create plots --------------------------------------------

  ## Boxpots -----------

  # - Create "No Data" plots
  tar_file(bx_empty, plot_bx_empty()),

  tar_group_by(wells_batch, aq_group(wells), aq_group),

  # - Run per well file (saved by aquifer_id - one plot per aquifer)
  # TODO: combine these into one target
  tar_file(boxplots, {
    map(unique(wells_batch$aquifer_id), \(aq) {
      data <- filter(wells_batch, aquifer_id == aq)
      c(plot_bx_well_yield(data, bx_empty),
        plot_bx_well_depth(data, bx_empty),
        plot_bx_water_depth(data, bx_empty))
    }) |> unlist()
  }, pattern = map(wells_batch)),

  ## Water-level / Precipitation Plots -------------
  # - Group by AQ batch but run for each OW (one plot per observation well)
  tar_group_by(wl_batch, aq_group(wl), aq_group),
  tar_group_by(ppt_batch, aq_group(ppt_normals), aq_group),
  tar_file(pl_gwl_ppt, {
    map(unique(c(wl_batch$ow, ppt_batch$ow)), \(o) {
      plot_gwl_ppt(filter(wl_batch, ow == o),
                   filter(ppt_batch, ow == o))
    }) |> unlist()
  }, pattern = map(wl_batch, ppt_batch), priority = 0),

  ## Ground water levels ----------------
  # - Group by AQ batch but run for each OW (one plot per observation well)
  tar_group_by(gwl_batch, aq_group(gwl_monthly), aq_group),
  tar_group_by(gwl_trends_batch, aq_group(gwl_trends), aq_group),
  tar_file(pl_gwl_trends, {
    map(unique(c(gwl_batch$ow, gwl_trends_batch$ow)), \(o) {
      plot_gwl_trends(filter(gwl_batch, ow == o),
                      filter(gwl_trends_batch, ow == o))
    }) |> unlist()
  }, pattern = map(gwl_batch, gwl_trends_batch), priority = 0),

  ## Piperplots -----------------
  # - Group by AQ batch but run for each StationID (one plot per observation well, called StationID in EMS)
  tar_group_by(ems_batch, aq_group(ems), aq_group),
  tar_file(pl_piperplot, {
    map(unique(ems_batch$StationID), \(o) {
      plot_piper(filter(ems_batch, StationID == o))
    }) |> unlist()
  }, pattern = map(ems_batch), priority = 0),

  # Factsheets -------------------

  # Prep details for factsheets (must have same aquifer_ids / aq_groups)

  tar_group_by(p1, fs_aq_details(aquifers, ow_index), aq_group),
  tar_group_by(figs_p1, fs_figs_p1(p1, boxplots), aq_group),
  tar_group_by(figs_p2,
               fs_figs_p2(p1, pl_gwl_ppt, pl_gwl_trends, pl_piperplot, ow = ow_index),
               aq_group, format = "rds"), # List columns
  tar_group_by(figs_p3,
               fs_figs_p3(p1, extra_index_file, extra_files),
               aq_group, format = "rds"), # List columns


  # Template files
  tar_file(factsheet_templates, dir_ls(f["inputs_templates"])),

  # Batch the aquifer data
  tar_file(factsheets, {
    map(unique(p1$aquifer_id), \(aq) {

      # List all created figures to force invalidation if they change
      boxplots
      pl_gwl_ppt
      pl_gwl_trends
      pl_piperplot

      factsheet(aq = filter(p1, aquifer_id == aq),
                figs_p1 = filter(figs_p1, aquifer_id == aq),
                figs_p2 = filter(figs_p2, aquifer_id == aq),
                figs_p3 = filter(figs_p3, aquifer_id == aq),
                templates = factsheet_templates,
                draft = draft)
    }) |> unlist()
  }, pattern = map(p1, figs_p1, figs_p2, figs_p3),
  deployment = "main" # Don't use parallel, sometimes can get a bit weird on names vs. contents
  ),

  # Problems --------------------
  # TODO: Checks for missing pipertext, why IDs in maps but not elsewhere,
  # Or elsewhere but not in maps, etc.
  # Log all the things that need to be fixed
  tar_file(problems, log_problems(aq_ids, pl_piperplot, figs_p2, extra_index_file, factsheets)),

  # Reports --------------------------------
  tar_file(report, report_stats(p1, figs_p2, figs_p3))
)
