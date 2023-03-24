# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

#   library(httr)
#   library(knitr)
#   library(rmarkdown)
#   library(kableExtra)

# Set target options:
tar_option_set(
  packages = c("tibble", "readr", "readxl", "fst", "fs",
               "dplyr", "tidyr", "purrr", "stringr", "lubridate", "ggplot2",
               "sf", "assertr", "magick", "scales",  "httr",
               "bcgroundwater", "bcdata", "rems2aquachem",
               "weathercan", "smwrBase", "smwrGraphs"),
  format = "fst"
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# source("other_functions.R") # Source other scripts as needed. # nolint

prep_dir()

update <- FALSE

# Urls -------------------------------------
u_dl <- aq_urls(update)
u_bc <- aq_urls_bcdata(update)


# Troubleshooting
# tar_make(callr_function = NULL)

# Targets -------------------------
list(

  # Specify aquifers
  tar_target(
    aq_ids, {
      fix_names(type = "maps", filename = "Aquifer_Map", ext = "pdf") # Check/fix map names
      as.numeric(str_extract(list.files("./figures/maps/"), "[0-9]{4}"))  #ALL Aquifers
    }, format = "rds"),

  # Subset for targeted factsheet re-runs
  tar_target(aq_ids_sub,
             if(length(sub_aq) > 0) aq_ids[aq_ids %in% sub_aq] else aq_ids),

  # Downloads - Alternate methods -------------------------------------
  # ONLY UPDATE if `update <- TRUE`
  tar_target( # File served via API
    aquifers_file,
    aq_dl(u_dl[["aquifers"]], update = update),
    format = "file"),

  tar_target( # Downloaded via bcdata
    licences_file,
    aq_dl(u_bc[["licences"]], remove_sf = TRUE, update = update),
    format = "file"),

  tar_target(# Downloaded via bcdata
    aquifer_map_file,
    aq_dl(u_bc[["aquifer_map"]], update = update),
    format = "file"),

  tar_target(ppt_normals_raw, dl_ppt_normals(ow_index, normals_yrs, update)),
  tar_target(ems_updated, dl_ems(update)),

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
             aq_unzip(gwells_zip, "data_dl", files = c("well.csv", "lithology.csv")),
             format = "rds"),
  tar_target(gwells_wells, gwells_files[["well.csv"]], format = "file"),
  tar_target(gwells_lith, gwells_files[["lithology.csv"]], format = "file"),

  # Load data ---------------------------------------------
  tar_target(aquifers_raw, aq_read(aquifers_file)),
  tar_target(ow, aq_read(ow_file)),
  tar_target(wells_raw, aq_read(gwells_wells)),
  tar_target(wells_lith, aq_read(gwells_lith)),
  tar_target(stress, aq_read(stress_file, sheet = "R0. Results")),
  tar_target(subtypes, aq_read(subtypes_file)),
  tar_target(gw, aq_read(gw_file)),
  tar_target(licences, aq_read(licences_file)),
  tar_target(aquifer_map, aq_read(aquifer_map_file), format = "rds"),
  tar_target(hc, aq_hc()),

  # Clean data --------------------------------------------
  tar_target(wells, fmt_wells_db(wells_raw)),
  tar_target(ow_index, fmt_ow_index(wells)),
  tar_target(aquifers, fmt_aquifers(aquifers_raw, wells, hc)),

  tar_target(wl, fmt_water_levels(ow, ow_index)),
  tar_target(wd, fmt_wd(aquifer_map)),

  tar_target(aquifers_final,
             left_join(aquifers, wd, by = "aquifer_id"))

)
