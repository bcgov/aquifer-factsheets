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

normals_yrs <- "1981-2010" # Available: https://climate.weather.gc.ca/climate_normals/index_e.html

# Wells / Aquifers to omit ------------------
omit_ow <- 433        # Not in Aquifer 217, but below
omit_ems <- "E290173" # Manually measured well

# Aquifer ids subset
sub_aq <- c()

# See run.R for runs and troubleshooting


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
  tar_target(wells_file, gwells_files[["well.csv"]], format = "file"),

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

  tar_target(wells,       fmt_wells(wells_file, aq_ids, omit_ow)),
  tar_target(ow_index,    fmt_ow_index(wells)),

  tar_target(wl,          fmt_water_levels(ow_file, ow_index)),
  tar_target(gwl_trends,  fmt_gwl(gwl_trends_file, ow_index)),
  tar_target(gwl_monthly, fmt_gwl(gwl_monthly_file, ow_index)),
  tar_target(ppt_normals, fmt_ppt_normals(ppt_normals_raw)),
  tar_target(ems,         fmt_ems(ow_index, omit_ems, update = ems_updated)),

  tar_target(
    aquifers,
    fmt_aquifers(aquifers_file, aq_ids, wells, regions, licences, subtypes, stress)),
)
