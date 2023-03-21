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
  packages = c("tibble", "readr", "readxl",
               "dplyr", "tidyr", "purrr", "stringr", "lubridate", "ggplot2",
               "sf", "assertr", "magick", "scales",
               "bcgroundwater", "bcdata", "rems2aquachem",
               "weathercan", "smwrBase", "smwrGraphs"),
  format = "rds"
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# source("other_functions.R") # Source other scripts as needed. # nolint

prep_dir()

# Targets
list(
  tar_download(
    aquifer_db_raw,
    urls = "https://apps.nrs.gov.bc.ca/gwells/api/v1/aquifers/csv",
    paths = "./data_dl/aquifers.csv"),

  #tar_download(aquifer_subtypes, "data_dl/aquifer_subtypes.csv"),

  tar_target(aquifer_db, clean_aquifer_db(aquifer_db_raw))
)
