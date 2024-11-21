#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# Prepare files & Updates ----------------------------------
# rems::download_historic_data(ask = FALSE)


# Notes ----------------------------------------
# First time, make sure you have the stations cache directory setup

# weathercan::stations_dl(quiet = TRUE)

# For updating ems, sometimes need more space for temp folder than is available
# If so, temporarily change the location of the temp folder by adding:
#
# TMPDIR=/TEMP/FOLDER/PATH
#
# to the .Renviron, then MAKE SURE IT EXISTS! and restart R
#
# usethis::edit_r_environ()
# tempdir() # check

# For downloading the files, you'll probably need to increase the timeout.
# Add R_DEFAULT_INTERNET_TIMEOUT=300 to your .Renviron
# (you can do this via usethis;:edit_r_environ())

# Setup -----------------------
# Change draft <- FALSE in `_targets.R` for final

# Run targets workflow ----------------------------------

# TODO: Make the link check in the factsheets have a longer timeout (or a retry)

targets::tar_make_future(workers = 6, reporter = "verbose_positives")

# Housekeeping - remove old, unused target objects
targets::tar_prune()


# Recompile the companion document
# rmarkdown::render("1_inputs/templates/factsheet_methods.Rmd",
#                   output_file = "Aquifer Factsheet - Companion Document.pdf",
#                   output_dir = "3_factsheets/")


# Troubleshooting ------------------------
# - add `browser()` to function then run:
targets::tar_make(callr_function = NULL, reporter = "verbose_positives")


# Find warnings/errors in specific targets
targets::tar_meta(fields = c(time, warnings), complete_only = TRUE) |>

targets::tar_meta(fields = errors, complete_only = TRUE)

# Use the `debug` and `cue` options in `_targets.R` under `tar_option_set()`
# to step into the exact place with a problem


targets::tar_invalidate("figs_p2")

# Nitty gritty troubleshooting ----------------------------------------------
library(targets)
library(tidyverse)
tar_source()

tar_read(gwl_monthly)

# Some piperplots from last year not current = Wells because inactive
e <- tar_read(ems)
ow <- tar_read(ow_index)

filter(ow, ow == 228)
filter(ow, ow == 380)

fmt_wd(targets::tar_read(aquifer_map))

targets::tar_read(aquifer_map) |>
  st_set_agr("constant") |>
  fmt_wd()

# Checks and drafts --------------------------------
library(targets)
library(fs)
library(stringr)
tar_load(extra_index_file)
tar_source()

aqs <- sprintf("%05d", extra_index_file$aquifer_id) |>
  paste0(collapse = "|")

dir_create(d1 <- "~/pCloudDrive/aquifer_factsheets/2024_Page3s")
dir_create(d2 <- "~/pCloudDrive/aquifer_factsheets/2024_All")
dir_ls(f["factsheets"]) |>
  str_subset(aqs) |>
  file_copy(d1, overwrite = TRUE)

dir_ls(f["factsheets"]) |>
  file_copy(d2, overwrite = TRUE)



