#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# First time, make sure you have the stations cache directory setup
# stations_dl(quiet = TRUE)

# For updating ems, sometimes need more space for temp folder than is available
# If so, temporarily change the location of the temp folder by adding:
#
# TMPDIR=/TEMP/FOLDER/PATH
#
# to the .Renviron, then MAKE SURE IT EXISTS! and restart R
#
# usethis::edit_r_environ()
# tempdir() # check

targets::tar_make(reporter = "verbose_positives")

targets::tar_make_future(workers = 8, reporter = "verbose_positives")

# Housekeeping - remove old, unused target objects
tar_prune()


# Troubleshooting
# - add `browser()` to function then run:
targets::tar_make(callr_function = NULL, reporter = "verbose_positives")


# Find warnings/errors in specific targets
targets::tar_meta(fields = warnings, complete_only = TRUE)
targets::tar_meta(fields = errors, complete_only = TRUE)



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
