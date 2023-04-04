#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# First time, make sure you have the stations cache directory setup
# stations_dl(quiet = TRUE)


targets::tar_make(reporter = "verbose_positives")
# targets::tar_make_clustermq(workers = 2) # nolint

targets::tar_make_future(workers = 10,
                         reporter = "verbose_positives")

# Troubleshooting
# - add `browser()` to function then run:
targets::tar_make(callr_function = NULL, reporter = "verbose_positives")

# Find warnings in specific targets
targets::tar_meta(fields = warnings, complete_only = TRUE)




# Nitty gritty troubleshooting ----------------------------------------------
targets::tar_source()

targets::tar_read(ems_4afc36e3)

fmt_wd(targets::tar_read(aquifer_map))

targets::tar_read(aquifer_map) |>
  st_set_agr("constant") |>
  fmt_wd()
