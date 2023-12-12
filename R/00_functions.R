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

# Functions ----------------------------

aq_read <- function(file, sheet = NULL, clean = TRUE) {
  ext <- fs::path_ext(file)
  if(ext == "csv") d <- readr::read_csv(file, guess_max = 200000, show_col_types = FALSE)
  if(ext == "xlsx") d <- readxl::read_excel(file, sheet)
  if(ext == "fst") d <- fst::read_fst(file)
  if(ext == "rds") d <- readr::read_rds(file)
  if(clean) janitor::clean_names(d) else d
}

aq_dl <- function(x, remove_sf = FALSE) {

  # Download URL
  if("url" %in% names(x)) GET(x$url, write_disk(x$path, overwrite = TRUE))

  # Download BC Data record
  if("record" %in% names(x)) {
    d <- bcdc_get_data(x$record, x$resource)
    if(remove_sf) d <- st_drop_geometry(d)
    if(inherits(d, "sf")) write_rds(d, x$path) else write_fst(d, x$path)
  }

  x$path
}

aq_unzip <- function(zip, path, files) {
  unzip(zip, exdir = path, files = files, overwrite = TRUE)
  file.path(path, files) |>
    setNames(files)
}

aq_bcdata_url <- function(record, name) {
  bcdata::bcdc_tidy_resources(record) |>
    dplyr::filter(.data$name == .env$name) |>
    dplyr::pull(.data$url)
}

aq_hc <- function() {
  # Hydraulic Connectivity
  # (originally from "Hydraulic Connectivity Table.csv" of unknown origin)
  tribble(~ aquifer_subtype_code, ~ hydraulic_connectivity,
          "1a", "Likely",
          "1b", "Likely",
          "1c", "Likely",
          "2", "Likely",
          "3", "Likely",
          "4a", "Likely",
          "4b", "Not Likely",
          "4c", "Not Likely",
          "5a", "Not Likely",
          "5b", "Likely",
          "6a", "Not Likely",
          "6b", "Not Likely",
          "UNK", "Unknown")
}

f <- function(name, type = NULL, f = NULL) {
  d <- dirs[stringr::str_subset(names(dirs), name)]
  if(!is.null(type)) d <- stringr::str_subset(d, type)
  if(!is.null(f)) d <- fs::path(d, f)
  d
}





min_na <- function(x) {
  if(!all(is.na(x))) min(x, na.rm = TRUE) else NA_real_
}

max_na <- function(x) {
  if(!all(is.na(x))) max(x, na.rm = TRUE) else NA_real_
}




get_breaks <- function(min, max, length.out) {

  by <- (max - min) / (length.out)

  to <- case_when(by > -0.25 ~ -0.10,
                  by > -0.5 ~ -0.25,
                  by > -3 ~ -0.5,
                  TRUE ~ -1)

  by <- round(by/to) * to
  min <- round(min/by) * by
  max <- round(max/by) * by
  seq(min, max, by = by)
}

fix_names <- function(dir_maps = f("maps"), filename, ext, digits = 4) {

  f <- list.files(file.path(dir_maps))

  d <- paste0("_[0-9]{", digits, "}")
  d_nice <- paste0(rep("0", digits), collapse = "")

  mismatch <- f[!str_detect(f, paste0(filename, d, ".", ext))]
  mismatch <- mismatch[mismatch != "Thumbs.db"]

  if(length(mismatch) > 0) {

    w <- paste0(type, " should have file names of ", filename, "_",
                d_nice, ".", ext, ", but...")
    mismatch <- tibble(orig = mismatch,
                       new = mismatch)

    # wrong extension?
    if(any(!str_detect(mismatch$new, paste0(ext, "$")))) {
      w <- paste0(w, "\n", " - some do not have the correct extension")
    }

    # lower/uppercase issues
    if(any(!str_detect(mismatch$new, filename) &
           str_detect(mismatch$new, regex(filename, ignore_case = TRUE)))) {
      w <- paste0(w, "\n", " - some have incorrect upper/lower case letters. Fixing...")
      mismatch <- mutate(mismatch,
                         new = str_replace(new, regex(filename, ignore_case = TRUE), filename))
    }

    # wrong name?
    if(any(!str_detect(mismatch$new, filename))) {
      w <- paste0(w, "\n", " - some have an incorrect filename (even after fixing lower/upper case letters).")
    }

    # wrong number of digits
    if(!any(str_detect(mismatch$new, d))) {
      w <- paste0(w, "\n", " - some have the wrong number of digits. Fixing...")

      mismatch <- mutate(mismatch,
                         id = str_extract_all(new, paste0("[0-9]{1,", digits, "}")),
                         id = map(id, ~sprintf("%04d", as.numeric(.))),
                         id = map_chr(id, ~paste0(., collapse = "_OW")),
                         new = paste0(filename, "_", id, ".", ext))
    }
    message(w)
    if(nrow(mismatch <- filter(mismatch, orig != new)) > 0) {
      file.rename(from = file.path(dir, type, mismatch$orig),
                  to = file.path(dir, type, mismatch$new))
      message(paste0(paste0("Renaming ", mismatch$orig, " to ", mismatch$new), collapse = "\n"))
    }
  }
}

# Based on plyr round_any and this stack overflow answer:
# https://stackoverflow.com/a/46489816/3362144
round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
}

check_piper_plots_gwells <- function(dir_piper = f("piper")) {
  p <- tibble(file = list.files(dir_piper)) %>%
    mutate(ow = str_extract(file, "OW[0-9]{4}"),
           ow = as.numeric(str_extract(ow, "[0-9]{4}")),
           aquifer_id = str_extract(file, "_[0-9]{4}_"),
           aquifer_id = as.numeric(str_extract(aquifer_id, "[0-9]{4}")))

  g <- read_csv(f("out_data", f = "well.csv"), guess_max = Inf, col_types = cols(),
                n_max = 1000000000) %>%
    select(aquifer_id, ow = observation_well_number) %>%
    filter(!is.na(ow)) %>%
    mutate(ow = as.numeric(ow))

  compare <- full_join(p, g, by = "ow", suffix = c("_piper", "_gwells")) %>%
    filter(aquifer_id_piper != aquifer_id_gwells)

  if(nrow(compare) > 0) {
    message("Mismatch between Piperplot Aquifers and GWELLS Aquifers, see:\n '",
            f("output"), "/LOG_PIPER_MISMATCH_", Sys.Date(), ".csv'")
    write_csv(compare, paste0(f("output"), "/LOG_PIPER_MISMATCH_", Sys.Date(), ".csv"))
  } else {
    message("No mismatches between Piperplots and GWELLS")
  }
  TRUE
}

check_piper_plots_text <- function(dir_piper = f("piper"),
                                   file_piper = f("in_data", f = "piper_text.xlsx"),
                                   dir_maps = f("maps")) {

  # Check piperplots against pipertext
  p <- tibble(file = list.files(dir_piper, pattern = "piperplot")) %>%
    mutate(file2 = str_remove_all(file, "(piperplot_)|(.png)"),
           aquifer_id = as.numeric(str_extract(file2, "^[0-9]{4}")),
           obs_well = as.numeric(str_extract(file2, "[0-9]{4}$")),
           plot = TRUE)

  p_text <- read_excel(file_piper)

  # All text
  all_text <- full_join(p_text,
                        select(p, aquifer_id, obs_well, plot),
                        by = c("aquifer_id", "obs_well")) %>%
    mutate(plot = replace_na(plot, FALSE))

  map_aquifers <- list.files(dir_maps) %>%
    str_extract("[0-9]{4}") %>%
    as.numeric()

  # No fig
  no_fig <- all_text %>%
    filter(!plot, hydrogeochemistry != "Do not publish") %>%
    mutate(map = aquifer_id %in% map_aquifers)

  # Wrong Aquifer ID
  wrong_id <- left_join(select(p, aquifer_id, obs_well),
                        select(p_text, aquifer_id, obs_well),
                        by = c("obs_well"),
                        suffix = c("_gwells", "_text")) %>%
    filter(aquifer_id_gwells != aquifer_id_text)

  # Get water type where there is no text
  # TODO: Where does ems.csv come from?
  ems <- read_csv(f("out_data", f = "ems.csv"), show_col_types = FALSE, guess_max = Inf) %>%
    group_by(obs_well = StationID) %>%
    select(ems_id = SampleID, obs_well, water_type) %>%
    mutate(ems_id = str_remove(ems_id, "-[0-9]+$")) %>%
    group_by(obs_well, water_type) %>%
    summarize(n = sum(!is.na(water_type)),
              ems_id = paste0(unique(ems_id), collapse = ","),
              .groups = "drop") %>%
    arrange(desc(n)) %>%
    group_by(ems_id, obs_well) %>%
    summarize(water_type = paste0(water_type, " (", n, ")"),
              water_type = paste0(water_type[water_type != "NA (0)"],
                                             collapse = "; "),
              .groups = "drop") %>%
    distinct()

  all_text <- left_join(select(all_text, -water_type, -ems_id),
                        select(ems, ems_id, obs_well, water_type),
                        by = c("obs_well")) %>%
    select(aquifer_id, obs_well, ems_id, everything()) %>%
    arrange(aquifer_id, obs_well)

  no_text <- filter(all_text, is.na(hydrogeochemistry)) %>%
    select(aquifer_id, obs_well, ems_id, water_type)

  # Backup log files
  logs <- list.files(f("outputs"), pattern = "LOG_PIPER_", full.names = TRUE)
  file.copy(logs, f("out_archive"), overwrite = TRUE)
  file.remove(logs)

  # Save new log files
  if(nrow(no_fig) > 0) {
    f <- paste0(f("outputs"), "/LOG_PIPER_MISSING_FIG_", Sys.Date(), ".csv")
    write_csv(no_fig, f)
    message("\nSome piperplots listed in ", text_file, " do not have ",
            "corresponding figures in ", dir_piper, "...\n",
            "Details saved to ", f)
  }

  if(nrow(no_text) > 0) {
    f <- paste0(f("outputs"), "/LOG_PIPER_MISSING_TEXT_", Sys.Date(), ".csv")
    write_csv(no_text, f)
    message("\nSome piperplots with figures in ", dir_piper, " do not have ",
            "corresponding text in ", text_file, "...\n",
            "Details saved to ", f)
  }

  if(nrow(wrong_id) > 0) {
    f <- paste0(f("outputs"), "/LOG_PIPER_TEXT_AQUIFER_ID_", Sys.Date(), ".csv")
    write_csv(wrong_id, f)
    message("\nSome piperplots with listed in ", text_file, " do not ",
            "correspond to the same Aquifer ID as in GWELLS...\n",
            "Details saved to ", f)
  }

}

check_piper_plots <- function(dir_piper = f("piper"),
                              file_piper = f("in_data", "piper_text.xlsx"),
                              which = c("text")) {

  if("gwells" %in% which) check_piper_plots_gwells(dir_piper)
  if("text" %in% which) check_piper_plots_text(dir_piper, file_piper)
}




