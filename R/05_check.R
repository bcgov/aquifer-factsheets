# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' Check for potential problems
#'
#' @param aq_ids List of Aquifers
#' @param pl_piperplot List of piperplot figures
#' @param figs_p2 Data frame of piperplot figures and text by aquifer_id and ow
#' @param extra_index_file Data frame of extra figures
#' @param factsheets List of factsheets created
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(targets)
#' tar_load_globals()
#' tar_source()
#'
#' tar_load(c("aq_ids", "pl_piperplot", "figs_p2", "extra_index_file", "factsheets"))
#' log_problems(aq_ids, pl_piperplot, figs_p2, extra_index_file, factsheets)
#'
#' }
log_problems <- function(aq_ids, pl_piperplot, figs_p2, extra_index_file, factsheets) {

  # Map but no factsheet produced
  p0 <- tibble(aquifer_id = aq_ids) |>
    filter(!aquifer_id %in% as.numeric(str_extract(factsheets, "(?<=AQ_)\\d{5}"))) |>
    mutate(problem = "Map exists but no factsheet produced",
           type = "factsheets")

  # Piper plots missing blurbs
  #dont_publish <- readxl::read_excel(f[["inputs_piperplots_text"]]) |>
  #  filter(stringr::str_detect(hydrogeochemistry, "Do not publish"))
  p1 <- tibble(piper = pl_piperplot) |>
    mutate(aquifer_id = as.numeric(str_extract(piper, "(?<!OW)\\d{4}")),
           ow = as.numeric(str_extract(piper, "(?<=OW)\\d{4}"))) |>
    left_join(unnest(figs_p2, "p2") |> select("aquifer_id", "ow", "ow_piper_text"),
              by = c("aquifer_id", "ow")) |>
    filter(!str_detect(piper, "missing") & (is.na(ow_piper_text) | str_detect(ow_piper_text, "No summary at this point"))) |>
    mutate(problem = "Have piper plot, but missing text",
           type = "piperplots") |>
    select("aquifer_id", "ow", "type", "problem")# |>
  #anti_join(dont_publish, by = c("aquifer_id", "ow" = "obs_well"))

  # Extra Page 3 images without blurbs and blurbs without page 3 images
  p2 <- extra_index_file |>
    filter(is.na(blurb) | is.na(image)) |>
    mutate(problem = case_when(is.na(blurb) ~ "Have extra image, but missing blurb",
                               is.na(image) ~ "Have extra blurb, but missing image"),
           type = "extra page 3") |>
    select("aquifer_id", "type", "problem")

  # Any extra content for an Aquifer we don't have a map for
  p3 <- extra_index_file |>
    filter(!aquifer_id %in% aq_ids) |>
    mutate(problem = "Have extra content, but missing Aquifer Map",
           type = "maps") |>
    select("aquifer_id", "type", "problem")


  p <- bind_rows(p0, p1, p2, p3)
  f <- file.path(f[["logs"]], paste0("log_problems_", Sys.Date(), ".csv"))
  write_csv(p, f)

  f
}

report_stats <- function(p1, figs_p2, figs_p3) {
  path_out <- file.path(f[["logs"]], paste0("report_stats_", Sys.Date(), ".csv"))

  p1 |>
    left_join(select(unnest(figs_p2, "p2"), "aquifer_id", starts_with("p2_")), by = "aquifer_id") |>
    left_join(select(unnest(figs_p3, "p3"), "aquifer_id", "p3_type", "p3_path_out"),
              by = "aquifer_id",relationship = "many-to-many") |>
    mutate(across(contains("p2"), \(x) if_else(stringr::str_detect(x, "missing"), NA, x))) |>
    mutate(across(c(-"aquifer_id", -"p3_type"), \(x) !is.na(x))) |>
    mutate(p2 = p2_gwl_ppt | p2_gwl_trends | p2_piperplots,
           p3 = p3_path_out,
           p2_3 = p2 & p3,
           p2_only = p2 & !p3,
           p3_only = p3 & !p2,
           p1_only = !p2 & !p3) |>
    summarize(n_factsheets = n(),
              n_p1_only = sum(p1_only),
              n_p2 = sum(p2),
              n_p3 = sum(p3),
              n_p2_p3_only = sum(p2_3),
              n_p2_only = sum(p2_only),
              n_p3_only = sum(p3_only),
              n_types = n_distinct(p3_type, na.rm = TRUE),
              types = paste0(unique(na.omit(p3_type)), collapse = "; "),
              n_with_piper = sum(p2_piperplots),
              n_with_gwl_ppt = sum(p2_gwl_ppt),
              n_with_gwl_trends = sum(p2_gwl_trends)) |>
    write_csv(path_out)
  path_out
}



#' Check for new piper plots and changes to existing
#'
#' @param year Numeric. Year for the run
#' @param log Character. Path to log file to find piperplot problems (will be loaded if not supplied)
#' @param ems Data frame. EMS data created by the targets run (will be loaded if not supplied)
#'
#' @returns
#' @export
#'
#' @examples
piper_plot_blurbs <- function(year, log = NULL, ems = NULL, zip = FALSE) {

  if(is.null(ems)) targets::tar_load("ems")
  if(is.null(log)) {
    targets::tar_source()
    log <- list.files("4_logs", "log_problems", full.names = TRUE) |>
      sort(decreasing = TRUE) |>
      _[1]
  }

  p <- log |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::filter(stringr::str_detect(problem, "Have piper plot")) |>
    dplyr::rename("obs_well" = "ow")

  # Get comparison

  # Get Previous version to compare to
  orig <- fs::dir_ls(fs::path(f[["inputs"]]), regexp = "piper_text_previous") |>
    sort(decreasing = TRUE) |>
    _[1] |>
    readxl::read_excel() |>
    dplyr::mutate(added = tidyr::replace_na(added, "unknown"))

  pp <- ems |>
    dplyr::as_tibble() |>
    # Should be able to omit missing, because to have a piper plot, MUST have at least one not missing
    tidyr::drop_na(water_type) |>
    dplyr::select("aquifer_id", "obs_well" = "StationID", "ems_id", "water_type", "missing_ion") |>
    dplyr::full_join(p, by = c("aquifer_id", "obs_well")) |>
    tidyr::nest(d = c(-"aquifer_id", -"obs_well")) |>
    dplyr::mutate(d = purrr::map(d, bcgwcat::dominant_water_types)) |>
    tidyr::unnest(d) |>
    dplyr::summarize(
      n = dplyr::n(),
      .by = c("aquifer_id", "obs_well", "ems_id", "water_type", "dominant", "missing_ion")) |>
    dplyr::arrange(aquifer_id, obs_well, dplyr::desc(n), water_type) |>
    dplyr::mutate(
      missing_ion = as.logical(missing_ion),
      n = dplyr::if_else(stringr::str_detect(water_type, "\\*"),
                         paste0(n, "*"), as.character(n)),
      n = dplyr::if_else(missing_ion, paste0(n, "MI"), n),
      water_type = stringr::str_remove(water_type, "\\*")) |>
    dplyr::summarize(
      n = paste0("(", paste0(n, collapse = ","), ")"),

      .by = c("aquifer_id", "obs_well", "ems_id", "water_type", "dominant", "missing_ion")) |>
    dplyr::mutate(water_type = paste(water_type, n)) |>
    dplyr::summarize(
      water_type_combo1 = paste0(water_type[dominant], collapse = "; "),
      water_type_combo2 = paste0(water_type[!dominant], collapse = "; "),
      water_type = dplyr::if_else(
        water_type_combo2 != "",
        paste0(water_type_combo1, "\n\nAlso: ", water_type_combo2),
        water_type_combo1),
      .by = c("aquifer_id", "obs_well", "ems_id")) |>
    dplyr::select(-"water_type_combo1", -"water_type_combo2") |>
    dplyr::full_join(orig, by = "ems_id", suffix = c("_new", "_orig")) |>
    dplyr::mutate(added = tidyr::replace_na(added, as.character(.env$year))) |>
    dplyr::mutate(
      change_removed = (is.na(aquifer_id_new) & !is.na(aquifer_id_orig)) | (is.na(obs_well_new) & !is.na(obs_well_orig)),
      change_aq = !is.na(aquifer_id_orig) & !is.na(aquifer_id_new) & aquifer_id_new != aquifer_id_orig,
      change_well = !is.na(obs_well_orig) & !is.na(obs_well_new) & obs_well_new != obs_well_orig,
      change_wt = !is.na(water_type_orig) & !is.na(water_type_new) & water_type_new != water_type_orig,
      change = paste0(
        dplyr::if_else(change_removed, glue::glue("Piperplot removed, likely Inactive well"), ""),
        dplyr::if_else(change_aq, glue::glue("New aquifer id: {aquifer_id_new} (originally {aquifer_id_orig}); "), ""),
        dplyr::if_else(change_well, glue::glue("New well no: {obs_well_new} (originally {obs_well_orig}); "), ""),
        dplyr::if_else(change_wt, glue::glue("New water type: '{water_type_new}'\n\n(originally '{water_type_orig}'); "), "")),
      hydrogeochemistry = tidyr::replace_na(hydrogeochemistry, "")) |>
    dplyr::arrange(aquifer_id_new, aquifer_id_orig, obs_well_new, obs_well_orig) |>
    dplyr::select("aquifer_id" = "aquifer_id_new", "obs_well" = "obs_well_new",
                  "ems_id", "water_type" = "water_type_new",
                  "hydrogeochemistry", "added", "change",
                  dplyr::contains("Notes"))

  create_piper_xlsx(pp)

  # - Add the following to the piper_text.xlsx file
  # - highlight in Green for need blurb
  # - Sort by aquifer id, ow
  #readr::write_csv(pp, file.path(f[["outputs"]], "add_to_piper_text.csv"))

  if(zip) {
    new_zip <- file.path(f[["outputs"]],
                         paste0("new_piperplots_", Sys.Date(), ".zip"))

    pp_zip <- tidyr::drop_na(pp, aquifer_id)

    file.path(
      f[["outputs_piperplots"]],
      paste0("piperplots_",
             stringr::str_pad(pp_zip$aquifer_id, pad = 0, width = 4),
             "_OW",
             stringr::str_pad(pp_zip$obs_well, pad = 0, width = 4),
             ".png")) |>
      zip(new_zip, file = _, flags = "-r9Xj")

    new_zip
  } else new_zip <- NULL
  new_zip
}

create_piper_xlsx <- function(pp) {

  new_piper <- openxlsx::createStyle(fgFill = "#81d41a")
  new_changes <- openxlsx::createStyle(fgFill = "#ffde59")
  do_not_publish <- openxlsx::createStyle(fgFill = "#ff0000")

  cols <- seq_len(ncol(pp))
  rows <- seq_len(nrow(pp) + 1)
  wrap_cols <- stringr::str_which(
    names(pp), stringr::regex("hydrogeochemistry|change|note|water", ignore_case = TRUE))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "blurbs")
  openxlsx::addWorksheet(wb, "legend")
  openxlsx::writeData(wb, 1, pp)
  openxlsx::addStyle(wb, 1, rows = rows, cols = 1:2, gridExpand = TRUE, stack = TRUE,
                     style = openxlsx::createStyle(halign = "center"))
  openxlsx::addStyle(wb, 1, rows = rows, cols, gridExpand = TRUE, stack = TRUE,
                     style = openxlsx::createStyle(
                       valign = "center", border = "TopBottomLeftRight", borderStyle = "thin"))
  openxlsx::addStyle(wb, 1, rows = rows, cols = wrap_cols,
                     gridExpand = TRUE, stack = TRUE,
                     style = openxlsx::createStyle(wrapText = TRUE))

  openxlsx::addStyle(wb, 1, new_piper, gridExpand = TRUE,
                     rows = 1 + which(pp$hydrogeochemistry == ""),
                     cols = cols, stack = TRUE)
  openxlsx::addStyle(wb, 1, new_changes, gridExpand = TRUE,
                     rows = 1 + which(pp$change != ""),
                     cols = cols, stack = TRUE)
  openxlsx::addStyle(
    wb, 1, do_not_publish, gridExpand = TRUE,
    rows = 1 + stringr::str_which(pp$hydrogeochemistry, stringr::regex("do not publish", ignore_case = TRUE)),
    cols = cols, stack = TRUE)

  openxlsx::setColWidths(wb, 1, cols = cols, widths = c(10, 10, 10,
                                                        30, 100, 8,
                                                        50, rep(50, ncol(pp) - 7)))

  # Add legend
  openxlsx::writeData(wb, 2, startCol = 1, startRow = 1, x = "Legend")
  openxlsx::writeData(wb, 2, startCol = 2, startRow = 2, x = "New Piperplots in need of a blurb")
  openxlsx::writeData(wb, 2, startCol = 2, startRow = 3, x = "Piperplots slated not to be published")
  openxlsx::writeData(wb, 2, startCol = 2, startRow = 4,
                      x = "Piperplots with a change in Aquifer ID, Well Number or Water Type")

  openxlsx::writeData(wb, 2, startCol = 2, startRow = 6,
                      x = "Water types with (X*) mean that the HCO3 was estimated from Meas Alk X times")
  openxlsx::writeData(wb, 2, startCol = 2, startRow = 7,
                      x = "Water types with (XMI) mean that the water type is Missing Ions, and is either all anions or all cations ")
  openxlsx::writeData(wb, 2, startCol = 2, startRow = 8,
                      x = "Water types with 'Also: ' indicate outlier observations which are not included in the piper plots.")


  openxlsx::addStyle(wb, 2, new_piper, cols = 1, rows = 2)
  openxlsx::addStyle(wb, 2, do_not_publish, cols = 1, rows = 3)
  openxlsx::addStyle(wb, 2, new_changes, cols = 1, rows = 4)

  openxlsx::saveWorkbook(wb, f[["inputs_piperplots_text"]], overwrite = TRUE)

  # Also keep a backup copy
  fs::file_copy(
    f[["inputs_piperplots_text"]],
    fs::path(f[["inputs_archive"]], paste0("piper_text_", Sys.Date(), "_auto.xlsx")),
    overwrite = TRUE)
}

