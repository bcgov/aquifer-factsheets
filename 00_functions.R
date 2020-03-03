# Copyright 2018 Province of British Columbia
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

##############################
# Functions
##############################

factsheet <- function(aq_num, pages = 2, draft = FALSE,
                      data_folder = NULL, out_folder = "./factsheets/",
                      template_path = NULL,
                      keep_tex = FALSE) {

  if(is.null(data_folder)) data_folder <- getwd()
  if(is.null(template_path)) template_path <- file.path(getwd(), "./templates/factsheet_template.Rmd")

  if(tolower(tools::file_ext(template_path)) != "rmd") stop("template_path must point to an .Rmd file")
  out_file <- paste0("AQ_", sprintf("%05d", aq_num), "_Aquifer_Factsheet_",
                     Sys.Date()) # Get aq_num with leading zeros
  if(draft) out_file <- paste0(out_file, "_DRAFT.pdf") else out_file <- paste0(out_file, ".pdf")

  rmarkdown::render(template_path,
                    params = list(aq_num = aq_num,
                                  pages = pages,
                                  draft = draft),
                    output_options = list(keep_tex = keep_tex),
                    output_file = out_file,
                    output_dir = out_folder, clean = TRUE, quiet = TRUE)
}

draft_tag <- function() {
  cat("\\begin{tikzpicture}",
      "\\node[at = (current page.center), opacity = 0.2, font=\\fontsize{240}{164}\\selectfont, rotate = 45] {DRAFT};",
      "\\end{tikzpicture}"
  )
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

check_piper_plots <- function(dir = "./figures/piperplots") {

  p <- tibble(file = list.files(dir)) %>%
    mutate(ow = str_extract(file, "OW[0-9]{4}"),
           ow = as.numeric(str_extract(ow, "[0-9]{4}")),
           aquifer_id = str_extract(file, "_[0-9]{4}_"),
           aquifer_id = as.numeric(str_extract(aquifer_id, "[0-9]{4}")))

  g <- read_csv("data_dl/well.csv", guess_max = 200000) %>%
    select(aquifer_id, ow = observation_well_number) %>%
    filter(!is.na(ow)) %>%
    mutate(ow = as.numeric(ow))

  compare <- full_join(p, g, by = "ow", suffix = c("_piper", "_gwells")) %>%
    filter(aquifer_id_piper != aquifer_id_gwells)

  write_csv(compare, "piper_aquifers.csv")

}

fix_names <- function(dir = "./figures", type, filename, ext, digits = 4) {

  f <- list.files(file.path(dir, type))

  if(type == "maps") {
    d <- paste0("_[0-9]{", digits, "}")
    d_nice <- paste0(rep("0", digits), collapse = "")
  }
  if(type == "piperplots") {
    d <- paste0("_[0-9]{", digits, "}_OW[0-9]{", digits, "}")
    d_nice <- paste0(paste0(rep("0", digits), collapse = ""),
                     "_OW", paste0(rep("0", digits), collapse = ""))
  }

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
      message(paste0("\n", paste0("Renaming ", mismatch$orig, " to ", mismatch$new), collapse = "\n"))
    }
  }
}

# Based on plyr round_any and this stack overflow answer:
# https://stackoverflow.com/a/46489816/3362144
round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
}
