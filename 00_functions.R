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
  out_file <- paste0("AQ_", sprintf("%05d", aq_num), "_Aquifer_Factsheet") # Get aq_num with leading zeros
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
