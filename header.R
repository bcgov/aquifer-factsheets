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


## Load required packages
suppressMessages({
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(stringr)
library(lubridate)
library(httr)

library(ggplot2)
library(scales)

library(knitr)
library(rmarkdown)
library(kableExtra)

library(bcgroundwater)
library(bcdata)

library(weathercan)
})

# Check names and fix if necessary:
checknames <- TRUE
if(exists("checknames") && checknames){
  if(!exists("checked") || !checked) {
    checked <- check_piper_plots()
  }
  fix_names(type = "maps", filename = "Aquifer_Map", ext = "pdf")
  fix_names(type = "piperplots", filename = "Piperplot", ext = "jpg")
  fix_names(type = "piperplots_trimmed", filename = "Piperplot", ext = "jpg")

  # Check piperplots against pipertext
  p <- tibble(file = list.files("./figures/piperplots/", pattern = "Piperplot")) %>%
    mutate(file2 = str_remove_all(file, "(Piperplot_)|(.jpg)"),
           aquifer_id = as.numeric(str_extract(file2, "^[0-9]{4}")),
           obs_well = as.numeric(str_extract(file2, "[0-9]{4}$")))

  p_text <- read_excel("./data/piper_text.xlsx") %>%
    mutate(aquifer_id = as.numeric(str_extract(AQUIFER_ID, "[0-9]{1,4}")))

  # No fig
  no_fig <- anti_join(select(p_text, aquifer_id, obs_well),
                      select(p, aquifer_id, obs_well),
                      by = c("obs_well", "aquifer_id"))

  # No text
  no_text <- anti_join(select(p, aquifer_id, obs_well),
                       select(p_text, obs_well, aquifer_id),
                       by = c("obs_well", "aquifer_id"))

  # Backup log files
  logs <- list.files("./out/", pattern = "LOG_PIPER_MISSING", full.names = TRUE)
  file.copy(logs, "./out/archive/", overwrite = TRUE)
  file.remove(logs)

  # Save new log files
  if(nrow(no_fig) > 0) {
    f <- paste0("./out/LOG_PIPER_MISSING_FIG_", Sys.Date(), ".csv")
    write_csv(no_fig, f)
    message("\nSome piperplots listed in ./data/piper_text.xlsx do not have ",
            "corresponding figures in ./figures/piperplots/...\n",
            "Details saved to ", f)
  }

  if(nrow(no_text) > 0) {
    f <- paste0("./out/LOG_PIPER_MISSING_TEXT_", Sys.Date(), ".csv")
    write_csv(no_text, f)
    message("\nSome piperplots with figures in ./figures/piperplots/ do not have ",
            "corresponding text in ./data/piper_text.xlsx ...\n",
            "Details saved to ", f)
  }

}
