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
if(exists("checknames") && checknames){
  check_piper_plots()
  fix_names(type = "maps", filename = "Aquifer_Map", ext = "pdf")
  fix_names(type = "piperplots", filename = "Piperplot", ext = "jpg")
}
