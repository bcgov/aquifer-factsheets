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
# Preparation
##############################

## Install the packages we will need from CRAN:
package_list <- c("dplyr", "tidyr", "readr", "readxl", "ggplot2", "stringr",
                  "lubridate", "purrr", "scales", "knitr", "rmarkdown", "kableExtra",
                  "weathercan")
package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_new)) install.packages(package_new)

# Check versions and update
if(packageVersion("weathercan") < "0.3.1") install.packages("weathercan")
if(packageVersion("tidyr") < "1.0.0") install.packages("tidyr")


## Install the packages we will need from GitHub:
package_github <- c(bcgov = "bcgroundwater", bcgov = "bcdata")
package_new <- package_github[!(package_github %in% installed.packages()[,"Package"])]
if(length(package_new)) {
  remotes::install_github(paste(names(package_new), package_new, sep = "/"))
}

## Load required packages
suppressMessages({
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

library(ggplot2)
library(scales)

library(knitr)
library(rmarkdown)
library(kableExtra)

library(bcgroundwater)
library(bcdata)

library(weathercan)
})

## Create project directories
if (!dir.exists("tmp")) dir.create("tmp")
if (!dir.exists("data_dl")) dir.create("data_dl")
if (!dir.exists("out")) dir.create("out")
if (!dir.exists("out/gwl")) dir.create("out/gwl")
if (!dir.exists("out/boxplots")) dir.create("out/boxplots")
if (!dir.exists("out/trends")) dir.create("out/trends")
if (!dir.exists("factsheets")) dir.create("factsheets")

# Check names and fix if necessary:
if(exists("checknames") && checknames){
  fix_names(type = "maps", filename = "Aquifer_Map", ext = "pdf")
  fix_names(type = "piperplots", filename = "Piperplot", ext = "jpg")
}
