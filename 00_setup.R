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


##############################
# Preparation
##############################

## Install the packages we will need from CRAN:
package_list <- c("dplyr", "tidyr", "readr", "readxl", "ggplot2", "stringr",
                  "lubridate", "purrr", "scales", "sf", "knitr", "rmarkdown",
                  "kableExtra", "weathercan", "httr")
package_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(package_new)) install.packages(package_new)

# Check versions and update
if(packageVersion("weathercan") < "0.3.1") install.packages("weathercan")
if(packageVersion("tidyr") < "1.0.0") install.packages("tidyr")
if(packageVersion("dplyr") < "0.8.0") install.packages("dplyr")

## Install the packages we will need from GitHub:
package_github <- c(bcgov = "bcgroundwater", bcgov = "bcdata", bcgov = "bcmaps")
package_new <- package_github[!(package_github %in% installed.packages()[,"Package"])]
if(length(package_new)) {
  remotes::install_github(paste(names(package_new), package_new, sep = "/"))
}

rm(package_github, package_list, package_new)


## Create project directories
if (!dir.exists("tmp")) dir.create("tmp")
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data_dl")) dir.create("data_dl")
if (!dir.exists("out")) dir.create("out")
if (!dir.exists("out/gwl")) dir.create("out/gwl")
if (!dir.exists("out/boxplots")) dir.create("out/boxplots")
if (!dir.exists("out/trends")) dir.create("out/trends")
if (!dir.exists("out/archive")) dir.create("out/archive")
if (!dir.exists("factsheets")) dir.create("factsheets")
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("figures/maps")) dir.create("figures/maps")
if (!dir.exists("figures/piperplots")) dir.create("figures/piperplots")
if (!dir.exists("figures/extra")) dir.create("figures/extra")
if (!dir.exists("figures/na")) dir.create("figures/na")
