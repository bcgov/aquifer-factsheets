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
# Create aquifer factsheets
##############################

# Load Packages and Functions --------------------------------------------------

# Check map and piperplots for correct names, fix issues with upper/lower case
# and leading zeros
checknames <- TRUE

source("00_functions.R")
source("00_header.R")

# Specify aquifers to run -------------------------------------------------

# Large run - Run all Aquifers that have a Map file
aquifers <- as.numeric(stringr::str_extract(list.files("./figures/maps/"), "[0-9]{4}"))

# Specify a Range of aquifers (from the above set)
# aquifers <- aquifers[aquifers >= 300 & aquifers <= 400]

# Specific specific aquifers
# aquifers <- c(6, 8, 353, 662, 738, 750, 15, 157)


# Update Data Sources -----------------------------------------------------
# NOTE: Sometimes the downloads don't work on the first try, if you get an error,
#       walk through the script and re-run lines which give you an error the first time
source("00_download.R") # You don't have to run this every time

# Prepare Data and Figures ------------------------------------------------
# These may take a while, depending how many aquifers have been selected
source("01_load.R")
source("02_clean.R")

delete_old <- TRUE   # Delete all old figures before rerunning?
source("03_output.R")

# Create all aquifer factsheets:
p <- progress_estimated(length(aquifers))
for (a in aquifers) {
  factsheet(a, draft = TRUE)
  p$tick()$print()
}

# Create a single factsheet
factsheet(1156)
factsheet(353)
factsheet(21) #Example with many wells

factsheet(98)
factsheet(219)

factsheet(662)

# Create a single factsheet with only page 1
factsheet(15, draft = FALSE, pages = 1)

# Create the Companion Document -------------------------------------------
rmarkdown::render("./templates/factsheet_methods.Rmd",
                  output_file = "Aquifer Factsheet - Companion Document.pdf",
                  output_dir = "./factsheets/")
