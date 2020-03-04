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
# Create aquifer factsheets
##############################

# Load functions and header
source("functions.R")
source("header.R")

# Update Data Sources -----------------------------------------------------
# NOTE: Sometimes the downloads don't work on the first try, if you get an
#   error, walk through the 01_download.R script and re-run lines which give you
#   an error the first time
source("01_download.R") # You don't have to run this every time



# Prepare Data and Figures ------------------------------------------------
# These may take a while, depending how many aquifers have been selected
source("02_load.R")




# Specify aquifers to run -------------------------------------------------

# Large run - Run all Aquifers that have a Map file
aquifers <- as.numeric(str_extract(list.files("./figures/maps/"), "[0-9]{4}"))

# Specify a Range of aquifers (from the above set)
#aquifers <- aquifers[aquifers >= 300 & aquifers <= 400]

# Specific specific aquifers
aquifers <- c(6, 8, 353, 662, 738, 750, 15, 157)



# Clean data and Create figures -------------------------------------------
delete_old <- TRUE   # Delete all old figures before rerunning?
source("03_clean.R")
source("04_output.R")



# Create aquifer factsheets -----------------------------------------------

# Create all aquifer factsheets
factsheet(aquifers, draft = TRUE)

# Create a single factsheet
#factsheet(608, draft = TRUE)
#factsheet(21)

# Create a single factsheet with only page 1
#factsheet(15, draft = FALSE, pages = 1)

# Create the Companion Document -------------------------------------------
rmarkdown::render("./templates/factsheet_methods.Rmd",
                  output_file = "Aquifer Factsheet - Companion Document.pdf",
                  output_dir = "./factsheets/")
