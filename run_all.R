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

# Update Data Sources -----------------------------------------------------
# NOTE: Sometimes the downloads don't work on the first try, if you get an
#   error, walk through the 01_download.R script and re-run lines which give you
#   an error the first time
source("00_setup.R")

if(TRUE) source("01_download.R") # You don't have to run this every time

# Prepare Data ------------------------------------------------------------
source("02_load.R")
source("03_clean.R")

# Check aquifer map names and select aquifers to use
fix_names(type = "maps", filename = "Aquifer_Map", ext = "pdf") # Check/fix map names
aquifers <- as.numeric(str_extract(list.files("./figures/maps/"), "[0-9]{4}"))  #ALL Aquifers

#aquifers <- c(aquifers, 1248:1274)

#aquifers <- 229

# wells_db %>%
#   filter(!is.na(ow), is.na(ems), ow_status != "Inactive", aquifer_id %in% aquifers) %>%
#   select(aquifer_id, ow) %>%
#   arrange(aquifer_id, ow) %>%
#   mutate(txt = paste0("Aq", aquifer_id, "/OW", ow)) %>%
#   pull(txt) %>%
#   write("out/gwells_missing_ems.txt")

# Create figures -------------------------------------------
# - These may take a while, depending how many aquifers have been selected
# - Every year, make sure to check piper plot omits to ensure they're still valid

delete_old <- TRUE   # Delete all old figures before rerunning?
source("04_output.R")


# Checks -----------------------
check_piper_plots() # Check piper plot text vs. figures


# Create aquifer factsheets -----------------------------------------------
source("00_setup.R")

# Create all aquifer factsheets
factsheet(aquifers, draft = TRUE)


#factsheet(aquifers[aquifers <= 866], draft = FALSE)

factsheet(c(37), draft = TRUE)

factsheet(c(131, 255, 297, 390, 1241), draft = TRUE)



# Create some aquifer factsheets
# - Here, the first 10
# - Note that this is the first 10 in the list, NOT aquifer IDs 1-10
# - This is useful when you want to run aquifers factsheets bit by bit (i.e. over lunch)
factsheet(aquifers[1:10], draft = TRUE)

# Create a single factsheet
factsheet(1242, draft = TRUE)
#factsheet(21)


factsheet(c(6, 8, 25, 254, 255, 256, 259, 1197, 1199), draft = TRUE)
factsheet(c(220, 320), draft = TRUE)
# Create a single factsheet with only page 1
#factsheet(15, draft = FALSE, pages = 1)

factsheet(c(133, 134, 157, 1147, 50), draft = TRUE)

factsheet(c(27, 50, 161, 172), draft = TRUE)

factsheet(c(133, 161), draft = TRUE)

factsheet(c(25, 254), draft = TRUE)

factsheet(c(1, 8, 15, 133, 157, 1147), draft = TRUE) # E.g. with only 1 obs Hydraulic conductivity

# Archive old sheets
old <- list.files("factsheets/", "DRAFT", full.names = TRUE)
file.copy(old, "factsheets/Archive")
file.remove(old)

# Rename and zip final run
final <- list.files("factsheets", "AQ_[0-9]{5}_Aquifer_Factsheet_[0-9]{4}-[0-9]{2}-[0-9]{2}.pdf",
                    full.names = TRUE) %>%
  tibble(names = .) %>%
  mutate(aq = str_extract(tolower(names), "(?<=aq_)[0-9]{5}"),
         new_names = file.path("factsheets", paste0("aquifer_factsheet_", aq, ".pdf")))

file.rename(from = final$names, to = final$new_names)
zip(paste0("factsheets_final_", Sys.Date(), ".zip"),
    files = final$new_names)

# Create the Companion Document -------------------------------------------
rmarkdown::render("./templates/factsheet_methods.Rmd",
                  output_file = "Aquifer Factsheet - Companion Document.pdf",
                  output_dir = "./factsheets/")

