# Creating Factsheets

### 0. Install software
- [Install R](https://mirror.its.sfu.ca/mirror/CRAN/)
- [Install RStudio](https://rstudio.com/products/rstudio/download/#download)

### 1. Get a copy of [the `aquifer-factsheets` repository](https://github.com/bcgov/aquifer-factsheets)
- Click on the green "Clone or download" button
- Click "Download Zip"
- Save and Unzip Project Folder
  
### 2. Setup the project
- Open the project by double-clicking on the `aquifer-factsheets.Rproj` file
- Open and run the complete `00_setup.R` script
  - This will install packages and create folders needed
  
### 3. Add supplemental data
Some data need to be supplied prior to running the aquifer factsheets.

All of the following should be placed in the `data` folder created in Step 2.

- Groundwater Trends Full Data (`clean_well_data.RData`) - [See Appendix](#1-groundwater-trends)
- Piper Plot descriptions (`piper_text.xlsx`)
- Index of extra page types (`extra_page_index.xlsx`)
- Images to include on extra pages (`extra_page_images.xlsx`)
  
### 4. Add supplemental figures
Some figures need to be supplied prior to running the aquifer factsheets.

All of the following should be placed in the `figures` folder created in Step 2.

- Aquifer maps in `maps` folder (i.e., `maps/Aquifer_Map_0001.pdf`)
- Piper plots in `piperplots` folder (i.e., `piperplots/Piperplot_0001_OW0001.jpg`)
- Extra figures in `extra` folder (i.e., `extra/Chilliwack_CrossSection_2L.jpg`) - [See Appendix](#2-extra-figures)
- Missing plot figures for Combo, Piper plots and Trend plots in `na` folder (i.e., `na/figure_missing_combo.png`)
- Figure explaining the boxplots for the companion document (`boxplot_key.png`)
- BC Gov Logo (`BCID_H_cmyk_rev.pdf`) - [See Appendix](#3-get-bc-government-logo)

### 5. Create factsheets
Open `run_all.R` and follow the instructions therein to do the following:

- Run `01_download.R` and `02_load.R` which will first download and then load all the data necessary
- Define which Aquifers you wish to create factsheets for
- Run `03_clean.R` to clean and summarize the data
- Run `04_output.R` to create figures for the factsheets
- Use the `factsheet()` function to create factsheets for the specified aquifers. 
  - Note that if you create a factsheet for an aquifer not specified in the `aquifers` object, it may create a factsheet, but that factsheet will likely be missing data and/or figures!
- Run the code to produce the companion document
  

### Appendix

#### 1. Groundwater Trends
Although the summarized trend data is available from the online BC Gov data repository, more detailed data is required to recreate the trend figures (for Page 2s). 

There are several steps needed to create the `clean_well_data.RData` data required:

- Clone/Download the [`groundwater-levels-indicator`](https://github.com/bcgov/groundwater-levels-indicator) repository
- In that project (i.e. **not** in this `aquifers-factsheet` project) run the following code
   - Make sure to replace `/home/steffi/Projects/aquifer-factsheets/data/` with the path to YOUR aquifer-factsheets folder
   - Alternatively, simply copy the `clean_well_data.RData` file from the `tmp` folder in the `groundwater-levels-indicator` project and paste it into the `data` folder in the `aquifer-factsheets` project.

```
library(envreportutils)

source("01_load.R
source("02_clean.R")

file.copy("tmp/clean_well_data.RData", "/home/steffi/Projects/aquifer-factsheets/data/")
```

#### 2. Extra Figures

Extra Figures are a method for including extra, non-standard pages in the Aquifer Factsheets. These pages occur after the normal type 2 page for specific observation wells (weather, aquifer trends, and piperplots). Each "extra page figure" can take up 1/2 of a page. 

To include extra page figures:

- Makes sure the figure dimensions are a ratio of 5:3 (e.g., 5 in wide and 3 in high)
- Place figures in the `figures/extra/` folder
- Each figure must be listed in the `extra_page_images.xlsx` file along with the Aquifer ID(s) that it corresponds to and the type of extra figure it is (e.g., `cross_section`)
- Each "type" of extra figure must be listed in `extra_page_index.xlsx`, along with the header it will receive and the order (i.e. if there are more than one type of extra page figures which should come first?)

#### 3. Get BC Government logo 

Download BC Government logo and unzip into figures folder (Then convert to pdf by hand)
```
if(!file.exists("./figures/BCID_H_cmyk_rev.eps")) {
  download.file(url = "http://www.corporate.gov.bc.ca/print-ads/Govt_of_BC_Logos/Resources/2018_BCID_Files.zip",
                dest = "./figures/2018_BCID_Files.zip")
  unzip(zipfile = "./figures/2018_BCID_Files.zip",
        files = "2018_BCID_Files/_Horizontal/Reverse/CMYK/BCID_H_cmyk_rev.eps",
        exdir = "./figures", junkpaths = TRUE)
  file.remove("./figures/2018_BCID_Files.zip")
}
```
