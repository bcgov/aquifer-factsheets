# Creating Factsheets

### 0. Install software
- [Install R](https://mirror.its.sfu.ca/mirror/CRAN/)
- [Install RStudio](https://rstudio.com/products/rstudio/download/#download)
- Consider updating R if you have an older version 
  - Type `R.version.string` to see what version of R you have
  - If your R version is < 3.6.0 consider updating

### 1. Get a copy of [the `aquifer-factsheets` repository](https://github.com/bcgov/aquifer-factsheets)
- Click on the green "Clone or download" button
- Click "Download Zip"
- Save and Unzip Project Folder
  
### 2. Setup the project
- Open the project by right-clicking on the `aquifer-factsheets.Rproj` file and open with RStudio
- Open and run the complete `00_setup.R` script
  - This will install packages and create folders needed
  
### 3. Add supplemental data
Some data need to be supplied prior to running the aquifer factsheets.

All of the following should be placed in the **`data`** folder created in Step 2.

- Groundwater Trends Full Data (`clean_well_data.RData`) - [See Appendix](#1-groundwater-trends)
- Piper Plot descriptions (`piper_text.xlsx`)
- Index of extra page types (`extra_page_index.xlsx`)
  
### 4. Add supplemental figures
Some figures need to be supplied prior to running the aquifer factsheets.

All of the following should be placed in the `figures` folder created in Step 2.

- Aquifer maps in **`maps`** folder (i.e., `maps/Aquifer_Map_0001.pdf`)
- Piper plots in **`piperplots`** folder (i.e., `piperplots/Piperplot_0001_OW0001.jpg`)
- Extra figures in **`extra`** folder (i.e., `extra/Aquifer 137 Water Budget.jpg`) - [See Appendix](#2-extra-figures)
- Missing plot figures for Combo, Piper plots and Trend plots in **`na`** folder (i.e., `na/figure_missing_combo.png`)
- Figure explaining the boxplots for the companion document (`boxplot_key.png`)
- BC Gov Logo (`BCID_H_cmyk_rev.pdf`) - [See Appendix](#3-get-bc-government-logo)

### 5. Create factsheets
Open `run_all.R` and follow the instructions therein to do the following:

- Run `01_download.R` and `02_load.R` which will first download and then load all the data necessary
- Define which Aquifers you wish to create factsheets for
- Run `03_clean.R` to clean and summarize the data
- Run `04_output.R` to create figures for the factsheets
- Use the `factsheet()` function to create factsheets for the specified aquifers. 
  - **Note** that if you create a factsheet for an aquifer not specified in the `aquifers` object, it may create a factsheet, but that factsheet will likely be missing data and/or figures!
  - You can specify `draft = TRUE` to create factsheets with the "DRAFT" watermark
  - Factsheet pdfs are produced in the `factsheets` folder
- Run the code to produce the companion document

## To Update Factsheets

- New Aquifer Maps in the `figures/maps` folder
- New Piperplots in the `figures/piperplots` folder
  - New Piperplot text added to the `piper_text.xlsx` file
- New Extra Supplementary information 
  - Images added to the `figures/extra` folder
  - If a new type of supplementary (i.e water budget), also add to `data/extra_page_index.xlsx`
- Download new data (see `run_all.R`)
- Create the factsheets!


## Potential Problems

### Local Team
- File names should remain constant
  - i.e. `Aquifer_0000.pdf`, `Piperplot_0000_OW0000.jpg`
- Groundwater Indicator data shouldn't change. If they do an update, we'll need to get a new `clean_well_data.Rdata` file.

### Potentially out of Team's Control
- Links to resources shouldn't change 
  - Links to static resources
  - Links to Aquifer factsheets and companion document
- Data sources in the `01_download.R` should remain active
- R and R packages shouldn't change too much in their functionality


## Appendix

#### 1. Groundwater Trends
Although the summarized trend data is available from the online BC Gov data repository, more detailed data is required to recreate the trend figures (for Page 2s). 

The `clean_well_data.RData` data is created by running the `01_load.R` and `02_clean.R` scripts in the [`groundwater-levels-indicator`](https://github.com/bcgov/groundwater-levels-indicator) repository. 

Unfortunately one of the data sources is currently unavailable, so these scripts are not currently usable.

#### 2. Extra Figures

Extra Figures are a method for including extra, non-standard pages in the Aquifer Factsheets. 
These pages occur after the normal type 2 page for specific observation wells (weather, aquifer trends, and piperplots).
Each "extra page figure" can take up 1/2 of a page. 

To include extra page figures:

- Makes sure the figure dimensions are a ratio of 5:3 (e.g., 5 in wide and 3 in high)
- Place figures in the `figures/extra/` folder
- Each "type" of extra figure must be listed in `extra_page_index.xlsx`, along
with the header it will receive and the order (i.e. if there are more than one
type of extra page figures which should come first?). You must also provide a
`match` which is text used to match the type of figure to text in the figure
name (e.g., for type `water_budget` add a match of `Water Budget`. This will
match the name "Aquifer 133 Water Budget.png"

**Example of content in `extra_page_index.xlsx`**  

Contains type (`type`) of content, the heading it should get in the report (`heading`), 
and the order in which it should appear (`order`; i.e. here, if a factsheet 
has both cross sections and water budgets, cross sections will appear first). 

> NOTE: 
> - `match` can contain more than one pattern, separated by comma (,)
> - match is case insensitive

type          | heading       | order  | match
------------- | ------------- | ------ | ------
cross_section | Cross-Section | 1      | x-section, Xsection
water_budget  | Water Budget  | 2      | Water Budget



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


#### 4. Piper plots

Piper plots are now being created as part of the R workflow, using the `rems2aquachem` package.
This package uses `rems` to download EMS data for the wells in question.
Where EMS charge balances do not exist, we calculate charge balances. 

Piper plots are created for any well with at least **one** 'good' sample (charge balance <= 10).
Note that this does not mean that every anion and cation is presentT, but that
even in their absence there is an acceptable charge balance. 


## Troubleshooting
- Sometimes you may need to have a larger temp director when downloading/unzipping
 the EMS data. You can change where R creates the temp folder by 
 specifying `TMPDIR=/TEMP/FOLDER/PATH` in your .Renviron, but make sure the 
 folder exists!
 
- `R non-conforming drawing primitive definition` and `cache resources exhausted ... error/cache.c/OpenPixelCache/4095` are potential errors related to piper plots
  - Try [increasing the amount of disk space available to the program](https://stackoverflow.com/a/53699200)


