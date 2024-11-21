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

### 3. Add supplemental data
Some data need to be supplied prior to running the aquifer factsheets.

All of the following should be placed in the **`1_inputs`** folder.

- Aquifer maps in **`maps`** folder (i.e., `1_inputs/figures/maps/Aquifer_Map_0001.pdf`)
- Piper Plot descriptions (`1_inputs/piper_text.xlsx`)
  - The `4_logs/log_problems_DATE.csv` file will contain plots which need new descriptions to be added
- Extra (page 3) figures [See Appendix](#extra-figures)
  - Figures in `1_inputs/extra`
  - Text in `1_inputs/docx` as docx files (Hyperlinks embeded as needed)
- Missing plot figures for Combo, Piper plots and Trend plots in **`na`** folder 
  (i.e., `1_inputs/figures/na/figure_missing_gwl_ppt.png`)  
- Figure explaining the boxplots for the companion document (`1_inputs/figures/boxplot_key.png`)
- BC Gov Logo (`1_inputs/figures/BCID_H_cmyk_rev.pdf`) - [See Appendix](get-bc-government-logo)

### 4. Create factsheets
Open `run.R` and follow the instructions therein to do the following:

- You can specify `draft = TRUE` to create factsheets with the "DRAFT" watermark
- Factsheet pdfs are produced in the `3_factsheets` folder
- Run the code to produce the companion methods document

## To Update Factsheets
- New Aquifer Maps in the `1_inputs/figures/maps/` folder
- New Extra Supplementary information 
  - Images added to the `1_inputs/extra/` folder
  - Text added to the `1_inputs/extra/docx/` folder
- Run targets workflow (`run.R`)
- Check the `4_logs/log_problems_DATE.csv` and fix any real or fixable problems
- Re-run the targets workflow (`run.R`)


## Potential Problems

### Local Team
- File names should remain constant
  - i.e. `Aquifer_0000.pdf`, `Piperplot_0000_OW0000.jpg`

### Potentially out of Team's Control
- Links to resources shouldn't change 
  - Links to static resources
  - Links to Aquifer factsheets and companion document
- Data sources in `R/01_download.R` should remain active
- R and R packages shouldn't change too much in their functionality


## Appendix

#### Extra Figures

Extra Figures are a method for including extra, non-standard pages in the Aquifer Factsheets. 
These pages occur after the normal type 2 page for specific observation wells (weather, aquifer trends, and piperplots).
Each "extra page figure" can take up 1/2 of a page. 

To include extra page figures:

- Place figures in the `figures/extra/` folder
- Each "type" of extra figure must be listed in `R/00_setup.R` under `e_types`.
- Ensure the files are labelled consistently so that the `match` text in `e_types` 
can accurately pick out what type of extra content each file is. 
(e.g., type `water_budget` matches `Water Budget`. This will identify files with the
name "Aquifer 133 Water Budget.png", but not "Aquifer 133 Watershed Budget.png

**Example of content in `e_types`**  

Contains type (`type`) of content, the heading it should get in the report (`heading`), 
and the order in which it should appear (`order`; i.e. here, if a factsheet 
has both cross sections and water budgets, cross sections will appear first). 

> NOTE: 
> - `match` can contain more than one pattern, separated by comma (,)
> - match is case insensitive

type          | heading       | order  | match
------------- | ------------- | ------ | ------
cross_section | Cross-Section | 1      | Cross Section
water_budget  | Water Budget  | 2      | Water Budget



#### Get BC Government logo 

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


#### Piper plots

Piper plots are created as part of the R workflow, using the `bcgwcat` package.
This package uses `rems` to download EMS data for the wells in question.
Where EMS charge balances do not exist, we calculate charge balances. 

Piper plots are created for any well with at least **one** 'good' sample (charge balance <= 10).
Note that this does not mean that every anion and cation is presentt, but that
even in their absence there is an acceptable charge balance. 


## Troubleshooting
- Sometimes you may need to have a larger temp director when downloading/unzipping
 the EMS data. You can change where R creates the temp folder by 
 specifying `TMPDIR=/TEMP/FOLDER/PATH` in your .Renviron, but make sure the 
 folder exists!
 
- `R non-conforming drawing primitive definition` and `cache resources exhausted ... error/cache.c/OpenPixelCache/4095` are potential errors related to piper plots
  - Try [increasing the amount of disk space available to the program](https://stackoverflow.com/a/53699200)


