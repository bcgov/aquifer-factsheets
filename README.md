<a id="devex-badge" rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Aquifer Factsheets

This repository contains [R](http://www.r-project.org) code that generates aquifer factsheets for British Columbia. 

## Usage

### Data

The data sourced for the analyses is provided under the [Open Government Licence – British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61). Data sets used by, but not create in, these scripts is stored in the "./data/" folder.

**Groundwater Trends** (Temporary data, future to be accessed from finalized dataset)  
- analysis_data.RData
- raw_data.RData

**Well Data** (Soon to be accessed from new GWELLS system)  
- WELLS_WELLS.csv
- WELLS_AQUIFER_SUBTYPE_CODES_DATA_TABLE.csv

**Aquifer Data**  
- GW_AQUIFER_ATTRS_DATA_TABLE.csv
- Aquifer Location Description and Regions.csv
- Aquifer_Water_District.csv
- licenced_vol_dec2017.csv

**Weather/Climate**  
- All stations BC_CCN_rainfall_snowfall_precip.csv
- Key_Aquifer_Obs well_EMS ID_climate ID.xlsx

**Miscellaneous/Connections**  
- Hydraulic Connectivity Table.csv
- GW_AQUIFER_WELLS_DATA_TABLE.csv
- piper_text.xlsx
- UVic stress index results.xlsx


### Figures

Some figures are created as part of the analysis, some are created in other software. Additional figures used by, but not created in, these scripts are stored in the "./figures/" folder:  

- ./figures/maps/ - Aquifer maps (i.e., Aquifer_Map_0001.pdf)
- ./figures/piperplots/ - Piper plots (i.e., Piperplot_0001_OW0001.jpg)
- ./figures/na/ - Missing plot figures for Combo, Piper pltos and Trend plots
- ./figures/boxplot_key.png - Figure explaining the boxplots for the companion document
- ./figures/BCID_H_cmyk_rev.pdf - BC Gov Logo

Download BC Government logo and unzip into figures folder (Then convert to pdf by hand!)
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


### Code

There are four core scripts that are required for the analysis, they need to be run in order:

- 01_load.R
- 02_clean.R
- 03_output.R

Most packages used in the analysis can be installed from CRAN using `install.packages()`. You will need to install  [`bcgroundwater`](https://github.com/bcgov/envreportutils) using the `devtools` package:

```r
install.packages("remotes") # If you don't already have it installed

library(remotes)
install_github("bcgov/bcgroundwater")
```
 
An installation of LaTeX (e.g. MiKTex, MacTeX or [`tinytex`](https://cran.r-project.org/web/packages/tinytex/index.html)) is required to render the .Rmd files to PDF with `knitr`.

### Output

These scripts output Aquifer Factsheets and the Aquifer Factsheet Companion document to the "./factsheets/" folder.

- AQ_00001_Aquifer_Factsheet.pdf - Example factsheet for Aquifer #1
- Aquifer Factsheet - Companion Document.pdf - Methodological details for factsheets

### Project Status
This project is under development.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/aquifer-factsheets/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2018 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```

This repository is maintained by the [ENVWater Team](https://github.com/orgs/bcgov/teams/envwater/members) in the [GitHub BCGov Organization](https://github.com/bcgov). 

---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
