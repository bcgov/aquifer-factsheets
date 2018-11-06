<a id="devex-badge" rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Aquifer Factsheets

This repository contains [R](http://www.r-project.org) code that generates aquifer factsheets for British Columbia. 


## Usage

### Data

The data sourced for the analyses is provided under the [Open Government Licence – British Columbia](http://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61).

- Groundwater level monitoring data are downloaded from the 
  [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/57c55f10-cf8e-40bb-aae0-2eff311f1685) via the [`bcgroundwater` R package](https://github.com/bcgov/bcgroundwater)
- Groundwater well attribute data are downloaded directly from the
  [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/e4731a85-ffca-4112-8caf-cb0a96905778)
- Natural Resource Regions used in the summaries are sourced from the [`bcmaps` R package](https://cran.r-project.org/web/packages/bcmaps/index.html)


### Code

There are four core scripts that are required for the analysis, they need to be run in order:

- 01_clean.R
- 02_analysis.R
- 03_visualize.R
- 04_output.R

Most packages used in the analysis can be installed from CRAN using `install.packages()`. You will need to install  [`bcgroundwater`](https://github.com/bcgov/envreportutils) using the `remotes` package:

```r
install.packages("remotes") # If you don't already have it installed

library(remotes)
install_github("bcgov/bcgroundwater")
```
 
An installation of LaTeX (e.g. MiKTex, MacTeX or [`tinytex`](https://cran.r-project.org/web/packages/tinytex/index.html)) is required to render the .Rmd file to PDF with `knitr`.

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
