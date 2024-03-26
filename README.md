
# ASAR (Automated Stock Assessment Reporting)

<!-- badges: start -->
<!-- badges: end -->

This package is currently in development. There are two proposed methods to use this tool and build a stock assessment report. 

  1) main: Use this repository as a template and use `create_template.R` to generate a template and create a skeleton from `inst/templates/skeleton` folder dependent on the analyst's region.
  
  2) draft-template:Use this repository as one would a package. Download using the directions below and fill in `create_template.R` function with the desired parameters (follow example below) to create a template quarto document to be rendered to create a stock assessment report.
  
Please refer to examples below on how to utilize both options.

The goal of ASAR is to automate stock assessment reports for NOAA science centers so they are reproducible and cohesive across the nation. This project intends to create a streamlined workflow that allows the analyst to create a customized report tailored to their needs and requirements by the SSC, council, or other regional management organizations. 

## User Community

This package is intended for use by stock assessment analysts across NOAA Fisheries, but the modularized process and workflow can be used in other applications such as developing other reports for managing organizations or for rendering stock assessment reports in other regions of the world.

Feel free to report any issues with the package to the [GitHub Issue Page](https://github.com/Schiano-NOAA/ASAR/issues) and any questions regarding the package on the [GitHub discussion board](https://github.com/Schiano-NOAA/ASAR/discussions). Before you create an issue, please see the status at the beginning of this page to check if the package is operational or still in early stages of development.

## Installation

To install `ASAR` as a template repository:

  (1) Navigate to the main page of the [repository](https://github.com/Schiano-NOAA/ASAR)
  (2) Above the file list, click *Use this template*.
  (3) Select *Create a new repository*.
  
 ![](https://docs.github.com/assets/cb-77734/mw-1440/images/help/repository/use-this-template-button.webp) 
 
To install `ASAR` as a package:

```r
remotes::install_github("Schiano-NOAA/ASAR@draft-template")
```

Here we are installing the branch which is designed as a package.

## Example

The following is a basic example to render a stock assessment report for a particular region in the U.S.:

### Option A

Downloaded as a template repo:

``` r
source(here::here('R', 'create_template.R'))
source(here::here('R', 'write_title.R'))
source(here::here('R', 'paste_child.R'))
source(here::here('R', 'chunkr.R'))
source(here::here('R', 'generate_citation.R'))

create_template(
  new_template = TRUE,
  format = "pdf",
  office = "SEFSC",
  region = "Gulf of Mexico",
  species = "Red Snapper",
  spp_latin = "Lutjanus campechanus",
  year = 2021,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  include_affiliation = TRUE,
  parameters = FALSE,
  type = "RT",
  model_results = "results.SSO",
  model = "SS"
)
```
Note: This is only an example. The parameters in the example do not convey accurate stock assessment information.

### Option B

Download as a package:

```r
ASAR::create_template(
  new_template = TRUE,
  format = "pdf",
  office = "SEFSC",
  region = "Gulf of Mexico",
  species = "Red Snapper",
  spp_latin = "Lutjanus campechanus",
  year = 2024,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  include_affiliation = TRUE,
  parameters = FALSE,
  type = "RT",
  model_results = "results.SSO",
  model = "SS"
)
```

## Tips


## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal laws. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
