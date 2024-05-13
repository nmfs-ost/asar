
# ASAR (Automated Stock Assessment Reporting)

<!-- badges: start -->
<!-- badges: end -->

This package is currently in development.

Download using the directions below and fill in `create_template.R` function with the desired parameters (follow example below) to create a template quarto document to be rendered to create a stock assessment report.


The goal of ASAR is to automate stock assessment reports for NOAA science centers so they are reproducible and cohesive across the nation. This project intends to create a streamlined workflow that allows the analyst to create a customized report tailored to their needs and requirements by the SSC, council, or other regional management organizations. 

## Installation

First please check to make sure `tinytex` package is installed on your machine. If not please install using the following lines:

```r
install.packages("tinytex")
library(tinytex)

```
Then install the package:

```r
install.packages("remotes")
remotes::install_github("Schiano-NOAA/ASAR")
```

## Example

The following is a basic example to render a stock assessment report for a particular region in the U.S.:

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
  model_results = "results.SS",
  model = "SS"
)
```

Note: This is only an example. The parameters in the example do not convey accurate stock assessment information.

## Tips

## User Community

This package is intended for use by stock assessment analysts across NOAA Fisheries, but the modularized process and workflow can be used in other applications such as developing other reports for managing organizations or for rendering stock assessment reports in other regions of the world.

Feel free to report any issues with the package to the [GitHub Issue Page](https://github.com/Schiano-NOAA/ASAR/issues) and any questions regarding the package on the [GitHub discussion board](https://github.com/Schiano-NOAA/ASAR/discussions). Before you create an issue, please see the status at the beginning of this page to check if the package is operational or still in early stages of development.


## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal laws. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
