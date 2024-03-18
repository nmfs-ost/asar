
# ASAR (Automated Stock Assessment Reporting)

<!-- badges: start -->
<!-- badges: end -->

This package is currently in development and does not yet produce a workable template for stock assessments. This page will be updated once a working package has begun.

The goal of ASAR is to automate stock assessment reports for NOAA science centers so they are reproducible and cohesive across the nation. This project intends to create a streamlined workflow that allows the analyst to create a customized report tailored to their needs and requirements by the SSC, council, or other regional management organizations. 

## User Community

This package is intended for use by stock assessment analysts across NOAA Fisheries, but the modularized process and workflow can be used in other applications such as developing other reports for managing organizations or for rendering stock assessment reports in other regions of the world.

Feel free to report any issues with the package to the [GitHub Issue Page](https://github.com/Schiano-NOAA/ASAR/issues) and any questions regarding the package on the [GitHub discussion board](https://github.com/Schiano-NOAA/ASAR/discussions). Before you create an issue, please see the status at the beginning of this page to check if the package is operational or still in early stages of development.

## Installation

You can install the development version of ASAR like so:

``` r
remotes::intall_github("Schiano-NOAA/ASAR")
```

## Example

The following is a basic example to render a stock assessment report for a particular region in the U.S.:

``` r
library(ASAR)
outdat = "exampledata.Rdata"
mod = "wham"

data <- ASAR::convert_output(output.file = outdat, model = mod)

ASAR::create_template(
    new_template = TRUE,
    format = "pdf",
    office = "NEFSC",
    region = "GB",
    species = "Bluefish",
    year = 2024,
    author = c("John Snow", "Danny Phantom", "Patrick Star"),
    include_affiliation = TRUE,
    parameters = TRUE,
    param_names = c("fleet1", "fleet2","species"),
    param_values = c("Recreational", "Commercial", "Bluefish"),
    type = "RT"
)
```
## Tips


## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal laws. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
