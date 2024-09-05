
# asar (Automated Stock Assessment Reporting)

<!-- badges: start -->
<!-- badges: end -->

This package is currently in development. For users interested in testing, please see [Testing](#-testing-section) section below. In its current form, this package builds a very simple template, but there are limited features including NOAA Fisheries formatting and included tables and figures.

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
  office = "NWFSC",
  species = "Dover sole",
  spp_latin = "Pomatomus saltatrix",
  year = 2010,
  author = c("John Snow", "Danny Phantom", "Patrick Star"),
  include_affiliation = TRUE,
  parameters = FALSE,
  resdir = "C:/Users/Documents/Example_Files",
  model_results = "Report.sso",
  model = "SS3"
)
```

Note: This is only an example. The parameters in the example do not convey accurate stock assessment information.

## Testing

We encourage users to test `asar` throughout its development. Please use the above example to get a basic understanding on how to create a stock assessment template. **Currently, `asar` is only setup to render to a pdf.** Once the user successfully executes `create_template()`, the template quarto file will open:

![alt text](man/figures/example_pop-up.PNG)

All other associated files will be created in a folder called `stock_assessment_reports` within the user's documents folder. From there, a file system following the user's associated science center > species name > region (if applicable) > year. The user will have to navigate to this folder to find additional files.

![alt text](man/figures/example_file_system.PNG)

This is a modularized template, there is no need to make any edits to the skeleton file. To write the report, user should navigate and open each supporting section quarto document labeled:

-   Executive Summary
-   Introduction
-   Data
-   Modeling Approach
-   Results
-   Projections
-   Discussion
-   Acknowledgments
-   References
-   Tables
-   Figures 

Please leave an issue for any bugs or suggestions to improve the package during testing on the [Issues Page](https://github.com/Schiano-NOAA/ASAR/issues). Please remember that this package is currently in development and we do not project to release version 1.0 until December 2024. Thank you for helping us improve this package!

## Tips

If there you receive an error creating your template due to authorship. Please submit requests to be added to the national archive for assessment scientists using [this issue](https://github.com/nmfs-ost/asar/issues/19). The developers will add you to the list as soon as possible. In the meantime, you can use the argument `create_template(add_author = "First Last")` to add yourself to the template. 

## User Community

This package is intended for use by stock assessment analysts across NOAA Fisheries, but the modularized process and workflow can be used in other applications such as developing other reports for managing organizations or for rendering stock assessment reports in other regions of the world.

Feel free to report any issues with the package to the [GitHub Issue Page](https://github.com/Schiano-NOAA/ASAR/issues) and any questions regarding the package on the [GitHub discussion board](https://github.com/Schiano-NOAA/ASAR/discussions). Before you create an issue, please see the status at the beginning of this page to check if the package is operational or still in early stages of development.


## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal laws. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
