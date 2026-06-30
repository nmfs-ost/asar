# Example Reports

The following reports have been made using `asar`. Please let us know if
you’d like us to add your own report to this list!

## Example report from README

``` r

# load libraries
library(asar)
library(stockplotr)

# convert SS3 output
output <- stockplotr::convert_output(
  file = system.file("extdata", "Report.sso", package = "asar"),
  model = "ss3",
  save_dir = here::here("readme_output.rda")
)

# make example figure and table
plot_fishing_mortality(output,
  module = "CATCH",
  make_rda = TRUE
)

table_landings(output,
  module = "CATCH",
  make_rda = TRUE
)

# make basic report
create_template(
  format = "pdf",
  office = "NWFSC",
  region = "U.S. West Coast",
  species = "Petrale sole",
  spp_latin = "Eopsetta jordani",
  year = 2025,
  authors = c("Ian G. Taylor" = "NWFSC", "Vladlena Gertseva" = "NWFSC", "Nick Tolimieri" = "NWFSC"),
  include_affiliation = TRUE,
  simple_affiliation = FALSE,
  param_names = c("nf", "sf"),
  param_values = c("North fleet", "South fleet"),
  model_results = here::here("readme_output.rda")
)

# add some text to the executive summary, which includes
# parameters, key quantities, a citation, and a reference
# to a section of the report
example_text <- "\n\nThe stock assessment for `r params$species` was conducted by the `r params$office` in `r params$region`. The terminal spawning biomass for the current assessment year (`r end_year`) was `r SBend`. You can find more information on the results of this assessment in @sec-assmt-results. The guidelines for this report follow those outlined in @Abrams2014."

exec_sum <- readLines(fs::path("report", "01_executive_summary.qmd"))

exec_sum_new <- append(exec_sum, example_text)

writeLines(
  exec_sum_new,
  fs::path("report", "01_executive_summary.qmd")
)

# render report
quarto::quarto_render(fs::path("report", "sar_USWC_Petrale_sole_skeleton.qmd"))
```

## Published reports

- [“Status of the Rougheye and Blackspotted Rockfishes stock off the
  U.S. West Coast in
  2025”](https://www.pcouncil.org/documents/2026/01/status-of-rougheye-and-blackspotted-rockfishes-stock-off-the-u-s-west-coast-in-2025.pdf/)
  by Jason M. Cope, Vladlena Gertseva, R. Claire Rosemond, Alison D.
  Whitman, and Fabio P. Caltabellotta

- [“Status of Quillback Rockfish in U.S. Waters off California in
  2025”](https://www.pcouncil.org/documents/2026/01/status-of-quillback-rockfish-in-u-s-waters-off-california-in-2025.pdf/)
  by Brian J. Langseth\*, Melissa H. Monk\*, and Julia H. Coates
