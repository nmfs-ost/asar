# Generate Citation for Stock Assessment Report

Generate Citation for Stock Assessment Report

## Usage

``` r
create_citation(
  authors = NULL,
  title = "[TITLE]",
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y")
)
```

## Arguments

- authors:

  A character vector of author names with their accompanying
  affiliations. For example, a Jane Doe at the NWFSC Seattle, Washington
  office would have an entry of c("Jane Doe"="NWFSC-SWA"). Information
  on NOAA offices is found in a database located in the package:
  `system.file("resources", "affiliation_info.csv", package = "asar")`.
  Keys to the office addresses follow the naming convention of the
  office acronym (ex. NWFSC) with a dash followed by the first initial
  of the city then the 2 letter abbreviation for the state the office is
  located in. If the city has 2 or more words such as Panama City, the
  first initial of each word is used in the key (ex. Panama City,
  Florida = PCFL)

- title:

  A custom title that is an alternative to the default title (composed
  in asar::create_title()). Example: "Management Track Assessments
  Spring 2024".

- year:

  Year the assessment is being conducted. Default is the year in which
  the report is rendered.

## Value

Generate a citation for use in publications and other references
associated with the stock assessment report produced with `asar`.

## Examples

``` r
if (FALSE) { # \dontrun{
create_citation(
  title = "SA Report for Jellyfish",
  authors = c("Danny Phantom" = "SWFSC-LJCA", "John Snow" = "AFSC-ABL", "Jane Doe" = "NWFSC-SWA"),
  year = 2024
)
} # }
```
