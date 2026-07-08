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

  A character vector of author names and affiliations. For example, a
  Jane Doe at the NWFSC Seattle, Washington office would have an entry
  of c("Jane Doe"="NWFSC-SWA"). Information on NOAA offices can be found
  with:
  [`asar::affiliation_info`](nmfs-ost.github.io/asar/reference/affiliation_info.md).
  Keys to the office addresses follow the naming convention of: office
  acronym (ex. NWFSC), a hyphen (-), the first initial of the city, and
  then the two-letter abbreviation for the state the office is located
  in. If the city has two or more words (e.g., Panama City), the first
  initial of each word is used in the key (ex. Panama City, Florida =
  PCFL).

  Default: NULL

  Options: See
  [`asar::affiliation_info`](nmfs-ost.github.io/asar/reference/affiliation_info.md).

- title:

  Custom report title superceding the default composed in
  [`asar::create_title()`](nmfs-ost.github.io/asar/reference/create_title.md).
  Example: "Management Track Assessments Spring 2024".

  Default: `[TITLE]`. If species and region are provided, a title will
  be generated based on the report type, species, and region.

- year:

  Year the assessment is conducted.

  Default: the year in which the report is rendered.

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
