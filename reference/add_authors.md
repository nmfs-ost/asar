# Format authors for skeleton

Format authors for skeleton

## Usage

``` r
add_authors(authors, rerender_skeleton = FALSE, prev_skeleton = NULL)
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

- rerender_skeleton:

  Re-create the "skeleton.qmd" in your outline when changes to the main
  skeleton need to be made. This reproduces the yaml, output (if
  changed), preamble quantities, and restructures your sectioning in the
  skeleton if indicated. All files in your folder will remain as is.

- prev_skeleton:

  A character vector of the previous skeleton file read in through
  [`readLines()`](https://rdrr.io/r/base/readLines.html)

## Value

A list of authors formatted for a yaml in quarto. Viewable by running
the return object inside of cat() for each part of the list.

## Examples

``` r
if (FALSE) { # \dontrun{
add_authors(
  authors = c("Danny Phantom" = "SWFSC-LJCA", "John Snow" = "AFSC-ABL", "Jane Doe" = "NWFSC-SWA"),
  rerender_skeleton = FALSE
)
} # }
```
