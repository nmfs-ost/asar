# Format authors for skeleton

Format authors for skeleton

## Usage

``` r
add_authors(authors, rerender_skeleton = FALSE, prev_skeleton = NULL)
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

- rerender_skeleton:

  TRUE/FALSE; Update the skeleton YAML and structure (R parameters,
  preamble, and skeleton sectioning) if relevant or indicated. All files
  in your folder, such as the `.qmd` child docs, will remain as is.

  Default: FALSE

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
