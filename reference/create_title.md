# Write Stock Assessment Report Title

Write Stock Assessment Report Title

## Usage

``` r
create_title(
  type = "skeleton",
  office = "",
  species = "species",
  spp_latin = NULL,
  region = NULL,
  year = format(Sys.Date(), "%Y"),
  complex = NULL
)
```

## Arguments

- type:

  Type of report to build. Default is "sar" (NOAA Fisheries Stock
  Assessment Report).

- office:

  Regional Fisheries Science Center producing the report (i.e., AFSC,
  NEFSC, NWFSC, PIFSC, SEFSC, SWFSC).

- species:

  Full common name for target species. Split naming with a space and
  capitalize first letter(s). Example: "Dover sole".

- spp_latin:

  Latin name for the target species. Example: "Pomatomus saltatrix".

- region:

  Full name of region in which the species is evaluated (if applicable).
  If the region is not specified for your center or species, do not use
  this variable.

- year:

  Year the assessment is being conducted. Default is the year in which
  the report is rendered.

- complex:

  TRUE/FALSE; Is this a species complex? Default is false.

## Value

Return a string containing a title for a NOAA Fisheries stock assessment
report.

## Examples

``` r
create_title(
  type = "SAR", office = "SEFSC", species = "Red Snapper",
  spp_latin = "Lutjanus campechanus", region = "South Atlantic",
  year = 2024
)
#> [1] "SEDAR XX Assessment Report for Red Snapper (Lutjanus campechanus) in the South Atlantic in 2024"
```
