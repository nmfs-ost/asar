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

  Report template type.

  Default: "sar" (a NOAA standard "Stock Assessment Report")

  Options: "sar" (Stock Assessment Report), "nemt" (Northeast Management
  Track), "pfmc" (Pacific Fishery Management Council), "safe" (Stock
  Assessment and Fishery Evaluation)

- office:

  Regional Fisheries Science Center producing the report.

  Default: NULL

  Options: "AFSC", "NEFSC", "NWFSC", "PIFSC", "SEFSC", "SWFSC"

- species:

  Common name of target species. Split multi-word names with space and
  capitalize first letter(s). Example: "Dover sole".

  Default: "species"

- spp_latin:

  Latin name of target species. Example: "Pomatomus saltatrix".

  Default: NULL

- region:

  Full name of the stock's sub-region, if applicable. If the region is
  not specified for your center or species, leave default. Example: "US
  West Coast".

  Default: NULL

- year:

  Year the assessment is conducted.

  Default: the year in which the report is rendered.

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
