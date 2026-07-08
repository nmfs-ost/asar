# Create string for yml header in quarto file

Create string for yml header in quarto file

## Usage

``` r
create_yaml(
  format = "pdf",
  office = NULL,
  region = NULL,
  species = "species",
  spp_latin = NULL,
  spp_image = NULL,
  year = NULL,
  bib_name = NULL,
  bib_file = "asar_references.bib",
  author_list = NULL,
  title = "[TITLE]",
  rerender_skeleton = FALSE,
  prev_skeleton = NULL,
  prev_format = NULL,
  parameters = TRUE,
  custom_params = NULL,
  type = "SAR"
)
```

## Arguments

- format:

  Report rendering format. Note: "docx" is currently unsupported and
  will default to "pdf".

  Default: "pdf"

  Options: "pdf", "html"

- office:

  Regional Fisheries Science Center producing the report.

  Default: NULL

  Options: "AFSC", "NEFSC", "NWFSC", "PIFSC", "SEFSC", "SWFSC"

- region:

  Full name of the stock's sub-region, if applicable. If the region is
  not specified for your center or species, leave default. Example: "US
  West Coast".

  Default: NULL

- species:

  Common name of target species. Split multi-word names with space and
  capitalize first letter(s). Example: "Dover sole".

  Default: "species"

- spp_latin:

  Latin name of target species. Example: "Pomatomus saltatrix".

  Default: NULL

- spp_image:

  Filepath to a custom species image to be used on the report cover.
  Supported file extension is .png. If empty, searches `asar` resources
  for a matching species name.

  Default: NULL

- year:

  Year the assessment is conducted.

  Default: the year in which the report is rendered.

- bib_name:

  Name of a bib file being added into the yaml. For example, "asar.bib".

- bib_file:

  File path to bibliography file (`.bib`) used for citing references in
  the report

  Default: "asar_references.bib"

- author_list:

  A list of strings containing pre-formatted author names and
  affiliations that would be found in the format in a yaml of a quarto
  file when using base R function
  [`cat()`](https://rdrr.io/r/base/cat.html).

- title:

  Custom report title superceding the default composed in
  [`asar::create_title()`](nmfs-ost.github.io/asar/reference/create_title.md).
  Example: "Management Track Assessments Spring 2024".

  Default: `[TITLE]`. If species and region are provided, a title will
  be generated based on the report type, species, and region.

- rerender_skeleton:

  TRUE/FALSE; Update the skeleton YAML and structure (R parameters,
  preamble, and skeleton sectioning) if relevant or indicated. All files
  in your folder, such as the `.qmd` child docs, will remain as is.

  Default: FALSE

- prev_skeleton:

  Vector of strings containing all the lines of the previous skeleton
  file. File is read in using the function readLines from base R.

- prev_format:

  The format that the previous skeleton was directed to render to.
  Parameter is inherited from create_template.

- custom_params:

  Character vector of additional custom parameter names and values to
  include in the skeleton YAML. For example, a parameter "year2" and its
  value "2026" would have an entry of `c("year2" = "2026")`. Parameters
  automatically included: office, region, species (each of which are
  listed as individual parameters for this function, above).

  Default: NULL

- type:

  Report template type.

  Default: "sar" (a NOAA standard "Stock Assessment Report")

  Options: "sar" (Stock Assessment Report), "nemt" (Northeast Management
  Track), "pfmc" (Pacific Fishery Management Council), "safe" (Stock
  Assessment and Fishery Evaluation)

## Value

Create a string indicating the important formatting pieces for a quarto
file for a stock assessment report.

## Examples

``` r
if (FALSE) { # \dontrun{
my_author_list <- paste(
  "  - name: 'Patrick Star'",
  "    affiliations:",
  "      - name: 'NOAA Fisheries Southeast Fisheries Science Center'",
  "        address: '75 Virginia Beach Drive'",
  "        city: 'Miami'",
  "        state: 'FL'",
  "        postal-code: '33149'",
  sep = "\n"
)
create_yaml(
  rerender_skeleton = FALSE,
  prev_skeleton = NULL,
  title = "My title",
  author_list = my_author_list,
  author = c("Patrick Star" = "SEFSC"),
  office = "SEFSC",
  add_author = NULL,
  spp_image = NULL,
  species = "",
  spp_latin = NULL,
  region = NULL,
  format = "pdf",
  parameters = TRUE,
  custom_params = NULL,
  bib_file = "path/asar_references.bib",
  bib_name = "asar_references.bib",
  year = 2025
)
} # }
```
