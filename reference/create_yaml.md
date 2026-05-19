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
  spp_image = "",
  year = NULL,
  bib_name = NULL,
  bib_file = "asar_references.bib",
  author_list = NULL,
  title = "[TITLE]",
  rerender_skeleton = FALSE,
  prev_skeleton = NULL,
  prev_format = NULL,
  parameters = TRUE,
  param_names = NULL,
  param_values = NULL,
  type = "SAR"
)
```

## Arguments

- format:

  Rendering format (pdf, html, or docx).

- office:

  Regional Fisheries Science Center producing the report (i.e., AFSC,
  NEFSC, NWFSC, PIFSC, SEFSC, SWFSC).

- region:

  Full name of region in which the species is evaluated (if applicable).
  If the region is not specified for your center or species, do not use
  this variable.

- species:

  Full common name for target species. Split naming with a space and
  capitalize first letter(s). Example: "Dover sole".

- spp_latin:

  Latin name for the target species. Example: "Pomatomus saltatrix".

- spp_image:

  File path to the species' image if not using the image included in the
  project's repository.

- year:

  Year the assessment is being conducted. Default is the year in which
  the report is rendered.

- bib_name:

  Name of a bib file being added into the yaml. For example, "asar.bib".

- bib_file:

  File path to a .bib file used for citing references in the report

- author_list:

  A list of strings containing pre-formatted author names and
  affiliations that would be found in the format in a yaml of a quarto
  file when using base R function
  [`cat()`](https://rdrr.io/r/base/cat.html).

- title:

  A custom title that is an alternative to the default title (composed
  in asar::create_title()). Example: "Management Track Assessments
  Spring 2024".

- rerender_skeleton:

  Re-create the "skeleton.qmd" in your outline when changes to the main
  skeleton need to be made. This reproduces the yaml, output (if
  changed), preamble quantities, and restructures your sectioning in the
  skeleton if indicated. All files in your folder will remain as is.

- prev_skeleton:

  Vector of strings containing all the lines of the previous skeleton
  file. File is read in using the function readLines from base R.

- prev_format:

  The format that the previous skeleton was directed to render to.
  Parameter is inherited from create_template.

- parameters:

  TRUE/FALSE; For parameterization of the script. Default is true.

- param_names:

  List of parameter names that will be called in the document.
  Parameters automatically included: office, region, species (each of
  which are listed as individual parameters for this function, above).

- param_values:

  List of values associated with the order of parameter names.
  Parameters automatically included: office, region, species (each of
  which are listed as individual parameters for this function, above).

- type:

  Type of report to build. Default is "sar" (NOAA Fisheries Stock
  Assessment Report).

## Value

Create a string indicating the important formatting pieces for a quarto
file for a stock assessment report.

## Examples
