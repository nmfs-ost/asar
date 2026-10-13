# Call previous assessment report and update

Call previous assessment report and update

## Usage

``` r
update_report(
  file_dir = getwd(),
  previous_file_dir,
  authors = NULL,
  model_results = NULL,
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
  format = "pdf",
  region = NULL,
  custom_sections = TRUE,
  new_section = NULL,
  section_location = NULL,
  figures_dir = getwd(),
  tables_dir = getwd(),
  reset_tables_and_figures = FALSE,
  reset_authors = FALSE
)
```

## Arguments

- file_dir:

  String of the path where the new report folder and files should be
  located. Required.

  Default: The working directory
  ([`getwd()`](https://rdrr.io/r/base/getwd.html))

- previous_file_dir:

  String of the path where the previous report files are located.
  Required.

- authors:

  String. A character vector of author names and affiliations. For
  example, a Jane Doe at the NWFSC Seattle, Washington office would have
  an entry of c("Jane Doe"="NWFSC-SWA"). Information on NOAA offices can
  be found with:
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

- model_results:

  Path. Filepath to the standardized, converted model output .rda file
  generated with
  [`stockplotr::convert_output()`](https://noaa-fisheries-integrated-toolbox.r-universe.dev/stockplotr/reference/convert_output.html),
  relative to the skeleton .qmd file that will be created within the
  'report' folder.

  Default: NULL

- year:

  Number. Year the assessment is conducted.

  Default: the year in which the report is rendered.

- format:

  String. Report rendering format. Note: "docx" is currently unsupported
  and will default to "pdf".

  Default: "pdf"

  Options: "pdf", "html"

- region:

  String. Full name of the stock's sub-region, if applicable. If the
  region is not specified for your center or species, leave default.
  Example: "US West Coast".

  Default: NULL

- custom_sections:

  Logical. Indicate whether the previous report had a customized
  sectioning. If TRUE, the report outline will mimic the previous
  report. If FALSE, the report will include the full standard guidelines
  outline along with any custom sectioning from the previous report.

  Default: FALSE

- new_section:

  String. Names of section(s) (e.g., "Special Section") or subsection(s)
  (e.g., a section within the introduction) that will be added to the
  document. Please make a short list if \>1 section/subsection will be
  added. The template will be created as a quarto document, added into
  the skeleton, and saved for reference.

  Default: NULL

- section_location:

  String. Where new section(s)/subsection(s) will be added to the
  skeleton template. Please use the notation of 'placement-section'. For
  example, 'in-introduction' signifies that the new content would be
  created as a child document and added into the 02_introduction.qmd. To
  add \>1 (sub)section, make the location a list corresponding to the
  order of (sub)section names listed in the 'new_section' parameter.

  Default: NULL

- figures_dir:

  Path. The location of the "figures" folder, which contains figures
  files

  Default: the working directory

- tables_dir:

  Path. The location of the "tables" folder, which contains tables files

  Default: the working directory

- reset_tables_and_figures:

  Logical indicating whether to reset tables and figures Quarto
  documents.

  Default: FALSE

- reset_authors:

  Logical indicating whether to reset authorship for the report. Only
  will work if authors argument is also used.

  Default: FALSE

## Value

Creates a new folder of pre-filled assessment report files for the next
assessment cycle.

## Details

This function is designed so a report made from
[`asar::create_template()`](nmfs-ost.github.io/asar/reference/create_template.md)
can be called and updated with the following:

- year (`year`)

- model results (`model_results`)

- authorship (`authors`)

- region (`region`)

- custom sections (`custom_sections `)

- new sections (`new_section and section_location`)

- figure and table folder locations (`figures_dir and tables_dir`)

- format (`format`)

- check standard structure against current structure (performed
  automatically)

This function performs three major tasks: (1) copies all files from the
old directory to the new one, (2) updates the skeleton according the the
input arguments using the
[`rerender_skeleton()`](nmfs-ost.github.io/asar/reference/rerender_skeleton.md)
function, and (3) if desired, resets your figures and tables documents.

## Examples

``` r
if (FALSE) { # \dontrun{
update_report(
  previous_file_dir = "~/testing/goa_2023/report",
  file_dir = "~/testing/goa_2025",
  model_results = "../new_std_res.rda")
  
update_report(
  previous_file_dir = "report_a",
  file_dir = "report_b",
  year = 2027,
  authors = c("John Doe" = "NEFSC"),
  reset_authors = TRUE
)
} # }
```
