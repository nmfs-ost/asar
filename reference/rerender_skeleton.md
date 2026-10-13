# Rerender skeleton quarto document

Rerender skeleton quarto document

## Usage

``` r
rerender_skeleton(
  file_dir,
  species = "species",
  spp_latin = NULL,
  office = NULL,
  region = NULL,
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
  custom_sections = NULL,
  new_section = NULL,
  section_location = NULL,
  custom_params = NULL,
  title = "[TITLE]",
  model_results = NULL,
  bib_file = NULL,
  type = "sar",
  spp_image = NULL,
  format = "pdf",
  authors = NULL
)
```

## Arguments

- file_dir:

  Required. Directory where the skeleton file is located. Can include or
  leave out the report folder in the path.

- species:

  String. Common name of target species. Split multi-word names with
  space and capitalize first letter(s). Example: "Dover sole".

  Default: "species"

- spp_latin:

  String. Latin name of target species. Example: "Pomatomus saltatrix".

  Default: NULL

- office:

  String. Regional Fisheries Science Center producing the report.

  Default: NULL

  Options: "AFSC", "NEFSC", "NWFSC", "PIFSC", "SEFSC", "SWFSC"

- region:

  String. Full name of the stock's sub-region, if applicable. If the
  region is not specified for your center or species, leave default.
  Example: "US West Coast".

  Default: NULL

- year:

  Number. Year the assessment is conducted.

  Default: the year in which the report is rendered.

- custom_sections:

  List of existing sections to include in a custom template (rather than
  the default for stock assessments in your region). If adding a new
  section, also use arguments 'new_section' and 'section_location'.

  Default: NULL

  Options: sections within
  `list.files(system.file("templates", "skeleton", package = "asar"))`.
  The name of the section, rather than the name of the file, can be used
  (e.g., 'abstract' rather than '00_abstract.qmd').

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

- custom_params:

  List. Character vector of additional custom parameter names and values
  to include in the skeleton YAML. For example, a parameter "year2" and
  its value "2026" would have an entry of `c("year2" = "2026")`.
  Parameters automatically included: office, region, species (each of
  which are listed as individual parameters for this function, above).

  Default: NULL

- title:

  String. Custom report title superceding the default composed in
  [`asar::create_title()`](nmfs-ost.github.io/asar/reference/create_title.md).
  Example: "Management Track Assessments Spring 2024".

  Default: `[TITLE]`. If species and region are provided, a title will
  be generated based on the report type, species, and region.

- model_results:

  Path. Filepath to the standardized, converted model output .rda file
  generated with
  [`stockplotr::convert_output()`](https://noaa-fisheries-integrated-toolbox.r-universe.dev/stockplotr/reference/convert_output.html),
  relative to the skeleton .qmd file that will be created within the
  'report' folder.

  Default: NULL

- bib_file:

  A character string of the path to a custom `.bib` file, or a logical.
  If a path is provided, the custom file is added to the skeleton and
  copied into the bibliography files folder.

  Default: NULL

- type:

  String. Report template type.

  Default: "sar" (a NOAA standard "Stock Assessment Report")

  Options: "sar" (Stock Assessment Report), "nemt" (Northeast Management
  Track), "pfmc" (Pacific Fishery Management Council), "safe" (Stock
  Assessment and Fishery Evaluation)

- spp_image:

  Path. Filepath to a custom species image to be used on the report
  cover. Supported file extension is .png. If empty, searches `asar`
  resources for a matching species name.

  Default: NULL

- format:

  String. Report rendering format. Note: "docx" is currently unsupported
  and will default to "pdf".

  Default: "pdf"

  Options: "pdf", "html"

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

## Value

Update the "skeleton" file produce after running `create_template`.
Prevents the loss of data in child documents and make easy updates
without prior knowledge of quarto.

## Examples

``` r
if (FALSE) { # \dontrun{
rerender_skeleton(
file_dir = getwd(),
species = "Red Snapper",
office = "SEFSC",
region = "Gulf of America",
year = 2027,
authors = c("Jane Doe" = "SEFSC")
)
} # }
```
