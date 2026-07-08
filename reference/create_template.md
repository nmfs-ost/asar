# Create Stock Assessment Report Template

Generates a set of Quarto files (.qmd) that set up a stock assessment
report with supporting files. Function builds a YAML specific to the
region and utilizes current resources and workflows from different NOAA
Fishery Science Centers. Automates authorship, bibliography, and other
report components.

## Usage

``` r
create_template(
  format = "pdf",
  type = "sar",
  office = NULL,
  region = NULL,
  species = "species",
  spp_latin = NULL,
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
  authors = NULL,
  file_dir = getwd(),
  title = "[TITLE]",
  model_results = NULL,
  tables_dir = getwd(),
  figures_dir = getwd(),
  spp_image = NULL,
  bib_file = "asar_references.bib",
  new_template = TRUE,
  rerender_skeleton = FALSE,
  custom_sections = NULL,
  new_section = NULL,
  section_location = NULL,
  custom_params = NULL,
  ...
)
```

## Arguments

- format:

  Report rendering format. Note: "docx" is currently unsupported and
  will default to "pdf".

  Default: "pdf"

  Options: "pdf", "html"

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

- year:

  Year the assessment is conducted.

  Default: the year in which the report is rendered.

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

- file_dir:

  Directory where report files will be created.

  Default: the working directory
  ([`getwd()`](https://rdrr.io/r/base/getwd.html)).

- title:

  Custom report title superceding the default composed in
  [`asar::create_title()`](nmfs-ost.github.io/asar/reference/create_title.md).
  Example: "Management Track Assessments Spring 2024".

  Default: `[TITLE]`. If species and region are provided, a title will
  be generated based on the report type, species, and region.

- model_results:

  Filepath to the standardized, converted model output .rda file
  generated with
  [`stockplotr::convert_output()`](https://noaa-fisheries-integrated-toolbox.r-universe.dev/stockplotr/reference/convert_output.html),
  relative to the skeleton .qmd file that will be created within the
  'report' folder.

  Default: NULL

- tables_dir:

  The location of the "tables" folder, which contains tables files

  Default: the working directory

- figures_dir:

  The location of the "figures" folder, which contains figures files

  Default: the working directory

- spp_image:

  Filepath to a custom species image to be used on the report cover.
  Supported file extension is .png. If empty, searches `asar` resources
  for a matching species name.

  Default: NULL

- bib_file:

  File path to bibliography file (`.bib`) used for citing references in
  the report

  Default: "asar_references.bib"

- new_template:

  TRUE/FALSE; Create a new template? If true, will pull the last saved
  stock assessment report skeleton.

  Default: FALSE

- rerender_skeleton:

  TRUE/FALSE; Update the skeleton YAML and structure (R parameters,
  preamble, and skeleton sectioning) if relevant or indicated. All files
  in your folder, such as the `.qmd` child docs, will remain as is.

  Default: FALSE

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

  Names of section(s) (e.g., "Special Section") or subsection(s) (e.g.,
  a section within the introduction) that will be added to the document.
  Please make a short list if \>1 section/subsection will be added. The
  template will be created as a quarto document, added into the
  skeleton, and saved for reference.

  Default: NULL

- section_location:

  Where new section(s)/subsection(s) will be added to the skeleton
  template. Please use the notation of 'placement-section'. For example,
  'in-introduction' signifies that the new content would be created as a
  child document and added into the 02_introduction.qmd. To add \>1
  (sub)section, make the location a list corresponding to the order of
  (sub)section names listed in the 'new_section' parameter.

  Default: NULL

- custom_params:

  Character vector of additional custom parameter names and values to
  include in the skeleton YAML. For example, a parameter "year2" and its
  value "2026" would have an entry of `c("year2" = "2026")`. Parameters
  automatically included: office, region, species (each of which are
  listed as individual parameters for this function, above).

  Default: NULL

- ...:

  Additional arguments passed into functions used in create_template
  such as
  [`create_citation()`](nmfs-ost.github.io/asar/reference/create_citation.md)
  or
  [`create_yaml()`](nmfs-ost.github.io/asar/reference/create_yaml.md).

## Value

Path to the created `report/` directory containing files needed to
produce the stock assessment report. Side effects include the creation
of a directory structure, `.qmd` files, and support files (e.g., images,
`.bib`, `.tex`).

## Details

The function creates a `report/` subdirectory within `file_dir`. The
primary file is a "skeleton" Quarto document that calls various sections
as child documents. The skeleton will be named based on arguments
provided to `create_template()`. For instance, in example 2, below, the
filename would be 'sar_Dover_sole_skeleton.qmd'.

The skeleton contains several sections that should require little to no
editing by the user. These sections include: the yaml, Parameters R
chunk, Preamble R chunk, Disclaimer, and Citations.

Report content is called as child documents in this skeleton. Each child
document (e.g., '01_executive_summary.qmd', '02_introduction.qmd')
should be edited separately.

To see report templates included in the base skeleton, run
`list.files(system.file('templates','skeleton', package = 'asar'))`.

For help with editing any of the sections in the skeleton, please see
the cheatsheet, tutorial, and other resources available at
<https://nmfs-ost.github.io/asar/>.

## See also

[`add_authors()`](nmfs-ost.github.io/asar/reference/add_authors.md),
[`add_base_section()`](nmfs-ost.github.io/asar/reference/add_base_section.md),
[`add_child()`](nmfs-ost.github.io/asar/reference/add_child.md),
[`add_chunk()`](nmfs-ost.github.io/asar/reference/add_chunk.md),
[`add_base_section()`](nmfs-ost.github.io/asar/reference/add_base_section.md),
[`add_section()`](nmfs-ost.github.io/asar/reference/add_section.md),
[`create_citation()`](nmfs-ost.github.io/asar/reference/create_citation.md),
[`create_figures_doc()`](nmfs-ost.github.io/asar/reference/create_figures_doc.md),
[`create_tables_doc()`](nmfs-ost.github.io/asar/reference/create_tables_doc.md),
[`create_title()`](nmfs-ost.github.io/asar/reference/create_title.md),
[`create_yaml()`](nmfs-ost.github.io/asar/reference/create_yaml.md),
[`format_quarto()`](nmfs-ost.github.io/asar/reference/format_quarto.md)

## Examples

``` r
if (FALSE) { # \dontrun{
create_template(
  new_section = "a_new_section",
  section_location = "before-introduction"
)


create_template(
  new_template = TRUE,
  format = "pdf",
  office = "NWFSC",
  species = "Dover sole",
  spp_latin = "Microstomus pacificus",
  year = 2010,
  authors = c(
    "John Snow" = "AFSC",
    "Danny Phantom" = "NEFSC",
    "Patrick Star" = "SEFSC-ML"
  ),
  model_results = here::here("folder", "std_output.rda"),
  figures_dir = here::here(),
  tables_dir = here::here("tables_folder_location"),
  new_section = "an_additional_section",
  section_location = "after-introduction"
)

asar::create_template(
  new_template = TRUE,
  format = "pdf",
  office = "PIFSC",
  species = "Striped marlin",
  spp_latin = "Kajikia audax",
  year = 2018,
  authors = c("John Snow" = "AFSC"),
  new_section = c("a_new_section", "another_new_section"),
  section_location = c("before-introduction", "after-introduction"),
  custom_sections = c("executive_summary", "introduction")
)

create_template(
  new_template = TRUE,
  format = "pdf",
  office = "NWFSC",
  region = "my_region",
  species = "Bluefish",
  spp_latin = "Pomatomus saltatrix",
  year = 2010,
  authors = c("John Snow" = "NEFSC", "Danny Phantom" = "SWFSC", "Patrick Star" = "SEFSC-ML"),
  title = "Management Track Assessments Spring 2024",
  custom_params = c("region2" = "North Coast", "year2" = "2026"),
  model_results = here::here("folder", "std_output.rda"),
  new_section = "an_additional_section",
  section_location = "before-discussion",
  type = "sar",
  custom_sections = c("executive_summary", "introduction", "discussion"),
  spp_image = "dir/containing/spp_image"
)
} # }
```
