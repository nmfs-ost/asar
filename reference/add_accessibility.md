# Add Accessibility to .tex documents

Altering latex file of report to increase accessibility of the document.

## Usage

``` r
add_accessibility(
  x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
  dir = getwd(),
  alttext_csv = file.path(getwd(), "captions_alt_text.csv"),
  compile = TRUE,
  rename = NULL
)
```

## Arguments

- x:

  .tex file containing report. Typically produced after initially
  rendering the skeleton made from create_template.

- dir:

  directory where the tex file is located that will be edited

- alttext_csv:

  File path for the csv file containing alternative text and captions
  generated when running stockplotr::exp_all_figs_tables

- compile:

  Indicate whether the document (X) should be rendered after these files
  are changed. Default TRUE.

- rename:

  Indicate a name for the new tex file produced from this function.
  There is no need to include ".tex" in the name. Defaults to current
  name and overwrites the current tex file.

## Value

This function runs all functions from `asar` associated with
accessibility and renders the final document. The document is tagged and
includes alternative text from the captions_alt_text.csv produced from
`stockplotr` package also available on GitHub.

## Examples

``` r
if (FALSE) { # \dontrun{
create_template(
  new_template = TRUE,
  format = "pdf",
  office = "NWFSC",
  region = "U.S. West Coast",
  species = "Dover sole",
  spp_latin = "Microstomus pacificus",
  year = 2010,
  author = c("John Snow" = "AFSC", "Danny Phantom" = "NWFSC", "Patrick Star" = "SEFSC-ML"),
  model_results = output,
  model = "SS3",
  new_section = "an_additional_section",
  section_location = "after-introduction"
)

quarto::quarto_render(file.path(getwd(), "report", "SAR_USWC_Dover_sole_skeleton.qmd"))

add_accessibility(
  x = "SAR_USWC_Dover_sole_skeleton.tex",
  dir = file.path(getwd(), "report"),
  alttext_csv = file.path(getwd(), "captions_alt_text.csv"),
  compile = TRUE
)
} # }
```
