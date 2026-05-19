# Add tagging structure to latex documents produced from quarto

Add tagging structure to latex documents produced from quarto

## Usage

``` r
add_tagging(
  x = list.files(getwd())[grep("skeleton.tex", list.files(getwd()))],
  dir = getwd(),
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

- compile:

  Indicate whether the document (X) should be rendered after these files
  are changed. Default TRUE.

- rename:

  Indicate a name for the new tex file produced from this function.
  There is no need to include ".tex" in the name. Defaults to current
  name and overwrites the current tex file.

## Value

This function was made to help add in latex packages and content
associated with PDF tagging. Quarto does not allow the user to edit
anything before documentclass, so this function alters the rendered .tex
file. Flextable-based tables will not be tagged as flextable is not
compatible with tagpdf.

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
  authors = c("John Snow" = "AFSC", "Danny Phantom" = "NEFSC", "Patrick Star" = "SEFSC-ML"),
  include_affiliation = TRUE,
  new_section = "an_additional_section",
  section_location = "after-introduction"
)

quarto::quarto_render(file.path(getwd(), "report", "SAR_USWC_Dover_sole_skeleton.qmd"))

add_tagging(
  x = "SAR_USWC_Dover_sole_skeleton.tex",
  dir = file.path(getwd(), "report"),
  compile = TRUE
)
} # }
```
