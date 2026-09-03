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

  .tex The name of the .tex file to modify.

  Default: "...skeleton.qmd"

- dir:

  The directory containing the .tex file.

  Default: the working directory
  ([`getwd()`](https://rdrr.io/r/base/getwd.html)).

- compile:

  Logical. If TRUE, renders the .tex file into a .pdf after
  modifications are complete.

  Default: TRUE

- rename:

  Optional new name for the modified .tex file (exclude the ".tex"
  extension). If NULL, the original file is overwritten.

  Default: NULL

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
