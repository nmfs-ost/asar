# Add New Section or Subsection to Template

Add New Section or Subsection to Template

## Usage

``` r
add_section(
  subdir = NULL,
  custom_sections = NULL,
  new_section = NULL,
  section_location = NULL
)
```

## Arguments

- subdir:

  Directory where the new sections will be saved. In the create_template
  function, this defaults to the location where the template is saved.

- custom_sections:

  List of existing sections to include in the custom template. Note:
  this only includes sections within list.files(system.file("templates",
  "skeleton", package = "asar")). The name of the section, rather than
  the name of the file, can be used (e.g., 'abstract' rather than
  '00_abstract.qmd'). If adding a new section, also use parameters
  'new_section' and 'section_location'.

- new_section:

  Names of section(s) (e.g., introduction, results) or subsection(s)
  (e.g., a section within the introduction) that will be added to the
  document. Please make a short list if \>1 section/subsection will be
  added. The template will be created as a quarto document, added into
  the skeleton, and saved for reference.

- section_location:

  Where new section(s)/subsection(s) will be added to the skeleton
  template. Please use the notation of 'placement-section'. For example,
  'in-introduction' signifies that the new content would be created as a
  child document and added into the 02_introduction.qmd. To add \>1
  (sub)section, make the location a list corresponding to the order of
  (sub)section names listed in the 'new_section' parameter.

## Value

Add an additional section or subsection to the report template if it is
not already present in the default template. This provides the option to
add it as a section before or after an existing section, or within a
section as a child document. For developers: this function creates a
list of sections that will be added to the skeleton file made from
create_template.

## Examples

``` r
add_section(
  new_section = "Ecosystem Considerations", section_location = "after-discussion",
  custom_sections = c("introduction.qmd", "model.qmd", "results.qmd", "discussion.qmd"),
  subdir = tempdir()
)
#> [1] "introduction.qmd"             "model.qmd"                   
#> [3] "results.qmd"                  "discussion.qmd"              
#> [5] "ecosystem_considerations.qmd"
```
