# Add selected sections to outline

Add selected sections to outline

## Usage

``` r
add_base_section(custom_sections = NULL)
```

## Arguments

- custom_sections:

  List of existing sections to include in the custom template. Note:
  this only includes sections within list.files(system.file("templates",
  "skeleton", package = "asar")). The name of the section, rather than
  the name of the file, can be used (e.g., 'abstract' rather than
  '00_abstract.qmd'). If adding a new section, also use parameters
  'new_section' and 'section_location'.

## Value

Call and copy the sections in the package templates to create an outline
for a stock assessment

## Examples

``` r
add_base_section(c("executive summary", "assessment", "results"))
#> [[1]]
#> [1] "01_executive_summary.qmd"
#> 
#> [[2]]
#> [1] "04a_assessment-configuration.qmd"
#> 
#> [[3]]
#> [1] "04b_assessment-results.qmd"
#> 
#> [[4]]
#> [1] "04c_assessment-sensitivity.qmd"
#> 
#> [[5]]
#> [1] "04d_assessment-benchmarks.qmd"
#> 
#> [[6]]
#> [1] "04e_assessment-projections.qmd"
#> 
#> [[7]]
#> [1] "04b_assessment-results.qmd"
#> 
```
