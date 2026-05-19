# Create Quarto Document of Figures

Create Quarto Document of Figures

## Usage

``` r
create_figures_doc(subdir = getwd(), figures_dir = getwd())
```

## Arguments

- subdir:

  Location of subdirectory storing the assessment report template

- figures_dir:

  The location of the "figures" folder, which contains figures files.

## Value

A quarto document with pre-loaded R chunk that adds the stock assessment
tables from the nmfs-ost/stockplotr R package. The quarto document will
become part of the stock assessment outline.

## Examples

``` r
if (FALSE) { # \dontrun{
create_figures_doc(
  subdir = getwd(),
  figures_dir = here::here()
)
} # }
```
