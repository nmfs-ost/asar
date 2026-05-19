# Add Formatting Arguments for YAML Header

Add Formatting Arguments for YAML Header

## Usage

``` r
format_quarto(format = "pdf", type = "sar")
```

## Arguments

- format:

  Rendering format (pdf, html, or docx).

- type:

  Type of report to build. Default is "sar" (NOAA Fisheries Stock
  Assessment Report).

## Value

This function returns part of a quarto YAML header involved in
formatting the document during rendering.

## Examples

``` r
if (FALSE) { # \dontrun{
format_quarto(format = "pdf")
} # }
```
