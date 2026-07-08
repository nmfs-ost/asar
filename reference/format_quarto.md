# Add Formatting Arguments for YAML Header

Add Formatting Arguments for YAML Header

## Usage

``` r
format_quarto(format = "pdf", type = "sar")
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

## Value

This function returns part of a quarto YAML header involved in
formatting the document during rendering.

## Examples

``` r
if (FALSE) { # \dontrun{
format_quarto(format = "pdf")
} # }
```
