# Write R Chunk to Add Child Document

Write R Chunk to Add Child Document

## Usage

``` r
add_child(x, label = NULL)
```

## Arguments

- x:

  An additional section to add into the template. Options for additional
  sections are in the 'skeleton' folder. Appropriate files are .qmd
  files and are formatted as such: XX_section.qmd (i.e., not a, b, c...
  subfiles).

- label:

  Description of the child document being added. It should be short- one
  or two words, maximum.

## Value

Formatting R chunk for child document to add section into the
template/skeleton. Utilize the cat() function to implement into readable
text.

## Examples

``` r
add_child("test_quarto.qmd", label = "test_doc")
#> [1] "\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'test_doc'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('test_quarto.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n"
```
