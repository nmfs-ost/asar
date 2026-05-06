# Write R chunk to template

Write R chunk to template

## Usage

``` r
add_chunk(
  x,
  label = NULL,
  add_option = TRUE,
  chunk_option = c("echo: false", "warnings: false", "eval: true"),
  rmark_option = NULL
)
```

## Arguments

- x:

  Content to be written within the R chunk. Wrap in quotation marks
  ("").

- label:

  The name of the chunk in the 'label:' section of the R code chunk.
  This should be in snakecase (i.e., in which words are written in
  lowercase and connected by underscores).

- add_option:

  TRUE/FALSE; Option to add additional chunk options. Default is false.

- chunk_option:

  List of chunk options to add. For example: c("output: true", "error:
  false)

- rmark_option:

  List of chunk options to add after indicating the language of the
  chunk as used in Rmarkdown.

## Value

Write an additional R chunk into the template using this function. The
code can be written as usual, just remember to put it entirely in quotes
for the function to render it properly

## Examples

``` r
add_chunk("plot(cars$speed, cars$distance)")
#> [1] "```{r} \n#| echo: false \n#| warnings: false \n#| eval: true\nplot(cars$speed, cars$distance)\n``` \n"
```
