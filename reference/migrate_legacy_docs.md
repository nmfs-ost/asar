# Detect, rename, and resolve legacy document paths

Detect, rename, and resolve legacy document paths

## Usage

``` r
migrate_legacy_docs(subdir, doc_type, rerender_skeleton = FALSE)
```

## Arguments

- subdir:

  Directory where template files are located.

- doc_type:

  String. Document type.

  Options: "figures", "tables"

- rerender_skeleton:

  Logical indicating if rerendering active.

  Default: FALSE

## Value

A list containing `using_legacy` (logical), `legacy_name`,
`current_name`, and `resolved_name`.
