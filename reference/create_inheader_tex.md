# Create in-header latex document

Create in-header latex document

## Usage

``` r
create_inheader_tex(species = NULL, year = NULL, subdir)
```

## Arguments

- species:

  String. Common species name - used for footer

- year:

  Number. Year assessment is conducted

- subdir:

  Path. Directory where other files will be copied into

## Value

Create an in-header latex document that dynamically changes based on the
species and year along with other factors.
