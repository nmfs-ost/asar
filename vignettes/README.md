# Vignettes

The `articles` sub-folder contains the articles (a special type of vignette) found in the [asar website](https://nmfs-ost.github.io/asar/) under the 
"Articles" drop-down tab.

Note that these files are "articles", rather than "vignettes". We chose articles over vignettes because these documents benefited from having features available to Quarto documents (like a floating table of contents) but not simple markdown documents. As a result, these articles appear on the package website but are not included in the user-facing package.

To create a new article, create a new quarto file then add this information as the yaml:

```
title: "My Title"
format:
  html:
    toc: true
    toc-depth: 3
    anchor-sections: true
    other-links:
      - text: stockplotr
        href: https://nmfs-ost.github.io/stockplotr/
knitr:
  opts_chunk:
    collapse: true
    comment: '#>'
bibliography: references.bib
```
  
Afterwards, edit the `_pkgdown.yml` file located in the `pkgdown` folder. The new article can be added under the "articles:" then "menu:" lines. Please include:

* text -- the title that's presented to the site visitor when looking at the drop-down menu
* href -- the path to the vignette file. It should be "articles/{name_of_file}.qmd"

