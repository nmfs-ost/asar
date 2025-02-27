create_yaml <- function(
    title = NULL,
    author_list = NULL,
    spp_image = NULL,
    species = NULL,
    spp_latin = NULL,
    region = NULL,
    format = "pdf",
    param_names = NULL,
    param_values = NULL,
    bib_name = NULL
    ){
  # Creating YAML
  yaml <- paste0(
    # start YAML notation
    "---", "\n",

    # Tile
    "title: ", "'", title, "'", "\n",

    # Author
    "author:", "\n"
  )
  # Add authors
  add_authors <- NULL
  for (i in 1:length(author_list)) {
    toad <- paste(author_list[[i]], sep = ",")
    add_authors <- paste0(add_authors, toad) # -> add_authors
  }
  yaml <- paste0(yaml, add_authors)

  # Add other parts
  yaml <- paste0(
    yaml,
    # Date
    "date: today", "\n",
    "lang: en \n",
    "keep-tex: true \n"
  )

  # Add species image on title page
  if (add_image) {
    # extract image name
    new_img <- sapply(strsplit(spp_image, "/"), utils::tail, 1)
    yaml <- paste0(
      yaml,
      # image as pulled in from above
      "cover: support_files/", new_img, "\n"
    )
  } else if (spp_image == "") {
    yaml <- paste0(
      yaml,
      # image as pulled in from above
      "cover: ", spp_image, "\n"
    )
  } else {
    yaml <- paste0(
      yaml,
      # image as pulled in from above
      "cover: support_files/", gsub(" ", "_", species), ".png", "\n"
    )
  }

  # Add lualatex engine
  if (format == "pdf") {
    yaml <- paste0(
      yaml,
      "pdf-engine: lualatex", "\n"
    )
  }

  # Formatting
  yaml <- paste0(
    yaml,
    format_quarto(format = format),
    # Add in output file name (Rendered name of pdf)
    "output-file: '", stringr::str_replace_all(species, " ", "_"), ifelse(is.null(species), "SAR_", "_SAR_"), year, "'", " \n"
  )

  # Add lua filters for compliance
  # PLACEHOLDER: Uncomment once .lua text is built

  # yaml <- paste0(yaml,
  #                # "contributes:", "\n",
  #                "filters:", "\n",
  #                "  ", "  ", "- acronyms.lua", "\n",
  #                "  ", "  ", "- accessibility.lua", "\n")

  # Parameters
  # office, region, and species are default parameters
  if (parameters) {
    yaml <- paste0(
      yaml, "params:", "\n",
      "  ", " ", "office: ", "'", office, "'", "\n",
      "  ", " ", "species: ", "'", species, "'", "\n",
      "  ", " ", "spp_latin: ", "'", spp_latin, "'", "\n"
    )
    if (!is.null(region)) {
      yaml <- paste0(yaml, "  ", " ", "region: ", "'", region, "'", "\n")
    }
    # Add more parameters if indicated
    if (!is.null(param_names) & !is.null(param_values)) {
      # check there are the same number of names and values
      if (length(param_names) != length(param_values)) {
        print("Please define ALL parameter names (param_names) and values (param_values).")
      } else {
        add_params <- NULL
        for (i in 1:length(param_names)) {
          toad <- paste("  ", " ", param_names[i], ": ", "'", param_values[i], "'", "\n", sep = "")
          add_params <- paste0(add_params, toad)
        } # close loop
      } # close check
      yaml <- paste0(yaml, add_params)
    } # close if adding add'l params
  } # close if params to be included in template

  # Add style guide
  # create_style_css(species = species, savedir = subdir)

  # yaml <- paste0(
  #   yaml,
  #   "css: styles.css", "\n"
  # )

  # Add option for bib file
  # if (!is.null(bib_file)) {
  bib <- glue::glue(
    "bibliography: ", "\n"
  )
  bib_all <- paste("  ", "- ", bib_name, "\n", collapse = "")
  bib <- glue::glue(
    bib, "\n",
    bib_all, "\n"
  )
  yaml <- paste0(yaml, bib)
  # }
  # add in else statement once a national .bib file is made

  # Close yaml
  yaml <- paste0(yaml, "---")

  print("__________Built YAML Header______________")
}
