create_styles_css <- function(
    species = NULL,
    savedir = NULL) {
  # Add species image to title
  spp_name <- sub(" ", "_", species)
  spp_image <- list.files(file.path(find.package("ASAR"), "resources", "spp_img"), pattern = spp_name)
  styles_css <- paste(
    ".quarto-title-block .quarto-title-banner {", "\n",
    "  ", "background-image: ", spp_image, ";", "\n",
    "  ", "background-size: 300px;", "\n",
    "  ", "background-position: left;", "\n",
    "  ", "background-repeat: no-repeat;", "\n",
    "  ", "padding-left: 10px;", "\n",
    "  ", "background-origin: content-box;", "\n",
    "}", "\n"
  )

  utils::capture.output(cat(styles_css), file = paste0(savedir, "/", "styles.css"), append = FALSE)
}
