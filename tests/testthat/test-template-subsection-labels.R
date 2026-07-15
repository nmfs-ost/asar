test_that("base template subsection headings have section labels", {
  template_dirs <- c("skeleton", "nemt", "pfmc", "safe")

  for (template_dir in template_dirs) {
    path <- system.file("templates", template_dir, package = "asar")
    files <- list.files(path, pattern = "\\.qmd$", full.names = TRUE)

    for (file in files) {
      lines <- readLines(file, warn = FALSE)
      subsection_idx <- grep("^(##|###|####)\\s+", lines)

      if (length(subsection_idx) > 0) {
        subsection_lines <- lines[subsection_idx]
        expect_true(
          all(grepl("\\{#sec-", subsection_lines)),
          info = basename(file)
        )
      }
    }
  }
})
