test_that("cjfas CSL includes stock-assessment report formatting", {
  csl <- paste(
    readLines(
      system.file("resources", "cjfas.csl", package = "asar"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(csl, '<if type="report" variable="genre" match="all">', fixed = TRUE)
  expect_match(
    csl,
    paste(
      c(
        '<if type="report" variable="genre" match="all">',
        '          <group delimiter=". ">',
        '            <text macro="author"/>',
        '            <text macro="issued-year"/>',
        '            <text variable="title"/>',
        '            <text variable="publisher"/>',
        '            <text variable="number-of-pages" suffix=" p."/>',
        '            <text macro="stock-assessment-access"/>'
      ),
      collapse = "\n"
    ),
    fixed = TRUE
  )
  expect_match(csl, '<text value="Accessible at"/>', fixed = TRUE)
  expect_false(
    grepl(
      '<if type="report" variable="genre" match="all">\n        <text variable="publisher"/>',
      csl,
      fixed = TRUE
    )
  )
})
