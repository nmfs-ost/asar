test_that("convert_output works for SS3", {
  # Get a list of directories containing SS3 models
  all_models <- list.dirs(
    file.path(test_path("fixtures", "ss3_models"), "models"),
    full.names = TRUE,
    recursive = FALSE
  )

  # Loop through each model directory and check convert_output function
  for (i in seq_along(all_models)) {
    # Ensure no errors occur while converting SS3 output
    expect_no_error(result <- convert_output(
      output_file = file.path(all_models[i], "Report.sso"),
      model = "ss3"
    ))

    # Check that the result has exactly 31 columns
    expect_equal(dim(result)[2], 36)
  }

  # Test saving the output in a global environment
  output <- convert_output(
    output_file = file.path(all_models[1], "Report.sso"),
    model = "ss3"
  )
  expect_equal(dim(output)[2], 36)
})
