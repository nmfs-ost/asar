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
      file = file.path(all_models[i], "Report.sso")
    ))

    # Check that the result has exactly 33 columns
    expect_equal(dim(result)[2], 33)
  }

  # Test saving the output in a global environment
  output <- convert_output(
    file = file.path(all_models[1], "Report.sso")
  )

  expect_equal(dim(output)[2], 33)
})


test_that("convert_output saves model ss3 hake output file", {
  conv_mod_dir <- fs::path("fixtures", "ss3_models_converted", "Hake_2018", "std_output.rda")

  file.remove(fs::path(conv_mod_dir))

  convert_output(
    file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
    save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018", "std_output.rda")
  )

  expect_true(list.files(fs::path("fixtures", "ss3_models_converted", "Hake_2018")) == "std_output.rda")
})

test_that("missing arguments trigger warnings or errors", {
  # TODO: Debug why this doesn't work
  # expect_error(
  #   asar::convert_output(
  #     model = "ss3",
  #     save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018")
  #   ),
  #   "Missing `file`"
  # )

  # TODO: Debug why this doesn't work
  # expect_error(
  #   asar::convert_output(
  #     file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
  #     model = "fake_model",
  #     save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018")
  #   ),
  #   "Missing `model`"
  # )

  expect_error(
    asar::convert_output(
      file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.rdat"),
      model = "ss3",
      save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018")
    ),
    "`file` not found"
  )
})
