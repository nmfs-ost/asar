test_that("add_theme applies NOAA formatting correctly", {

  # Test with a ggplot object
  ggplot_obj <- ggplot2::ggplot(data = cars, ggplot2::aes(x = speed, y = dist)) +
    ggplot2::geom_point()
  result_ggplot <- add_theme(ggplot_obj)

  expect_true(inherits(result_ggplot, "ggplot"))
  expect_true(ggplot2::is.ggplot(result_ggplot))

  # Test with a flextable object
  flex_obj <- flextable::flextable(head(cars))
  result_flextable <- add_theme(flex_obj)

  expect_true(inherits(result_flextable, "flextable"))
  expect_true("flextable" %in% class(result_flextable))

  # Test with a gt table object
  gt_obj <- gt::gt(head(cars))
  result_gt <- add_theme(gt_obj)

  expect_true(inherits(result_gt, "gt_tbl"))

  # Test with a kableExtra object
  kable_obj <- knitr::kable(head(cars), format = "html") %>%
    kableExtra::kable_styling()
  result_kable <- add_theme(kable_obj)

  expect_true("kableExtra" %in% class(result_kable))

  # Test with unsupported objects
  unsupported_obj <- data.frame(a = 1, b = 2)
  expect_message(add_theme(unsupported_obj), "NOAA formatting cannot be applied to this object.")

  unsupported_obj2 <- list(1,3,4)
  expect_message(add_theme(unsupported_obj2), "NOAA formatting cannot be applied to this object.")
})

test_that("nmfspalette returns correct scales", {

  # Test 1: Ensure nmfspalette integrates with ggplot color scales
  ggplot_obj <- ggplot2::ggplot(data = cars, ggplot2::aes(x = speed, y = dist, color = speed)) +
    ggplot2::geom_point() +
    nmfspalette::scale_color_nmfs()

  # Check if the correct scale is added
  expect_true("ScaleDiscrete" %in% class(ggplot_obj$scales$scales[[1]]))
  expect_true(ggplot_obj$scales$scales[[1]]$aesthetics == "colour")

  # Test 2: Ensure fill scale works with ggplot
  ggplot_fill_obj <- ggplot2::ggplot(data = cars, ggplot2::aes(x = factor(speed), fill = factor(speed))) +
    ggplot2::geom_bar() +
    nmfspalette::scale_fill_nmfs()

  # Check if the correct fill scale is applied
  expect_true("ScaleDiscrete" %in% class(ggplot_fill_obj$scales$scales[[1]]))
  expect_true(ggplot_fill_obj$scales$scales[[1]]$aesthetics == "fill")

})

test_that("nmfspalette scales are applied to ggplot object", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl))) +
    geom_point()
  formatted_plot <- add_theme(p)

  # Extract the color and fill scales from the plot
  scales <- formatted_plot$scales$scales

  # Check if the color scale is from nmfspalette
  color_scale <- scales[[1]]
  expect_true("ScaleDiscrete" %in% class(color_scale)) # It's a discrete scale
  expect_true(grepl("nmfs", deparse(color_scale$call))) # Check if nmfspalette scale is used

  # Ensure the color scale is the default "oceans" nmfspalette scale
  test_plot <- ggplot(Orange,
                        aes(
                          x = age,
                          y = circumference,
                          color = Tree)) +
    geom_smooth(se = F,
                method = "loess",
                formula = "y ~ x")

  ## use default color palette
  plot_default <- test_plot +
    nmfspalette::scale_color_nmfs()

  ## use oceans color palette
  plot_oceans <- test_plot +
    nmfspalette::scale_color_nmfs("oceans", 5)

  ## extract colors used for both plots
  plot_default_build <- ggplot_build(plot_default)
  colors_default <- unique(plot_default_build$data[[1]]["colour"])

  plot_oceans_build <- ggplot_build(plot_oceans)
  colors_oceans <- unique(plot_oceans_build$data[[1]]["colour"])

  ## test if plots' colors are identical
  expect_equal(colors_default, colors_oceans) # Check if the colors match

})

# Test for both color and fill scales
test_that("nmfspalette scales apply both color and fill scales", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl), fill = as.factor(cyl))) +
    geom_point()
  formatted_plot <- add_theme(p)

  # Extract the scales from the plot
  scales <- formatted_plot$scales$scales

  # Check color scale
  color_scale <- scales[[1]]
  expect_true(grepl("nmfs", deparse(color_scale$call))) # Color scale from nmfspalette

  # Check fill scale
  fill_scale <- scales[[2]]
  expect_true(grepl("nmfs", deparse(fill_scale$call))) # Fill scale from nmfspalette
})

