# test_that("formatting applied to ggplot", {
#   test_fig <- ggplot2::ggplot(data = cars, ggplot2::aes(x = speed, y = dist)) +
#     ggplot2::geom_point()+
#     ggplot2::theme_classic()
#   test_fxn <- add_theme(
#                 ggplot2::ggplot(data = cars, ggplot2::aes(x = speed, y = dist)) +
#                 ggplot2::geom_point()
#               )
#
#   expect_equal(2 * 2, 4)
# })
#
# test_that("formatting applied to base R plotting", {
#   test_fig <- plot(x = cars$speed, y= cars$dist)
#
#   expect_equal(2 * 2, 4)
# })
#
# test_that("formatting applied to flextable", {
#   test_tab <- flextable::flextable(head(cars))
#
#   expect_equal(2 * 2, 4)
# })
#
# test_that("formatting applied to gt table", {
#   test_tab <- gt::gt(head(cars))
#
#   expect_equal(2 * 2, 4)
# })
#
# test_that("formatting applied to kable table", {
#   test_tab <- kableExtra::kbl(head(cars))
#
#   expect_equal(2 * 2, 4)
# })
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
  
  # Test with an unsupported object
  unsupported_obj <- data.frame(a = 1, b = 2)
  expect_message(add_theme(unsupported_obj), "NOAA formatting cannot be applied to this object.")
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

  # Ensure the color scale is the "default"ocean" nmfspalette scale
  default_nmfs_colors <- nmfspalette::nmfs_palette("oceans")(3)
  plot_colors <- color_scale$palette(3) # Extract applied colors
  expect_equal(plot_colors, default_nmfs_colors) # Check if the colors match

  default_nmfs_colors <- nmfspalette::nmfs_palette("oceans")(6)
  plot_colors <- color_scale$palette(6) # Extract applied colors
  expect_equal(plot_colors, default_nmfs_colors) # Check if the colors match

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
