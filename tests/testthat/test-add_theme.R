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
