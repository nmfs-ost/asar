test_that("add_chunk() generates correct R Markdown chunk", {
  # The test checks if the add_chunk() function correctly generates an R Markdown chunk.

  # Use expect_snapshot() to capture and compare the output of the add_chunk() function.
  # This ensures that any changes in the function's output will be detected in future test runs.
  expect_snapshot(cat(add_chunk("plot(cars$speed, cars$distance)")))

  expect_snapshot(cat(add_chunk("plot(cars$speed, cars$distance)", chunk_option = c("echo:true", "warning: false", "eval: true"))))
})
