test_that("points outside the limits are plotted as NA", {
  df <- data.frame(x = c(-1, 1, 2))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, 1, colour = x)) +
    ggplot2::geom_point() +
    scale_duke_continuous(limits = c(-1, 1))

  correct_fill <- c("#00539B", "#E2E6ED", "#666666")
  expect_equal(ggplot2::layer_data(p)$colour, correct_fill)
})
