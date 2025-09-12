testthat::test_that("BinaryPlot: plot function works", {
  testthat::skip_if_not_installed("grDevices")
  plot <- BinaryPlot$new()

  summary <- data.frame(c(1, 2), c(2, 3))
  names(summary) <- c("k", "dispersion")

  grDevices::pdf(NULL)

  testthat::expect_equal(c("gg", "ggplot"), class(plot$plot(summary)))
})

testthat::test_that("BinaryPlot: plot function checks parameter type", {

  plot <- BinaryPlot$new()

  testthat::expect_error(plot$plot("wrong"),
                         "[BinaryPlot][FATAL] Summary parameter must be defined as 'data.frame' type. Aborting...",
                         fixed = TRUE)
})
