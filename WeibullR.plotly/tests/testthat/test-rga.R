# Create a mock 'rga' class object for testing
mock_rga_obj <- list(
  model = list(model = list(
    log_times = log(c(100, 200, 300, 400, 500)),
    log_cum_failures = log(c(1, 3, 6, 10, 15))
  )),
  fitted_values = c(1.5, 3.5, 6.5, 10.5, 15.5),
  lower_bounds = c(1.2, 3.2, 6.2, 10.2, 15.2),
  upper_bounds = c(1.8, 3.8, 6.8, 10.8, 15.8),
  breakpoints = c(200, 400)
)
class(mock_rga_obj) <- "rga"

# Unit tests
test_that("plotly_rga throws an error for incorrect input class", {
  incorrect_obj <- list(model = list(model = list()))
  expect_error(plotly_rga(incorrect_obj), "Argument 'rga_obj' is not of class 'rga'.")
})

test_that("plotly_rga returns a plotly object", {
  plot <- plotly_rga(mock_rga_obj)
  expect_s3_class(plot, "plotly")
})

test_that("plotly_rga works with default parameters", {
  plot <- plotly_rga(mock_rga_obj)
  expect_true(!is.null(plot))
  expect_s3_class(plot, "plotly")
})

test_that("plotly_rga works with custom color parameters", {
  plot <- plotly_rga(mock_rga_obj, pointCol = "red", fitCol = "blue", confCol = "green", gridCol = "yellow", breakCol = "purple")
  expect_s3_class(plot, "plotly")
})

test_that("plotly_rga respects the showGrid parameter", {
  plot_with_grid <- plotly_rga(mock_rga_obj, showGrid = TRUE)
  plot_without_grid <- plotly_rga(mock_rga_obj, showGrid = FALSE)

  # Check if grid lines are reflected in the plot layout
  expect_equal(plot_with_grid$x$layoutAttrs[[1]]$xaxis$showgrid, TRUE)
  expect_equal(plot_with_grid$x$layoutAttrs[[1]]$yaxis$showgrid, TRUE)
  expect_equal(plot_without_grid$x$layoutAttrs[[1]]$xaxis$showgrid, FALSE)
  expect_equal(plot_without_grid$x$layoutAttrs[[1]]$yaxis$showgrid, FALSE)
})

test_that("plotly_rga includes hover text for data points", {
  plot <- plotly_rga(mock_rga_obj)
  hover_data <- plot$x$data[[1]]$text
  expect_true(all(grepl("Failures: \\(", hover_data)))
})

