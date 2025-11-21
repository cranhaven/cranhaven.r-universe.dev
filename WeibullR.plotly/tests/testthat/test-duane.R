# Create a mock 'duane' class object for testing
mock_duane_obj <- list(
  Cumulative_Time = c(100, 200, 300, 400, 500),
  Cumulative_MTBF = c(10, 20, 30, 40, 50),
  Fitted_Values = c(12, 22, 32, 42, 52)
)
class(mock_duane_obj) <- "duane"

# Unit tests
test_that("plotly_duane throws an error for incorrect input class", {
  incorrect_obj <- list(Cumulative_Time = 1:5, Cumulative_MTBF = 1:5)
  expect_error(plotly_duane(incorrect_obj), "Argument 'duane_obj' is not of class 'duane'.")
})

test_that("plotly_duane returns a plotly object", {
  plot <- plotly_duane(mock_duane_obj)
  expect_s3_class(plot, "plotly")
})

test_that("plotly_duane works with default parameters", {
  plot <- plotly_duane(mock_duane_obj)
  expect_true(!is.null(plot))
  expect_s3_class(plot, "plotly")
})

test_that("plotly_duane works with custom color parameters", {
  plot <- plotly_duane(mock_duane_obj, pointCol = "red", fitCol = "blue", gridCol = "green")
  expect_s3_class(plot, "plotly")
})

test_that("plotly_duane includes hover text for data points", {
  plot <- plotly_duane(mock_duane_obj)
  hover_data <- plot$x$data[[1]]$text
  expect_true(all(grepl("MTBF: \\(", hover_data)))
})

