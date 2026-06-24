library(testthat)
library(ggplot2)
library(dplyr)
library(lubridate)

# Create a sample dataset for testing
sample_data <- data.frame(
  review_date = c("May 1, 2022", "June 15, 2022", "July 30, 2022", "August 10, 2022", "September 5, 2022"),
  sentiment_score = c(-2, 0, 1, 3, 2)
)

test_that("sentiment_histogram function works correctly", {
  # Test sentiment_histogram function
  hist_plot <- sentiment_histogram(sample_data)

  expect_s3_class(hist_plot, "ggplot")
  expect_equal(hist_plot$labels$x, "Sentiment Score")
  expect_equal(hist_plot$labels$y, "Count")
  expect_equal(hist_plot$labels$title, "Distribution of Sentiment Scores")

  # Check if geom_histogram is used
  expect_true(any(sapply(hist_plot$layers, function(l) "GeomBar" %in% class(l$geom))))
})

test_that("sentiment_trend function works correctly", {
  # Test sentiment_trend function with show_smooth_trend = FALSE
  trend_plot <- sentiment_trend(sample_data, time_period = "month", show_smooth_trend = FALSE)

  expect_s3_class(trend_plot, "ggplot")
  expect_equal(trend_plot$labels$x, "Time")
  expect_equal(trend_plot$labels$y, "Average Sentiment Score")
  expect_equal(trend_plot$labels$title, "Sentiment Trend by month")

  # Check if geom_line and geom_point are used
  expect_true(any(sapply(trend_plot$layers, function(l) "GeomLine" %in% class(l$geom))))
  expect_true(any(sapply(trend_plot$layers, function(l) "GeomPoint" %in% class(l$geom))))

  # Test with show_smooth_trend = TRUE
  trend_plot_smooth <- sentiment_trend(sample_data, time_period = "month", show_smooth_trend = TRUE)

  # Check if geom_smooth is added
  expect_true(any(sapply(trend_plot_smooth$layers, function(l) "GeomSmooth" %in% class(l$geom))))
})

test_that("sentiment_trend function handles different time periods", {
  time_periods <- c("day", "week", "month", "year")

  for (period in time_periods) {
    plot <- sentiment_trend(sample_data, time_period = period)
    expect_s3_class(plot, "ggplot")
    expect_equal(plot$labels$title, paste("Sentiment Trend by", period))
  }
})
