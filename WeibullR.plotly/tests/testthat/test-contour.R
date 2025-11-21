# Create a sample wblr object for testing
failures <- c(30, 49, 82, 90, 96)
wblr_obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')

test_that("plotly_contour runs without errors", {
  expect_silent(plotly_contour(wblr_obj))
})

test_that("plotly_contour returns a plotly object", {
  plot <- plotly_contour(wblr_obj)
  expect_s3_class(plot, "plotly")
})

test_that("plotly_contour handles invalid wblr_obj", {
  invalid_wblr_obj <- list(a = 1, b = 2)
  expect_error(plotly_contour(invalid_wblr_obj), "All inputs must be of class 'wblr'.")
})

test_that("plotly_contour handles wblr_obj without contours", {
  no_contour_wblr_obj <- wblr(failures)
  expect_error(plotly_contour(no_contour_wblr_obj), "Each wblr object must have contours generated using method.conf='lrb'.")
})


