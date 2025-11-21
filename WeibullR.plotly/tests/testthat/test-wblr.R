test_that("plotly_wblr works with valid wblr object", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj)
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr works with missing optional parameters", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj, showConf = FALSE)
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr stops with invalid wblr object", {
  expect_error(plotly_wblr(list()), "Argument 'wblr_obj' is not of class 'wblr'.")
})

test_that("plotly_wblr stops with invalid susp argument", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  expect_error(plotly_wblr(obj, susp = "invalid"), "Argument 'susp' must be a numeric vector.")
})

test_that("plotly_wblr works with susp argument", {
  failures <- c(30, 49, 82, 90, 96)
  suspensions <- c(100, 150)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj, susp = suspensions)
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr generates plot with confidence intervals", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj, showConf = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr generates plot without confidence intervals", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj, showConf = FALSE)
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr works with different colors", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj, probCol = "red", fitCol = "blue", confCol = "green", intCol = "purple", gridCol = "grey")
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr works with showGrid parameter", {
  failures <- c(30, 49, 82, 90, 96)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')
  p <- plotly_wblr(obj, showGrid = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("plotly_wblr handles subLayouts correctly", {
  failures <- c(30, 49, 82, 90, 96)
  suspensions <- c(100, 150)
  obj <- wblr.conf(wblr.fit(wblr(failures), method.fit = 'mle'), method.conf = 'lrb')

  p1 <- plotly_wblr(obj, showSusp = TRUE, showRes = TRUE)
  expect_s3_class(p1, "plotly")

  p2 <- plotly_wblr(obj, showSusp = TRUE, showRes = FALSE)
  expect_s3_class(p2, "plotly")

  p3 <- plotly_wblr(obj, showSusp = FALSE, showRes = TRUE)
  expect_s3_class(p3, "plotly")

  p4 <- plotly_wblr(obj, showSusp = FALSE, showRes = FALSE)
  expect_s3_class(p4, "plotly")
})

