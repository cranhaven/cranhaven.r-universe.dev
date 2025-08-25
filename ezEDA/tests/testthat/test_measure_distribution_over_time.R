h1 <- round(rnorm(50, 60, 8), 0)
h2 <- round(rnorm(50, 65, 8), 0)
h3 <- round(rnorm(50, 70, 8), 0)
h <- c(h1, h2, h3)
y <- c(rep(1999, 50), rep(2000, 50), rep(2001, 50))
df <- data.frame(height = h, year = y)
p <- measure_distribution_over_time(df, h, year)
test_that("Plot layers match expectations",{
  expect_is(p$layers[[1]], "ggproto")
})

test_that("Plot returns ggplot object",{
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_that(length(names(p$data)), equals(2))
})

test_that("x axis label is h",{
  expect_match(p$labels$x, "h")
})

test_that("y axis is labeled 'count'",{
  expect_match(p$labels$y, "count")
})

test_that("Facet type is correct", {
  expect_match(class(p$facet)[[1]], "FacetWrap")
})



