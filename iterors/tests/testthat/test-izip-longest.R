test_that("i_zip_longest properly iterates through two numeric vectors", {
  it <- i_zip_longest(1:3, 4:6)
  expect_equal(nextOr(it, NA), list(1, 4))
  expect_equal(nextOr(it, NA), list(2, 5))
  expect_equal(nextOr(it, NA), list(3, 6))
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_zip_longest properly iterates through three numeric vectors", {
  it <- i_zip_longest(1:3, 4:6, 7:9)
  expect_equal(nextOr(it, NA), list(1, 4, 7))
  expect_equal(nextOr(it, NA), list(2, 5, 8))
  expect_equal(nextOr(it, NA), list(3, 6, 9))
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_zip_longest properly iterates through three numeric vectors and one has extra values", {
  it <- i_zip_longest(1:3, 4:6, 7:10, fill=NA)
  expect_equal(nextOr(it, NA), list(1, 4, 7))
  expect_equal(nextOr(it, NA), list(2, 5, 8))
  expect_equal(nextOr(it, NA), list(3, 6, 9))
  expect_equal(nextOr(it, NA), list(NA, NA, 10))
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_zip_longest properly iterates through two numeric vectors and a character vector", {
  it <- i_zip_longest(1:3, 4:7, levels(iris$Species), fill="yooks")
  expect_equal(nextOr(it, NA), list(1, 4, "setosa"))
  expect_equal(nextOr(it, NA), list(2, 5, "versicolor"))
  expect_equal(nextOr(it, NA), list(3, 6, "virginica"))
  expect_equal(nextOr(it, NA), list("yooks", 7, "yooks"))
  expect_equal(nextOr(it, NA), NA)
})

test_that("i_zip_longest properly iterates through a numeric vector, a character vector, and a data.frame's columns", {
  it <- i_zip_longest(1:3, levels(iris$Species), iris, fill="zooks")
  expect_equal(nextOr(it, NA), list(1, "setosa", iris[, 1]))
  expect_equal(nextOr(it, NA), list(2, "versicolor", iris[, 2]))
  expect_equal(nextOr(it, NA), list(3, "virginica", iris[, 3]))
  expect_equal(nextOr(it, NA), list("zooks", "zooks", iris[, 4]))
  expect_equal(nextOr(it, NA), list("zooks", "zooks", iris[, 5]))
  expect_equal(nextOr(it, NA), NA)
})
