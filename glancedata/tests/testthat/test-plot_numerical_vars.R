context("test-plot_numerical_vars")

test_that("histogram mode is a list of length 9", {
  myplot <- plot_numerical_vars(iris, "histogram")
  mymode <- mode(myplot)
  mylength <- length(myplot)
  expect_true(mymode == "list" & mylength == 9)
})

test_that("density is a list of length 9", {
  myplot <- plot_numerical_vars(iris, "density")
  mymode <- mode(myplot)
  mylength <- length(myplot)
  expect_true(mymode == "list" & mylength == 9)
})


test_that("boxplot is a list of length 9", {
  myplot <- plot_numerical_vars(iris, "boxplot")
  mymode <- mode(myplot)
  mylength <- length(myplot)
  expect_true(mymode == "list" & mylength == 9)
})


test_that("violin is a list of length 9", {
  myplot <- plot_numerical_vars(iris, "violin")
  mymode <- mode(myplot)
  mylength <- length(myplot)
  expect_true(mymode == "list" & mylength == 9)
})

test_that("qqplot is a list of length 9", {
  myplot <- plot_numerical_vars(iris, "qqplot")
  mymode <- mode(myplot)
  mylength <- length(myplot)
  expect_true(mymode == "list" & mylength == 9)
})

test_that("pairwise is a list of length 20", {
  myplot <- plot_numerical_vars(iris, "pairwise")
  mymode <- mode(myplot)
  mylength <- length(myplot)
  expect_true(mymode == "list" & mylength == 20)
})


test_that("should explode", {
  expect_error( plot_numerical_vars(iris, "nonexistent"))
})

