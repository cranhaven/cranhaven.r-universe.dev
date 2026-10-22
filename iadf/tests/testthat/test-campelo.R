context("campelo frequency")

testiadf <- data.frame("AbcAA01a" = c(1, 1, 0, 0, NA),
                       "AbcAA02a" = c(NA, 1, 1, 1, NA),
                       row.names = as.character(seq(2000, 2004, 1)))
testrwl <- data.frame("AbcAA01a" = c(10, 10, 20, 20, NA),
                      "AbcAA02a" = c(NA, 10, 10, 10, NA),
                      row.names = as.character(seq(2000, 2004, 1)))
testout <- data.frame(class = factor(c("(9.99,15]", "(15,20]"),
                                     levels = c("(9.99,15]", "(15,20]")),
                      freq = c(1, 0),
                      class.mean.rwl = c(10, 20),
                      sample.depth = c(5L, 2L))
class(testout) <- c("data.frame", "campelo.freq")

test_that("campelo_freq behaves as expected", {
  expect_that(campelo_freq(testiadf, testrwl, n = 2), equals(testout))
})

test_that("campelo_freq handles wrong iadf input", {
  expect_that(campelo_freq(NA, testrwl),
              throws_error("iadf has to be a data.frame"))
  expect_that(campelo_freq(NULL, testrwl),
              throws_error("iadf has to be a data.frame"))
  expect_that(campelo_freq(1:3, testrwl),
              throws_error("iadf has to be a data.frame"))
  expect_that(campelo_freq(as.factor(1:3), testrwl),
              throws_error("iadf has to be a data.frame"))
})

test_that("campelo_freq handles wrong rwl input", {
  expect_that(campelo_freq(testiadf, NA),
              throws_error("rwl has to be a data.frame"))
  expect_that(campelo_freq(testiadf, NULL),
              throws_error("rwl has to be a data.frame"))
  expect_that(campelo_freq(testiadf, 1:3),
              throws_error("rwl has to be a data.frame"))
  expect_that(campelo_freq(testiadf, as.factor(1:3)),
              throws_error("rwl has to be a data.frame"))
})

context("campelo chapman")

test_that("campelo_chapman behaves as expected", {
  expect_that(class(campelo_chapman(campelo_freq(example_iadf, example_rwl),
                                    make.plot = FALSE)),
              equals("nls"))
})

test_that("campelo_chapman handles wrong input", {
  expect_that(campelo_chapman(NA),
              throws_error("input must be derived from campelo_freq()"))
  expect_that(campelo_chapman(NULL),
              throws_error("input must be derived from campelo_freq()"))
  expect_that(campelo_chapman(1:3),
              throws_error("input must be derived from campelo_freq()"))
  expect_that(campelo_chapman(as.factor(1:3)),
              throws_error("input must be derived from campelo_freq()"))
})
