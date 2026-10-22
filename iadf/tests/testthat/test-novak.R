context("novak frequency")

testdata <- data.frame("AbcAA01a" = c(1, 1, 0, 0, NA),
                       "AbcAA02a" = c(NA, 1, 1, 1, NA),
                       row.names = as.character(seq(2000, 2004, 1)))
testout <- data.frame(cambial.age = 1:4,
                      freq = c(1.0, 1.0, 0.5, 0.0),
                      sample.depth = c(2, 2, 2, 1))
class(testout) <- c("data.frame", "novak.freq")

test_that("novak_freq behaves as expected", {
  expect_that(novak_freq(testdata), equals(testout))
})

test_that("novak_freq handles wrong input", {
  expect_that(novak_freq(NA),
              throws_error("iadf has to be a data.frame or matrix"))
  expect_that(novak_freq(NULL),
              throws_error("iadf has to be a data.frame or matrix"))
  expect_that(novak_freq(1:3),
              throws_error("iadf has to be a data.frame or matrix"))
  expect_that(novak_freq(as.factor(1:3)),
              throws_error("iadf has to be a data.frame or matrix"))
})


context("novak weibull")

test_that("novak_weibull behaves as expected", {
  expect_that(class(novak_weibull(novak_freq(example_iadf), make.plot = FALSE)),
              equals("nls"))
})

test_that("novak_weibull handles wrong input", {
  expect_that(novak_weibull(NA),
              throws_error("input must be derived from novak_freq()"))
  expect_that(novak_weibull(NULL),
              throws_error("input must be derived from novak_freq()"))
  expect_that(novak_weibull(1:3),
              throws_error("input must be derived from novak_freq()"))
  expect_that(novak_weibull(as.factor(1:3)),
              throws_error("input must be derived from novak_freq()"))
})
