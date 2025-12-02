#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.

test_that("qqplot.rga() errors on invalid inputs", {
  # Not an rga object
  expect_error(
    qqplot.rga(list()),
    "'x' must be an object of class 'rga'."
  )

  # Invalid 'main': numeric instead of string
  dummy <- structure(list(), class = "rga")
  expect_error(
    qqplot.rga(dummy, main = 123),
    "'main' must be a single character string."
  )

  # Invalid 'main': character vector of length > 1
  expect_error(
    qqplot.rga(dummy, main = c("a", "b")),
    "'main' must be a single character string."
  )
})

test_that("ppplot.rga() errors on invalid inputs", {
  # Not an rga object
  expect_error(
    ppplot.rga(list()),
    "'x' must be an object of class 'rga'."
  )

  # Invalid 'main': numeric instead of string
  dummy <- structure(list(), class = "rga")
  expect_error(
    ppplot.rga(dummy, main = 999),
    "'main' must be a single character string."
  )

  # Invalid 'main': character vector of length > 1
  expect_error(
    ppplot.rga(dummy, main = c("p", "q")),
    "'main' must be a single character string."
  )
})

# Build a minimal reproducible rga object for testing
make_rga_numeric <- function() {
  times <- c(5, 10, 15, 20, 25)
  failures <- c(1, 2, 1, 3, 2)
  rga(times, failures) # assumes your rga() is available
}

make_rga_matrix <- function() {
  fit <- make_rga_numeric()
  # Overwrite betas and lambdas to matrix/list form to cover branches
  fit$betas <- list(log_times = matrix(
    c(0.9, 0.1),
    nrow = 1, dimnames = list(NULL, c("Est.", "SE"))
  ))
  fit$lambdas <- matrix(
    c(0.002, 0.0005),
    nrow = 1, dimnames = list(NULL, c("Est.", "SE"))
  )
  fit
}

test_that("qqplot.rga runs silently on valid input (numeric params)", {
  fit <- make_rga_numeric()
  expect_silent(qqplot.rga(fit))
  expect_silent(qqplot.rga(fit, main = "Custom QQ", pch = 19, col = "blue"))
})

test_that("qqplot.rga runs silently on valid input (matrix/list params)", {
  fit <- make_rga_matrix()
  expect_silent(qqplot.rga(fit))
})

test_that("ppplot.rga runs silently on valid input (numeric params)", {
  fit <- make_rga_numeric()
  expect_silent(ppplot.rga(fit))
  expect_silent(ppplot.rga(fit, main = "Custom PP", pch = 17, col = "red"))
})

test_that("ppplot.rga runs silently on valid input (matrix/list params)", {
  fit <- make_rga_matrix()
  expect_silent(ppplot.rga(fit))
})
