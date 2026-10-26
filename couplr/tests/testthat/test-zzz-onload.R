# ==============================================================================
# Tests for zzz.R package initialization
# ==============================================================================

# The zzz.R file contains .onLoad which sets options
# These tests verify the option handling behavior

test_that("couplr.emoji option can be set and retrieved", {
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old))

  options(couplr.emoji = TRUE)
  expect_true(getOption("couplr.emoji"))

  options(couplr.emoji = FALSE)
  expect_false(getOption("couplr.emoji"))
})

test_that("couplr.verbose option can be set and retrieved", {
  old <- getOption("couplr.verbose")
  on.exit(options(couplr.verbose = old))

  options(couplr.verbose = TRUE)
  expect_true(getOption("couplr.verbose"))

  options(couplr.verbose = FALSE)
  expect_false(getOption("couplr.verbose"))
})

test_that("couplr is a proper package", {
  # Check that couplr namespace is loaded
  expect_true("couplr" %in% loadedNamespaces())

  # Check that key functions are exported
  expect_true(exists("assignment", where = asNamespace("couplr")))
  expect_true(exists("match_couples", where = asNamespace("couplr")))
})
