test_that("Apply ifilterfalse to an integer sequence", {
  is_even <- function(x) {
    x %% 2 == 0
  }
  it <- i_drop(1:10, is_even)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 7)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply ifilterfalse to an integer sequence using anonymous function", {
  it <- i_drop(1:10, function(x) x %% 2 == 1)

  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), 8)
  expect_equal(nextOr(it, NA), 10)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply i_drop to a character vector", {
  is_vowel <- function(x) {
    x %in% c('a', 'e', 'i', 'o', 'u')
  }
  it <- i_drop(letters, is_vowel)

  expect_equal(nextOr(it, NA), 'b')
  expect_equal(nextOr(it, NA), 'c')
  expect_equal(nextOr(it, NA), 'd')
  expect_equal(nextOr(it, NA), 'f')
  expect_equal(nextOr(it, NA), 'g')
  expect_equal(nextOr(it, NA), 'h')
  expect_equal(nextOr(it, NA), 'j')
  expect_equal(nextOr(it, NA), 'k')
  expect_equal(nextOr(it, NA), 'l')
  expect_equal(nextOr(it, NA), 'm')
  expect_equal(nextOr(it, NA), 'n')
  expect_equal(nextOr(it, NA), 'p')
  expect_equal(nextOr(it, NA), 'q')
  expect_equal(nextOr(it, NA), 'r')
  expect_equal(nextOr(it, NA), 's')
  expect_equal(nextOr(it, NA), 't')
  expect_equal(nextOr(it, NA), 'v')
  expect_equal(nextOr(it, NA), 'w')
  expect_equal(nextOr(it, NA), 'x')
  expect_equal(nextOr(it, NA), 'y')
  expect_equal(nextOr(it, NA), 'z')
  expect_equal(nextOr(it, NA), NA)
})
