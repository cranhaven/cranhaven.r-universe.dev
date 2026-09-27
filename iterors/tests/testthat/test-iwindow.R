`%is%` <- expect_equal

test_that("i_window functions properly with an iterator", {
  it <- iteror(letters[1:4])
  it_i_window <- i_window(it, 2)
  expect_equal(nextOr(it_i_window, NA), list("a", "b"))
  expect_equal(nextOr(it_i_window, NA), list("b", "c"))
  expect_equal(nextOr(it_i_window, NA), list("c", "d"))
  expect_equal(nextOr(it_i_window, NA), NA)
})

test_that("i_window functions properly with a vector", {
  it_i_window <- i_window(letters[1:5], 2)
  expect_equal(nextOr(it_i_window, NA), list("a", "b"))
  expect_equal(nextOr(it_i_window, NA), list("b", "c"))
  expect_equal(nextOr(it_i_window, NA), list("c", "d"))
  expect_equal(nextOr(it_i_window, NA), list("d", "e"))
  expect_equal(nextOr(it_i_window, NA), NA)
})

test_that("i_window with tails", {
  it_i_window <- i_window(letters[1:5], 2, tail=NULL)
  expect_equal(nextOr(it_i_window, NA), list(NULL, "a"))
  expect_equal(nextOr(it_i_window, NA), list("a", "b"))
  expect_equal(nextOr(it_i_window, NA), list("b", "c"))
  expect_equal(nextOr(it_i_window, NA), list("c", "d"))
  expect_equal(nextOr(it_i_window, NA), list("d", "e"))
  expect_equal(nextOr(it_i_window, NA), list("e", NULL))
  expect_equal(nextOr(it_i_window, NA), NA)
})

test_that("i_window 3-wide functions properly with an iterator", {
  it <- iteror(letters[1:4])
  it_i_window <- i_window(it, 3)
  expect_equal(nextOr(it_i_window, NA), list("a", "b", "c"))
  expect_equal(nextOr(it_i_window, NA), list("b", "c", "d"))
  expect_equal(nextOr(it_i_window, NA), NA)
})

test_that("i_window 3-wide functions properly with a vector", {
  it_i_window <- i_window(letters[1:5], 3)
  expect_equal(nextOr(it_i_window, NA), list("a", "b", "c"))
  expect_equal(nextOr(it_i_window, NA), list("b", "c", "d"))
  expect_equal(nextOr(it_i_window, NA), list("c", "d", "e"))
  expect_equal(nextOr(it_i_window, NA), NA)
})

test_that("window with tails, short sequence", {
  it <- i_window(c("a", "b", "c"), 5, " ")
  nextOr(it, NA) %is% list(" ", " ", " ", " ", "a")
  nextOr(it, NA) %is% list(" ", " ", " ", "a", "b")
  nextOr(it, NA) %is% list(" ", " ", "a", "b", "c")
  nextOr(it, NA) %is% list(" ", "a", "b", "c", " ")
  nextOr(it, NA) %is% list("a", "b", "c", " ", " ")
  nextOr(it, NA) %is% list("b", "c", " ", " ", " ")
  nextOr(it, NA) %is% list("c", " ", " ", " ", " ")
  nextOr(it, NA) %is% NA
})

test_that("window with no tails over short sequence", {
  it <- i_window(c("a", "b", "c"), 5)
  nextOr(it, NA) %is% NA
})
