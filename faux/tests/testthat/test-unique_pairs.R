context("test_unique_pairs")

test_that("number", {
  expect_equal(unique_pairs(2), "A-B")
  expect_equal(unique_pairs(3), c("A-B", "A-C", "B-C"))
  expect_equal(unique_pairs(4), c("A-B", "A-C", "A-D", "B-C", "B-D", "C-D"))
  expect_error(unique_pairs(1), "There must be at least 2 levels")
})

test_that("levels", {
  expect_equal(unique_pairs(c("dog", "cat")), "dog-cat")
  expect_equal(unique_pairs(c("Z", "A", "P")), c("Z-A", "Z-P", "A-P"))
  expect_equal(unique_pairs(4:2), c("4-3", "4-2", "3-2"))
  expect_equal(unique_pairs(c("first", "next", "last")), c("first-next", "first-last", "next-last"))
  expect_equal(unique_pairs(c("a.name", "another.name")), "a.name-another.name")
  expect_error(unique_pairs(c("same", "same")), "You have duplicate levels")
  expect_error(unique_pairs(c("one")), "There must be at least 2 levels")
})
