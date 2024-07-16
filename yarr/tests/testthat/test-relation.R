test_that("relations with spaces are read", {
  rel <- read_header("@relation 'test thi\\'s \"and\" that'")
  expect_equal(rel, "test thi\\'s \"and\" that")

  rel <- read_header("@relation \"test thi's \\\"and\\\" that\"")
  expect_equal(rel, "test thi's \\\"and\\\" that")

  rel <- read_header("@relation test-this-and-that")
  expect_equal(rel, "test-this-and-that")
})
test_that("meka parameter is read", {
  rel <- read_header("@relation 'test: -c 2'")
  expect_equal(attr(rel, "c"), 2)
})
