test_that("errors", {
  expect_error(make_id(LETTERS), "n must be numeric")
})

test_that("default", {
  ids <- make_id()
  
  expect_equal(length(ids), 100)
  expect_equal(ids[1], "S001")
  expect_equal(ids[100], "S100")
})

test_that("unnamed arguments", {
  ids <- make_id(10, "P_", 6, "!")
  
  expect_equal(length(ids), 10)
  expect_equal(ids[1], "P_000001!")
  expect_equal(ids[10], "P_000010!")
})

test_that("named arguments", {
  ids <- make_id(digits = 6, n = 10, suffix = "!", prefix = "P_")
  
  expect_equal(length(ids), 10)
  expect_equal(ids[1], "P_000001!")
  expect_equal(ids[10], "P_000010!")
})

test_that("vector n", {
  ids <- make_id(c(1:10, 21:30))
  
  expect_equal(length(ids), 20)
  expect_equal(ids[1], "S01")
  expect_equal(ids[11], "S21")
  expect_equal(ids[20], "S30")
})

test_that("decimals", {
  ids <- make_id(seq(1,2,.1))
  
  expect_equal(length(ids), 11)
  expect_equal(ids[1], "S1.0")
  expect_equal(ids[11], "S2.0")
})
