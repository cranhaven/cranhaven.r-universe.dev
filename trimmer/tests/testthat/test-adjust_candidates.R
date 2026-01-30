context("adjust_candidates()")

test_that("Expected results after removal of shallow element", {

  dt <- data.table(i1 = c(1,3), size = c(5,10))
  res <- adjust_candidates(dt, 2)
  expect_is(res, "data.table")
  expect_equal(nrow(res), nrow(dt))
  expect_equal(res$i1[order(res$i1)], 1:2)
  # are elements ordered after size?
  expect_identical(res$size, res$size[order(res$size, decreasing = TRUE)])
  
})

test_that("Expected results after removal of deep element", {
  
  dt <- data.table(i1 = c(1,2,2), 
                   i2 = c(1,2,3), 
                   size = c(1,2,3))
  
  cand <- adjust_candidates(dt, c(2,1))
  expect_is(cand, "data.table")
  expect_equal(nrow(cand), nrow(dt))
  # check if results are as expected.
  expect_equal(nrow(cand[i1 == 2 & i2 == 1 & size == 2, , drop = FALSE]), 1)
  expect_equal(nrow(cand[i1 == 2 & i2 == 2 & size == 3, , drop = FALSE]), 1)
  expect_equal(nrow(cand[i1 == 1 & i1 == 1 & size == 1, , drop = FALSE]), 1)
  expect_identical(cand$size, cand$size[order(cand$size, decreasing = TRUE)])
  
})