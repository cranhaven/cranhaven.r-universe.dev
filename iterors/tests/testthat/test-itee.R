test_that("i_tee returns n independent numeric vectors", {
  # Iterate through each of the iterators without any order in mind
  iter_list <- i_tee(1:5, n=3)
  expect_equal(nextOr(iter_list[[1]], NA), 1)
  expect_equal(nextOr(iter_list[[1]], NA), 2)
  expect_equal(nextOr(iter_list[[1]], NA), 3)

  expect_equal(nextOr(iter_list[[2]], NA), 1)
  expect_equal(nextOr(iter_list[[2]], NA), 2)

  expect_equal(nextOr(iter_list[[3]], NA), 1)
  expect_equal(nextOr(iter_list[[3]], NA), 2)

  expect_equal(nextOr(iter_list[[1]], NA), 4)
  expect_equal(nextOr(iter_list[[1]], NA), 5)
  expect_equal(nextOr(iter_list[[1]], NA), NA)

  expect_equal(nextOr(iter_list[[2]], NA), 3)
  expect_equal(nextOr(iter_list[[2]], NA), 4)
  expect_equal(nextOr(iter_list[[2]], NA), 5)
  expect_equal(nextOr(iter_list[[2]], NA), NA)

  expect_equal(nextOr(iter_list[[3]], NA), 3)
  expect_equal(nextOr(iter_list[[3]], NA), 4)
  expect_equal(nextOr(iter_list[[3]], NA), 5)
  expect_equal(nextOr(iter_list[[3]], NA), NA)

  # After the iterators are exhausted, ensure that they are truly exhausted
  expect_equal(nextOr(iter_list[[1]], NA), NA)
  expect_equal(nextOr(iter_list[[2]], NA), NA)
  expect_equal(nextOr(iter_list[[3]], NA), NA)
})

# Based on GitHub Issue #36
test_that("i_tee returns n independent numeric vectors based on n iterators", {
  # Iterate through each of the iterators without any order in mind
  it <- iteror(1:5)
  iter_list <- i_tee(it, n=3)
  expect_equal(nextOr(iter_list[[1]], NA), 1)
  expect_equal(nextOr(iter_list[[1]], NA), 2)
  expect_equal(nextOr(iter_list[[1]], NA), 3)

  expect_equal(nextOr(iter_list[[2]], NA), 1)
  expect_equal(nextOr(iter_list[[2]], NA), 2)

  expect_equal(nextOr(iter_list[[3]], NA), 1)
  expect_equal(nextOr(iter_list[[3]], NA), 2)

  expect_equal(nextOr(iter_list[[1]], NA), 4)
  expect_equal(nextOr(iter_list[[1]], NA), 5)
  expect_equal(nextOr(iter_list[[1]], NA), NA)

  expect_equal(nextOr(iter_list[[2]], NA), 3)
  expect_equal(nextOr(iter_list[[2]], NA), 4)
  expect_equal(nextOr(iter_list[[2]], NA), 5)
  expect_equal(nextOr(iter_list[[2]], NA), NA)

  expect_equal(nextOr(iter_list[[3]], NA), 3)
  expect_equal(nextOr(iter_list[[3]], NA), 4)
  expect_equal(nextOr(iter_list[[3]], NA), 5)
  expect_equal(nextOr(iter_list[[3]], NA), NA)

  # After the iterators are exhausted, ensure that they are truly exhausted
  expect_equal(nextOr(iter_list[[1]], NA), NA)
  expect_equal(nextOr(iter_list[[2]], NA), NA)
  expect_equal(nextOr(iter_list[[3]], NA), NA)
})
