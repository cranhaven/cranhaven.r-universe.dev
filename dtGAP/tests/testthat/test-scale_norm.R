test_that("scale_norm percentize returns values in [0, 1]", {
  result <- scale_norm(1:5, "percentize")
  expect_length(result, 5)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("scale_norm normalize maps to [0, 1]", {
  result <- scale_norm(c(2, 4, 6, 8, 10), "normalize")
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
})

test_that("scale_norm scale centers and scales", {
  result <- scale_norm(1:10, "scale")
  expect_equal(mean(result), 0, tolerance = 1e-10)
  expect_equal(sd(result), 1, tolerance = 1e-10)
})

test_that("scale_norm none returns input unchanged", {
  input <- c(3.1, 4.1, 5.9)
  result <- scale_norm(input, "none")
  expect_identical(result, input)
})

test_that("scale_norm rejects invalid trans_type", {
  expect_error(scale_norm(1:5, "invalid"))
})
