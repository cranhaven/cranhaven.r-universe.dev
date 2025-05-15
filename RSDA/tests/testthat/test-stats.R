data("example3")
context("Stats")
test_that("Mean", {
  expect_true(round(mean(example3$F1), 4) == 1.6286)
  expect_true(mean(example3$F2, method = "centers") == 5)
  expect_equal(mean(example3$F2, method = "interval"), sym.interval(c(1.857143, 8.142857)), tolerance = 3e-8)
})

test_that("Median", {
  expect_true(round(median(example3$F1), 4) == 1.4)
  expect_true(median(example3$F2, method = "centers") == 1.5)
  expect_equal(median(example3$F6, method = "interval"), sym.interval(c(5, 89)), tolerance = 3e-8)
})

test_that("Variance", {
  expect_true(round(var(example3$F1), 4) == 15.9824)
  expect_true(round(var(example3$F2, method = "centers"), 4) == 90.6667)
  expect_equal(var(example3$F6, method = "interval"), RSDA:::new.sym.intreval(min = 2408.966, max = 1670.509), tolerance = 3e-6)
  expect_equal(round(var(example3$F6, method = "billard"), 4), 1355.143, , tolerance = 3e-6)
})

test_that("Standard Deviation", {
  expect_true(round(sd(example3$F1), 4) == 3.9978)
  expect_true(round(sd(example3$F2, method = "centers"), 4) == 6.733)
  expect_equal(sd(example3$F6, method = "interval"), RSDA:::new.sym.intreval(min = 49.08121, max = 40.87186), tolerance = 3e-6)
  expect_equal(round(sd(example3$F6, method = "billard"), 4), 36.8123, tolerance = 3e-6)
})

test_that("Corralation", {
  expect_true(round(cor(example3$F1, example3$F4), 4) == 0.2865)
  expect_true(round(cor(example3$F2, example3$F6), 4) == -0.6694)
  expect_true(round(cor(example3$F2, example3$F6, method = "billard"), 4) == -0.602)
})
