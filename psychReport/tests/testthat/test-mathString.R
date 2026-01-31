context("mathString")

test_that("mathString", {

  result <- mathString("2", "1", operation = "+", unit = "ms")
  testthat::expect_equal(result, "3 ms")

  result <- mathString("2 bananas", "1 apple", operation = "-", unit = "ms")
  testthat::expect_equal(result, "1 ms")

  result <- mathString("2 bananas", "1 apple", operation = "-", unit = "mv")
  testthat::expect_equal(result, "1 $\\mu$V")

  result <- mathString("922.2567", "621.2134", operation = "+", numDigits = 0, unit = "ms")
  testthat::expect_equal(result, "1543 ms")

  result <- mathString("9.27", "6.24", operation = "-", numDigits = 2, unit = "%")
  testthat::expect_equal(result, "3.03 \\%")

  result <- mathString("-2", "1", operation = "+", numDigits = 2, unit = "%")
  testthat::expect_equal(result, "-1.00 \\%")

  result <- mathString("-2", "-1", operation = "-", numDigits = 2, unit = "%")
  testthat::expect_equal(result, "-1.00 \\%")

})
