local({
  # logLevel works ----
  
  current <- later2:::logLevel()
  expect_true(current %in% c("OFF", "ERROR", "WARN", "INFO", "DEBUG"))

  previous <- later2:::logLevel("DEBUG")
  expect_equal(previous, current)
  expect_equal(later2:::logLevel(), "DEBUG")

  expect_equal(later2:::logLevel(current), "DEBUG")
  expect_equal(later2:::logLevel(), current)
})
