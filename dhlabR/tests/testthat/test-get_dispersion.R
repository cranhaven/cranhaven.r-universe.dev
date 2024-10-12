# Test if the function returns the expected result for a known input
test_that("get_dispersion returns expected result for known input", {
  urn <- "URN:NBN:no-nb_digibok_2013060406055"
  words <- c("Dracula", "Mina", "Helsing")
  window <- 1000
  pr <- 1000
  expected_result <- data.frame(
    # Add expected data frame structure and values here
    Dracula = c(2, 0, 0, 0, 0, 0),
    Mina = c(29, 19, 11, 21, 16, 20),
    Helsing = c(27, 19, 11, 21, 16, 20)
  )

  result <- get_dispersion(urn, words, window, pr)
  expect_equal(head(result), expected_result)
})
