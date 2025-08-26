context("test-gof.statistics.only.R")

test_that("statistics for a pit", {
  x = c(0.67572797, 0.96927020, 0.52890276, 0.70750918, 0.58588569,
        0.05399775, 0.78072214, 0.41405244, 0.78772023, 0.41516433)
  result = gof.statistics.only(x)

  expect_equal(result$AD,0.8532651)
  expect_equal(result$CvM,0.18013217)
  expect_equal(result$Watson,0.095684764)

  expect_output(str(result), "List of 3")
})
