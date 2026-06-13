test_that("Basic comparison plots generation only by Condition", {
  plots <- suppressWarnings(comparisonPlots(testAudioData, by = "Condition"))
  expect_true(all(sapply(plots, class)[2,] %in% "ggplot"))
  expect_true(all(names(plots) %in% c("duration", "voice_breaks_percent",
                                      "RMS_env", "mean_loudness", "mean_F0",
                                      "sd_F0", "mean_entropy", "mean_HNR")))
})


test_that("Basic comparison plots generation by Condition and Dimension", {
  plots <- suppressWarnings(comparisonPlots(testAudioData, by = c("Condition",
                                                                  "Dimension")))
  expect_true(all(sapply(plots, class)[2,] %in% "ggplot"))
  expect_true(all(names(plots) %in% c("duration", "voice_breaks_percent",
                                      "RMS_env", "mean_loudness", "mean_F0",
                                      "sd_F0", "mean_entropy", "mean_HNR")))
})
