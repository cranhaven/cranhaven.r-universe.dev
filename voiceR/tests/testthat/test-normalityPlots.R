test_that("Normality plots generation", {
  plots <- suppressWarnings(normalityPlots(testAudioData))
  expect_true(all(sapply(plots, class)[2,] %in% "ggplot"))
  expect_true(all(names(plots) %in% c("duration", "voice_breaks_percent",
                                      "RMS_env", "mean_loudness", "mean_F0",
                                      "sd_F0", "mean_entropy", "mean_HNR")))
})


