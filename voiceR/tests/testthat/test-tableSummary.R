test_that("Create Summary table for all vocal measures by Condition and Dimension", {
  table_summary <- tableSummary(testAudioData, by = c("Condition", "Dimension"),
                               measures = c("duration", "voice_breaks_percent",
                                            "RMS_env", "mean_loudness",
                               "mean_F0"))
  expect_true(any(class(table_summary) %in% "kableExtra"))
})
