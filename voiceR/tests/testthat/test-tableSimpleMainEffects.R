test_that("Create Simple Main Effects table for duration by Condition and Dimension", {
  table_simple <- tableSimpleMainEffects(testAudioData, by = c("Condition",
                                                             "Dimension"), measure = "duration")
  expect_true(any(class(table_simple) %in% "kableExtra"))
})
