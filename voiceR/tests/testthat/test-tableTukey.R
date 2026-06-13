test_that("Create Tukey table for duration by Condition", {
  table_simple <- tableTukey(testAudioData, by = "Condition", measure = "duration")
  expect_true(any(class(table_simple) %in% "kableExtra"))
})
