test_that("Create a Dunn table for Duration by Condition", {
  table_dunn <- suppressWarnings(tableDunn(testAudioData, by = "Condition", measure = "duration"))
  expect_true(any(class(table_dunn) %in% "kableExtra"))
})
