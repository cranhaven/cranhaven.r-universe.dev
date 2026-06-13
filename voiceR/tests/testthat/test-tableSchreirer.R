test_that("Create Schreirer table for duration by Condition and Dimension", {
  table_norm <- tableSchreirer(testAudioData, by = c("Condition", "Dimension"),
                               measure = "duration")

  expect_true(any(class(table_norm) %in% "kableExtra"))
})
