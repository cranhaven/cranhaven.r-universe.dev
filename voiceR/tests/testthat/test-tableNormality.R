test_that("Create normality table - data.frame format", {
  table_norm <- tableNormality(testAudioData, measure = "duration")
  conditions <- unique(testAudioData$Condition)
  expect_true(all(table_norm$Condition %in% c("General", conditions)))
})

test_that("Create normality table - html format", {
  table_norm <- tableNormality(testAudioData, measure = "duration",
                               HTMLTable = TRUE)

  expect_true(any(class(table_norm) %in% "kableExtra"))
})
