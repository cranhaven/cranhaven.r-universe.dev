test_that("Create a two-way anova table for duration by Condition and Dimension", {
  table_anova <- tableANOVA(testAudioData, by = c("Condition", "Dimension"), measure = "duration")
  expect_true(any(class(table_anova) %in% "kableExtra"))
})
