test_that("Create a Kruskal table for Duration by Condition", {
  table_kruskal <- suppressWarnings(tableKruskal(testAudioData, by = "Condition", measure = "duration"))
  expect_true(any(class(table_kruskal) %in% "kableExtra"))
})
