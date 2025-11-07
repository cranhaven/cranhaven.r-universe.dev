test_that("filter_with_lists works as expected", {
  df <- dplyr::tibble(
    VIS_Groep = sample(c("Group1", "Group2", "Group3"), 100, replace = TRUE),
    VIS_Groep_naam = sample(c("Name1", "Name2", "Name3"), 100, replace = TRUE),
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = rnorm(100),
    color_var = sample(c("Red", "Blue", "Green"), 100, replace = TRUE)
  )

  filters = list(list("var1", list("A", "B")))
  dfFiltered <- filter_with_lists(df, filters)

  expect_equal(nrow(dfFiltered), sum(df$var1 %in% c("A", "B")))
})
