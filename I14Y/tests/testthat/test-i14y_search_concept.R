test_that("i14y_search_concept() returns a data.frame", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  df_concept_all <- i14y_search_concept(page = 1, pageSize = 10)
  expect_s3_class(df_concept_all, "data.frame")
  expect_true(nrow(df_concept_all) == 10)
  
  df_concept_noga <- i14y_search_concept(query = "noga", page = 1, pageSize = 1)
  expect_s3_class(df_concept_noga, "data.frame")
  expect_true(nrow(df_concept_noga) >= 1)
})
