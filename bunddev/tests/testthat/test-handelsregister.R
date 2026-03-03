test_that("handelsregister search returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  result <- tryCatch(
    handelsregister_search("Deutsche Bahn"),
    error = function(e) skip(paste("API unavailable:", conditionMessage(e)))
  )
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("name", "court", "register_num", "state") %in% names(result)))
})
