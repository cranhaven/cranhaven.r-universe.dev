test_that("bunddev_registry returns a valid tibble", {
  registry <- bunddev_registry()

  expect_s3_class(registry, "tbl_df")
  expect_true(all(
    c("id", "title", "provider", "spec_url", "docs_url", "auth", "rate_limit", "tags") %in%
      names(registry)
  ))
  expect_equal(anyDuplicated(registry$id), 0)
  expect_true(all(registry$auth %in% c("none", "api_key", "oauth2")))
})

test_that("bunddev_list filters by tag and auth", {
  weather <- bunddev_list(tag = "weather")
  expect_true(all(purrr::map_lgl(weather$tags, ~ "weather" %in% .x)))

  none_auth <- bunddev_list(auth = "none")
  expect_true(all(none_auth$auth == "none"))
})

test_that("bunddev_search finds entries", {
  results <- bunddev_search("abfallnavi")
  expect_true(any(results$id == "abfallnavi"))
})

test_that("bunddev_info returns a single entry", {
  info <- bunddev_info("abfallnavi")
  expect_s3_class(info, "tbl_df")
  expect_equal(nrow(info), 1)
  expect_equal(info$id, "abfallnavi")
  expect_error(bunddev_info("missing"))
})
