# tests/testthat/test-validate.R

test_that(".nys_dataset_request validates limit", {
  expect_error(.nys_dataset_request("28gk-bu58", limit = "bad"), "limit.*single numeric", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", limit = NA), "limit.*non-missing", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", limit = -1), "limit.*between 0", ignore.case = TRUE)
})

test_that(".nys_dataset_request validates timeout_sec", {
  expect_error(.nys_dataset_request("28gk-bu58", timeout_sec = "bad"), "timeout_sec.*single numeric", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", timeout_sec = NA), "timeout_sec.*non-missing", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", timeout_sec = 0), "timeout_sec.*> 0", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", timeout_sec = -5), "timeout_sec.*> 0", ignore.case = TRUE)
})

test_that(".nys_dataset_request validates filters structure", {
  expect_error(.nys_dataset_request("28gk-bu58", filters = "not a list"), "filters.*named list", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", filters = list("MBA")), "filters.*named", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", filters = list(award_name = NA)), "filters.*NA", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", filters = list(award_name = character(0))), "filters.*empty", ignore.case = TRUE)
})

test_that(".nys_dataset_request validates order and where", {
  expect_error(.nys_dataset_request("28gk-bu58", order = 1), "order.*character", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", order = ""), "order.*non-empty", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", where = 1), "where.*character", ignore.case = TRUE)
  expect_error(.nys_dataset_request("28gk-bu58", where = NA_character_), "where.*non-missing", ignore.case = TRUE)
})

test_that(".nys_add_filters supports IN() for multi-value filters", {
  q <- .nys_add_filters(list(), list(award_name = c("MBA", "BBA")))
  expect_true(grepl("(TRIM\\(award_name\\)|award_name)\\s+IN\\s*\\(", q[["$where"]]))
  expect_true(grepl("'MBA'", q[["$where"]]))
  expect_true(grepl("'BBA'", q[["$where"]]))
})

test_that(".nys_add_where combines clauses with AND", {
  q <- .nys_add_filters(list(), list(award_name = "MBA"))
  q2 <- .nys_add_where(q, "tap_eligible == 'Yes'")
  expect_true(grepl("award_name", q2[["$where"]]))
  expect_true(grepl("tap_eligible", q2[["$where"]]))
  expect_true(grepl("\\) AND \\(", q2[["$where"]]))
})
