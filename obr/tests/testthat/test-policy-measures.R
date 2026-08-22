# Network tests for the Policy Measures Database wrapper.

test_that("get_policy_measures() errors on invalid type", {
  expect_error(
    get_policy_measures(type = "bananas"),
    regexp = "should be one of"
  )
})

test_that("get_policy_measures() errors on bad search/since", {
  expect_error(get_policy_measures(search = c("a", "b")),
               regexp = "single character string")
  expect_error(get_policy_measures(since = "20-21"),
               regexp = "fiscal-year string")
})

test_that("get_policy_measures(type='tax') returns expected structure", {
  skip_on_cran()
  skip_if_offline()

  res <- get_policy_measures(type = "tax", since = "2024-25")
  expect_s3_class(res, "obr_tbl")
  expect_named(res, c("type", "event", "measure", "head",
                      "fiscal_year", "value_mn"))
  expect_true(all(res$type == "tax"))
  expect_type(res$value_mn, "double")
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", res$fiscal_year)))
  expect_gte(nrow(res), 50L)

  prov <- obr_provenance(res)
  expect_equal(prov$publication, "PMD")
  expect_match(prov$source_url, "policy-measures-database")
})

test_that("get_policy_measures() defaults return both tax and spending", {
  skip_on_cran()
  skip_if_offline()

  res <- get_policy_measures(since = "2023-24")
  expect_true(all(c("tax", "spending") %in% unique(res$type)))
})

test_that("get_policy_measures(search = ...) filters case-insensitively", {
  skip_on_cran()
  skip_if_offline()

  res <- get_policy_measures(type = "tax", search = "alcohol",
                             since = "2010-11")
  expect_gt(nrow(res), 0L)
  expect_true(all(
    grepl("alcohol", res$measure, ignore.case = TRUE) |
    grepl("alcohol", res$head,    ignore.case = TRUE)
  ))
})

test_that("policy_measures_summary() aggregates by event and fiscal_year", {
  skip_on_cran()
  skip_if_offline()

  raw <- get_policy_measures(type = "tax", since = "2023-24")
  agg <- policy_measures_summary(raw)
  expect_s3_class(agg, "obr_tbl")
  expect_named(agg, c("type", "event", "fiscal_year", "value_mn"))
  expect_lte(nrow(agg), nrow(raw))
  # Same provenance fields preserved
  expect_equal(obr_provenance(agg)$publication, "PMD")
  # No duplicate (event, type, fiscal_year) rows
  key <- paste(agg$event, agg$type, agg$fiscal_year, sep = "|")
  expect_equal(anyDuplicated(key), 0L)
})

test_that("policy_measures_summary() rejects non-obr_tbl input", {
  expect_error(policy_measures_summary(data.frame(x = 1)),
               regexp = "obr_tbl")
})
