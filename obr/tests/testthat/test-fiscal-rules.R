# Tests for obr_fiscal_rules() - offline.

test_that("obr_fiscal_rules() returns the three rules in the current Charter", {
  rules <- obr_fiscal_rules()
  expect_s3_class(rules, "data.frame")
  expect_setequal(rules$rule, c("stability", "investment", "welfare_cap"))
  expect_named(rules, c(
    "rule", "metric", "target_description", "target_year_rule",
    "direction_of_pass", "source_charter", "source_act"
  ))
  # All character columns are populated
  for (col in setdiff(names(rules), "rule")) {
    expect_true(all(nzchar(rules[[col]])),
                info = paste("column", col))
  }
})

test_that("obr_fiscal_rules() cites the current Charter and the Act", {
  rules <- obr_fiscal_rules()
  expect_match(rules$source_charter, "Autumn 2024", all = TRUE)
  expect_match(rules$source_act,     "Budget Responsibility Act 2024",
               all = TRUE)
})

test_that("obr_fiscal_rules() does not ship hardcoded numerical headroom", {
  rules <- obr_fiscal_rules()
  # Numerical headroom values are intentionally not shipped because they
  # change at every fiscal event. Guard against accidental re-introduction.
  expect_false("headroom_published_gbp_bn" %in% names(rules))
  expect_false("headroom" %in% names(rules))
})
