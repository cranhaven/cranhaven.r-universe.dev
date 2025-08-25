library(testthat)

# @@@@@@@@@
# IRIS ----
# @@@@@@@@@

col_names <- c(
  "chemical_name", "casrn", "exposure_route", "assessment_type",
  "critical_effect_or_tumor_type", "woe_characterization",
  "toxicity_value_type", "toxicity_value", "query"
)

Sys.sleep(4)

test_that("extr_iris_ fetches data for multiple CASRN", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("50-00-0", "1332-21-4")
  expect_message(
    {
      out <- extr_iris(ids_search, verbose = TRUE, delay = 4)
    },
    "Quering"
  )

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 4)
  expect_equal(names(out), col_names)
  expect_true(all(out$query %in% ids_search))
})

Sys.sleep(4)

test_that("extr_iris_ warn and fill with NA wrong CASRN", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("50-00-0", "1332-21-4", "bella", "ciao")
  expect_warning(
    {
      out <- extr_iris(ids_search, verbose = TRUE, delay = 4)
    },
    "Chemicals.*found!"
  )

  expect_equal(nrow(out), 6)
  expect_true(all(out$query %in% ids_search))
  expect_equal(sum(is.na(out$casrn)), 2)
})

Sys.sleep(4)

test_that("extr_iris_ cancer_types and verbose = FALSE work", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("50-00-0", "1332-21-4", "bella", "ciao")
  expect_silent({
    out <- extr_iris(ids_search, verbose = FALSE, delay = 4)
  })

  expect_equal(nrow(out), 6)
  expect_true(all(out$query %in% ids_search))
  expect_equal(sum(is.na(out$casrn)), 2)
})
