library(testthat)

# @@@@@@@@@
# ICE ----
# @@@@@@@@@

col_names <- c(
  "assay", "endpoint", "substance_type", "casrn", "qsar_ready_id",
  "value", "unit", "species", "receptor_species", "route", "sex",
  "strain", "life_stage", "tissue", "lesion", "location",
  "assay_source", "in_vitro_assay_format", "reference",
  "reference_url", "dtxsid", "substance_name", "pubmed_id", "query"
)

Sys.sleep(4)

test_that("extr_ice fetches data for CASRN 50-00-0", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("50-00-0", "1332-21-4", "bella", "ciao")

  expect_warning({
    out <- extr_ice(casrn = ids_search, verbose = TRUE)
  })

  expect_equal(sum(is.na(out$casrn)), 2)
  expect_true(is.data.frame(out))
  expect_equal(names(out), col_names)
  expect_equal(nrow(out), 287)
  expect_true(all(c("bella", "ciao") %in% out$query))
})


Sys.sleep(3)

test_that("extr_ice generate results with 1 casrn", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("bella")

  expect_silent({
    out <- extr_ice(casrn = ids_search, verbose = FALSE)
  })

  expect_equal(sum(is.na(out$casrn)), 1)
  expect_true(is.data.frame(out))
  expect_equal(names(out), col_names)
  expect_equal(nrow(out), 1)
})

test_that("extr_ice generate results with 2 casrn", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("bella", "ciao")

  expect_no_warning({
    out <- extr_ice(casrn = ids_search, verbose = FALSE)
  })

  expect_equal(sum(is.na(out$casrn)), 2)
  expect_true(is.data.frame(out))
  expect_equal(names(out), col_names)
  expect_equal(nrow(out), 2)
})

# @@@@@@@@@@@@@@@@@@@@@@
# TEST FIND ASSAYS. ---
# @@@@@@@@@@@@@@@@@@@@@@

test_that("extr_ice_assay_names returns 2030 elements with NULL", {
  result <- extr_ice_assay_names()
  expect_equal(length(result), 2030)
})

# Test 2: When searching for "opera", it should return exactly 45 elements
test_that("extr_ice_assay_names returns 45 elements for 'opera' search", {
  result <- extr_ice_assay_names("OPERA")
  expect_equal(length(result), 45) #
})

test_that("extr_ice_assay_names returns 0 elements for '10' search", {
  expect_error(extr_ice_assay_names(10))
})
