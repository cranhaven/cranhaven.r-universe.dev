library(testthat)

Sys.sleep(5)

# Test 1: Check if the function works with a valid chemical name input
test_that("Valid chemical name input", {
  skip_on_cran()
  skip_if_offline()

  suppressWarnings(
    out <- extr_comptox(
      ids = c("Aspirin"),
      download_items = c("DTXCID", "CASRN"),
      verbose = FALSE
    )
  )

  out <- extr_comptox(
    ids = c("Aspirin"),
    download_items = c("DTXCID", "CASRN"),
    verbose = FALSE
  )
  expect_true(is.list(out))
  expect_true(all(unlist(lapply(out, is.data.frame))))
  expect_equal(nrow(out$comptox_main_data), 1)
  expect_equal(nrow(out$comptox_main_data), 1)
  expect_equal(nrow(out$comptox_cover_sheet), 4)
})

Sys.sleep(5)

col_names <- c(
  "comptox_cover_sheet", "comptox_main_data", "comptox_abstract_sifter",
  "comptox_synonym_identifier", "comptox_related_relationships",
  "comptox_toxcast_assays_ac50", "comptox_toxval_details", "comptox_chemical_properties"
)

test_that("Valid inputs", {
  skip_on_cran()
  skip_if_offline()

  ids <- c("50-00-0", "Aspirin", "DTXSID5020023")

  suppressWarnings(
    out <- extr_comptox(
      ids = ids,
      verbose = FALSE
    )
  )

  expect_equal(names(out), col_names)
  expect_equal(out$comptox_main_data$input, ids)
  expect_equal(ncol(out$comptox_main_data), 64)
})

Sys.sleep(5)

test_that("extr_comptox when download_items is set to one val", {
  skip_on_cran()
  skip_if_offline()

  expect_no_error(
    out <- extr_comptox(c("50-00-0", "80-05-7"))
  )
})

test_that("extr_comptox warn for unknown ids", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      out <- extr_comptox(c("31-12-5", "bella", "ciao"))
    },
    "Chemicals.*bella.*ciao.*not found!"
  )
})
