library(testthat)

col_names <- c(
  "casrn", "agent", "group", "volume", "volume_publication_year",
  "evaluation_year", "additional_information", "query"
)

test_that("extr_monograph returns correct outs for CASRN search", {
  ids <- c("105-74-8", "120-58-1")
  out <- extr_monograph(ids = ids, search_type = "casrn", verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(out$casrn %in% ids))
  expect_equal(col_names, names(out))
  expect_equal(sum(is.na(out$agent)), 0)
})

test_that("extr_monograph returns correct outs for name search", {
  ids <- c("Aloe", "Schistosoma", "Styrene")
  out <- extr_monograph(ids = ids, search_type = "name", verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 8)
  expect_equal(ncol(out), 8)
  expect_true(all(grepl(paste(ids, collapse = "|"), out$query)))
  expect_equal(col_names, names(out))
})

test_that("extr_monograph handles missing ids argument", {
  expect_error(
    extr_monograph(search_type = "casrn", verbose = FALSE),
    "The argument .*ids.* is required."
  )
})

test_that("extr_monograph handles invalid search_type argument", {
  ids <- c("105-74-8")
  expect_error(
    extr_monograph(ids = ids, search_type = "invalid_type", verbose = FALSE),
    "The argument"
  )
})

test_that("extr_monograph outputs verbose messages when enabled", {
  ids <- c("105-74-8", "bella", "ciao")
  expect_warning(
    out <- extr_monograph(ids = ids, search_type = "casrn", verbose = TRUE),
    "Chemicals.*found!"
  )

  expect(sum(is.na(out$agent)), 2)
})
