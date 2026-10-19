ec_description <- function() {
  path <- system.file("DESCRIPTION", package = "EconCausal")
  expect_true(nzchar(path))
  read.dcf(path)
}

ec_dependencies <- function(field) {
  desc <- ec_description()
  if (!field %in% colnames(desc)) {
    return(character())
  }
  entries <- strsplit(desc[1L, field], ",", fixed = TRUE)[[1L]]
  trimws(sub("\\(.*", "", entries))
}

test_that("the package exports exactly the three documented functions", {
  expect_setequal(
    getNamespaceExports("EconCausal"),
    c("bglmar1", "bsts_model", "ecm_mars")
  )
})

test_that("the three exported functions share the same first three arguments", {
  for (fun in list(ecm_mars, bsts_model, bglmar1)) {
    expect_identical(names(formals(fun))[1:3],
                     c("data_path", "circ_vars", "prod_vars"))
  }
})

test_that("the Boom family stays optional", {
  # CRAN archives Boom, BoomSpikeSlab and bsts on 2026-10-06. The package must
  # keep installing and checking without them, which holds only while they are
  # suggested and never imported.
  optional <- c("bsts", "BoomSpikeSlab", "Boom")
  expect_true(all(c("bsts", "BoomSpikeSlab") %in% ec_dependencies("Suggests")))
  expect_false(any(optional %in% ec_dependencies("Imports")))
  expect_false(any(optional %in% ec_dependencies("Depends")))
})

test_that("the Stan backends stay optional", {
  backends <- c("rstan", "cmdstanr")
  expect_true(all(backends %in% ec_dependencies("Suggests")))
  expect_false(any(backends %in% ec_dependencies("Imports")))
})

test_that("the package declares the licence it was relicensed to", {
  expect_identical(unname(ec_description()[1L, "License"]), "GPL (>= 3)")
})

test_that("the test suite runs under testthat edition 3", {
  # local_mocked_bindings(), expect_setequal() and the third edition semantics
  # this suite relies on are not available under edition 2.
  expect_identical(unname(ec_description()[1L, "Config/testthat/edition"]), "3")
  expect_true("testthat" %in% ec_dependencies("Suggests"))
})

test_that("the selection thresholds keep their declared defaults", {
  # These are the magnitudes that decide which pairs survive; a silent change
  # would move every published result without touching a single formula.
  expect_equal(formals(ecm_mars)$support_min, 0.75)
  expect_equal(formals(ecm_mars)$folds_min_abs, 5)
  expect_equal(formals(ecm_mars)$eg_p_cutoff, 0.05)
  expect_equal(formals(ecm_mars)$ecm_p_cutoff, 0.05)

  expect_equal(formals(bsts_model)$support_min, 0.6)
  expect_equal(formals(bsts_model)$sup_hi, 0.7)
  expect_equal(formals(bsts_model)$sup_lo, 0.6)
  expect_equal(formals(bsts_model)$folds_min, 5)

  expect_equal(formals(bglmar1)$support_min, 0.6)
  expect_equal(formals(bglmar1)$sup_hi, 0.7)
  expect_equal(formals(bglmar1)$sup_lo, 0.6)
  expect_equal(formals(bglmar1)$folds_min, 5)
})

test_that("the seeds that make a run reproducible keep their values", {
  expect_equal(formals(bsts_model)$seed, 123)
  expect_equal(formals(bglmar1)$seed, 2025)
})
