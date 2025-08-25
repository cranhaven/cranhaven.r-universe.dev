library(testthat)

# @@@@@@@@@@@@@@@@@@@@@@@@@
# extr_casrn_from_cid ----
# @@@@@@@@@@@@@@@@@@@@@@@@

col_names <- c("cid", "iupac_name", "casrn", "source_name", "source_id", "query")

Sys.sleep(4)

test_that("extr_casrn_from_cid generate results with 2 cid, one wrong", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("bella", "712")
  suppressWarnings(
    expect_warning(
      {
        out <- extr_casrn_from_cid(pubchem_ids = ids_search, verbose = TRUE)
      },
      "Chemical .* found!"
    )
  )

  expect_equal(sum(is.na(out$casrn)), 1)
  expect_true(is.data.frame(out))
  expect_equal(names(out), col_names)
  expect_equal(nrow(out), 28)
})

Sys.sleep(4)

test_that("extr_casrn_from_cid generate results with all wrong", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("bella", "ciao")
  expect_warning(
    {
      out <- extr_casrn_from_cid(pubchem_ids = ids_search, verbose = TRUE)
    },
    "Chemicals .* found!"
  )

  expect_equal(sum(is.na(out$casrn)), 2)
  expect_true(is.data.frame(out))
  expect_equal(names(out), col_names)
  expect_equal(nrow(out), 2)
})

Sys.sleep(4)

test_that("extr_casrn_from_cid generate results with all wrong", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("bella", "ciao")
  expect_silent({
    out <- extr_casrn_from_cid(pubchem_ids = ids_search, verbose = FALSE)
  })
})


Sys.sleep(4)

# @@@@@@@@@@@@@@@@@@@
# extr_chem_info ----
# @@@@@@@@@@@@@@@@@@@


df_names <- create_na_df("ciao")

test_that("extr_chem_info fetches chems", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("Formaldehyde", "Aflatoxin B1", "bella", "ciao")

  suppressWarnings(
    expect_warning({
      out <- extr_chem_info(ids_search)
    })
  )

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), length(ids_search))
  expect_equal(names(out), names(df_names))
  expect_true(all(out$query %in% ids_search))
})

Sys.sleep(4)

test_that("extr_chem_info wrong only, silent", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- "bella ciao"
  skip_on_cran()
  expect_silent({
    out <- extr_chem_info(ids_search, verbose = FALSE)
  })

  expect_true(is.data.frame(out))
  expect_equal(nrow(out), length(ids_search))
  expect_equal(names(out), names(df_names))
  expect_true(all(out$query %in% ids_search))
})

# @@@@@@@@@@@@@@@
# extr_fema  ----
# @@@@@@@@@@@@@@@

col_names <- c(
  "cid",
  "casrn",
  "IUPAC_name",
  "result",
  "source_name",
  "source_id",
  "other",
  "query"
)

Sys.sleep(4)

test_that("extr_pubchem_fema works correctly", {
  skip_on_cran()
  skip_if_offline()

  casrn_list <- c("1490-04-6", "50-00-0", "bella_ciao")

  expect_silent({
    out <- extr_pubchem_fema(casrn_list, verbose = FALSE)
  })

  expect_equal(nrow(out), length(casrn_list))
  expect_equal(names(out), col_names)
  expect_equal(out$query, casrn_list)
  expect_equal(out$casrn, c("1490-04-6", "50-00-0", NA))
})

Sys.sleep(4)

test_that("extr_pubchem_fema produce CASRN warning", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      out <- extr_pubchem_fema(c("bella", "ciao"), verbose = TRUE)
    },
    "Chemical.*not found!"
  )
})

Sys.sleep(4)

test_that("extr_pubchem_fema produce FEMA warning", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      out <- extr_pubchem_fema("50-00-0", verbose = TRUE)
    },
    "FEMA .*not found"
  )
})

Sys.sleep(4)

test_that("extr_pubchem_ghs works correctly", {
  skip_on_cran()
  skip_if_offline()

  casrn_list <- c("1490-04-6", "50-00-0", "bella_ciao")

  expect_silent({
    out <- extr_pubchem_ghs(casrn_list, verbose = FALSE)
  })

  expect_equal(unique(out$query), casrn_list)
  expect_equal(names(out), col_names)
  expect_equal(unique(out$casrn), c("1490-04-6", "50-00-0", NA))
})

Sys.sleep(4)

test_that("extr_pubchem_ghs produce warning", {
  skip_on_cran()
  skip_if_offline()

  expect_warning(
    {
      out <- extr_pubchem_ghs(c("bella", "ciao"), verbose = TRUE)
    },
    "not found"
  )
})
