library(testthat)

temp_dir <- tempdir()


Sys.sleep(4)

test_that("extr_pprtv casrn hit and not hit, verbose,  force = TRUE", {
  skip_on_cran()
  skip_if_offline()
  ids_search <- c("112-27-6", "98-86-2")

  expect_message(
    {
      out <- extr_pprtv(ids = ids_search, force = TRUE, verbose = TRUE)
    },
    "Extracting EPA PPRTVs."
  )

  tmp_out <- tools::R_user_dir("extractox", which = "cache")
  tmp_out <- normalizePath(tmp_out, mustWork = FALSE)
  cache_exist <- file.exists(file.path(tmp_out, "epa_pprtvs.rds"))

  expect_true(cache_exist)
  expect_equal(nrow(out), length(ids_search))
  expect_true("query" %in% names(out))
  expect_equal(out$query, ids_search)
})

Sys.sleep(4)

test_that("Function to warn with  verbose = TRUE", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("112-27-6", "bella", "ciao")
  expect_warning(
    {
      out <- extr_pprtv(
        ids = ids_search,
        force = FALSE, verbose = TRUE
      )
    },
    "Chemicals .* not found!"
  )

  expect_equal(out$query, ids_search)
  expect_equal(nrow(out), length(ids_search))
  expect_true(is.na(out$casrn[[3]]))
})

Sys.sleep(4)

test_that("Function verbose = FALSE", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("112-27-6", "98-86-2")
  expect_silent({
    out <- extr_pprtv(
      ids = ids_search,
      force = FALSE, verbose = FALSE
    )
  })
})

Sys.sleep(4)

test_that("extr_pprtv na,es hit and not hit, verbose,  force = TRUE", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("Ace", "Acetophenone")

  expect_message(
    {
      out <- extr_pprtv(
        ids = ids_search,
        search_type = "name",
        force = TRUE,
        verbose = TRUE
      )
    },
    "Extracting EPA PPRTVs."
  )

  expect_equal(nrow(out), 11)
})
