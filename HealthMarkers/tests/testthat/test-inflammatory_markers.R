# HM-CS v2 compatible tests for inflammatory_markers()

test_that("default na_action is 'keep' (not 'error')", {
  skip_on_cran()
  df <- tibble::tibble(neutrophils = c(4, NA), lymphocytes = c(2, 1))
  cm <- list(neutrophils = "neutrophils", lymphocytes = "lymphocytes")
  out <- inflammatory_markers(df, cm, panel = "classic", verbose = FALSE)
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$NLR[2]))
})

test_that("mapping validation errors and messages", {
  skip_on_cran()
  df <- tibble::tibble(neutrophils = numeric())
  expect_error(inflammatory_markers("x", list()), "data.frame or tibble")
  expect_error(inflammatory_markers(df, list(1)), "named list")
  # classic requires neutrophils + lymphocytes
  expect_error(
    inflammatory_markers(df, list(neutrophils = "neutrophils"), panel = "classic"),
    "missing col_map entries for: lymphocytes"
  )
  # eos requires albumin name if CRP name present
  df2 <- tibble::tibble(neutrophils = 1, lymphocytes = 1, monocytes = 1, platelets = 1, CRP = 1)
  expect_error(
    inflammatory_markers(df2, list(neutrophils="neutrophils", lymphocytes="lymphocytes",
                                   monocytes="monocytes", platelets="platelets", CRP="CRP"),
                         panel = "eos"),
    "missing col_map entries for: albumin"
  )
})

test_that("verbose emits col_map, optional inputs, computing markers, and results messages", {
  skip_on_cran()
  df_v <- tibble::tibble(neutrophils = numeric(0), lymphocytes = numeric(0))
  cm_v <- list(neutrophils = "neutrophils", lymphocytes = "lymphocytes")
  msgs <- testthat::capture_messages(
    inflammatory_markers(df_v, cm_v, panel = "classic", verbose = TRUE)
  )
  expect_true(any(grepl("col_map", msgs)))
  expect_true(any(grepl("optional inputs", msgs)))
  expect_true(any(grepl("computing markers", msgs)))
  expect_true(any(grepl("results:", msgs)))
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df_v <- tibble::tibble(neutrophils = numeric(0), lymphocytes = numeric(0))
  cm_v <- list(neutrophils = "neutrophils", lymphocytes = "lymphocytes")
  msgs <- testthat::capture_messages(
    inflammatory_markers(df_v, cm_v, panel = "classic", verbose = TRUE)
  )
  expect_equal(sum(grepl("col_map",   msgs)), 1L)
  expect_equal(sum(grepl("results:",          msgs)), 1L)
  expect_equal(sum(grepl("optional inputs",   msgs)), 1L)
  expect_equal(sum(grepl("computing markers", msgs)), 1L)
})

test_that("classic panel computes markers", {
  skip_on_cran()
  df <- tibble::tibble(
    neutrophils = 4, lymphocytes = 2, monocytes = 0.5, platelets = 200, WBC = 7, CRP = 2.5
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- inflammatory_markers(df, cm, panel = "classic", na_action = "keep", verbose = FALSE)
  expect_equal(names(out), c("NLR","PLR","LMR","dNLR","SII","SIRI","AISI","CRP_category"))
  expect_equal(out$NLR, 4/2)
  expect_equal(out$PLR, 200/2)
  expect_equal(out$dNLR, 4/(7-4))
  expect_equal(as.character(out$CRP_category), "moderate")
})

test_that("eosinophil panel computes markers and ESR passthrough", {
  skip_on_cran()
  df <- tibble::tibble(
    neutrophils = 4, lymphocytes = 2, monocytes = 0.5, platelets = 200,
    CRP = 5, albumin = 40, eosinophils = 0.2, ESR = 12
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- inflammatory_markers(df, cm, panel = "eos", na_action = "keep", verbose = FALSE)
  expect_equal(names(out), c("NLR","PLR","LMR","NER","SII","SIRI","PIV","CLR","CAR","PCR","mGPS","ESR"))
  expect_equal(out$NLR, 4/2)
  expect_equal(out$CAR, 5/40)
  expect_equal(out$mGPS, 0L)
  expect_equal(out$ESR, 12)
})

test_that("na_action policies", {
  skip_on_cran()
  df <- tibble::tibble(neutrophils = c(2, NA), lymphocytes = c(2, 1))
  cm <- list(neutrophils="neutrophils", lymphocytes="lymphocytes")
  out_keep <- inflammatory_markers(df, cm, panel = "classic", na_action = "keep", verbose = FALSE)
  expect_equal(nrow(out_keep), 2L)
  expect_true(is.na(out_keep$NLR[2]))
  out_omit <- inflammatory_markers(df, cm, panel = "classic", na_action = "omit", verbose = FALSE)
  expect_equal(nrow(out_omit), 1L)
  expect_equal(out_omit$NLR, 1)
  expect_error(inflammatory_markers(df, cm, panel = "classic", na_action = "error", verbose = FALSE),
               "missing or non-finite")
})

test_that("extreme values produce no warning/error; range note appears in verbose", {
  skip_on_cran()
  df <- tibble::tibble(
    neutrophils = 40, lymphocytes = 0.2, monocytes = 6, platelets = 5000,
    CRP = 500, albumin = 5, eosinophils = 0.1, ESR = 99
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_no_warning(
    out <- inflammatory_markers(df, cm, panel = "eos", na_action = "keep", verbose = FALSE)
  )
  expect_true(is.numeric(out$NLR))
  # Range note appears in verbose
  msgs <- testthat::capture_messages(
    inflammatory_markers(df, cm, panel = "eos", na_action = "keep", verbose = TRUE)
  )
  expect_true(any(grepl("range note", msgs)))
})

test_that("zero denominators warn and keep Inf/NaN", {
  skip_on_cran()
  df <- tibble::tibble(neutrophils = 2, lymphocytes = 0, platelets = 10, monocytes = 0, WBC = 2)
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(out <- inflammatory_markers(df, cm, panel = "classic", na_action = "keep", verbose = FALSE),
                 "zero denominators")
  expect_true(is.infinite(out$NLR))
  expect_true(is.infinite(out$PLR))
})

test_that("ID column is prepended to output when detected", {
  skip_on_cran()
  df_id <- tibble::tibble(
    id = 1:3, neutrophils = c(4, 3, 5), lymphocytes = c(2, 1.5, 2.5)
  )
  cm_id <- list(neutrophils = "neutrophils", lymphocytes = "lymphocytes")
  out <- inflammatory_markers(df_id, cm_id, panel = "classic", verbose = FALSE)
  expect_equal(names(out)[1L], "id")
  expect_equal(out$id, 1:3)
})
