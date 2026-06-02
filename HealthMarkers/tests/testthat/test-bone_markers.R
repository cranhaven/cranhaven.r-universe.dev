# File: tests/testthat/test_bone_markers.R

library(testthat)
library(tibble)

cm_full <- list(
  age = "age", weight = "weight", height = "height",
  ALM = "ALM", FM = "FM",
  BMD = "BMD", BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd",
  TBS = "TBS", HSA = "HSA", PINP = "PINP",
  CTX = "CTX", BSAP = "BSAP", Osteocalcin = "Osteocalcin"
)

df_full <- tibble(
  age = c(65, 70),
  weight = c(60, 65),
  height = c(1.6, 1.7),
  ALM = c(18, 20),
  FM = c(20, 25),
  BMD = c(0.9, 1.0),
  BMD_ref_mean = c(1.0, 1.0),
  BMD_ref_sd = c(0.1, 0.1),
  TBS = c(1.1, 1.2),
  HSA = c(0.3, 0.3),
  PINP = c(40, 50),
  CTX = c(0.3, 0.4),
  BSAP = c(12, 15),
  Osteocalcin = c(10, 12)
)

test_that("bone_markers infers missing col_map entries from data (no error)", {
  skip_on_cran()
  bad_map <- cm_full[-1]  # removes "age" key; data still has "age" column
  expect_no_error(bone_markers(df_full, col_map = bad_map))
})

test_that("bone_markers computes core indices correctly", {
  skip_on_cran()
  out <- bone_markers(df_full, col_map = cm_full)
  expect_equal(out$OSTA, (df_full$weight - df_full$age) * 0.2)
  expect_equal(out$ALMI, df_full$ALM / df_full$height^2)
  expect_equal(out$FMI, df_full$FM / df_full$height^2)
  expect_equal(
    out$BMD_Tscore,
    (df_full$BMD - df_full$BMD_ref_mean) / df_full$BMD_ref_sd
  )
})

test_that("optional biomarkers are passed through when present", {
  skip_on_cran()
  out <- bone_markers(df_full, col_map = cm_full)
  expect_equal(out$TBS, df_full$TBS)
  expect_equal(out$HSA, df_full$HSA)
  expect_equal(out$PINP, df_full$PINP)
  expect_equal(out$CTX, df_full$CTX)
  expect_equal(out$BSAP, df_full$BSAP)
  expect_equal(out$Osteocalcin, df_full$Osteocalcin)
})

test_that("missing optional biomarkers yield NA columns", {
  skip_on_cran()
  # drop all optional keys
  cm_core <- cm_full[setdiff(
    names(cm_full),
    c("TBS", "HSA", "PINP", "CTX", "BSAP", "Osteocalcin")
  )]
  df_core <- df_full[, names(cm_core)]
  out <- bone_markers(df_core, col_map = cm_core)
  expect_true(all(is.na(out$TBS)))
  expect_true(all(is.na(out$HSA)))
  expect_true(all(is.na(out$PINP)))
  expect_true(all(is.na(out$CTX)))
  expect_true(all(is.na(out$BSAP)))
  expect_true(all(is.na(out$Osteocalcin)))
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(bone_markers(df_full, col_map = cm_full, verbose = TRUE), "bone_markers")
  expect_message(bone_markers(df_full, col_map = cm_full, verbose = TRUE), "col_map")
  expect_message(bone_markers(df_full, col_map = cm_full, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(
    bone_markers(df_full, col_map = cm_full, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("na_action = 'keep' retains rows with missing/non-finite", {
  skip_on_cran()
  df_bad <- df_full
  df_bad$BMD[1] <- NA_real_
  out <- bone_markers(df_bad, col_map = cm_full, na_action = "keep")
  expect_equal(nrow(out), nrow(df_bad))
})

test_that("na_action = 'omit' drops rows with missing/non-finite inputs", {
  skip_on_cran()
  df_bad <- df_full
  df_bad$height[2] <- NA_real_
  out <- bone_markers(df_bad, col_map = cm_full, na_action = "omit")
  expect_equal(nrow(out), 1L)
})

test_that("na_action = 'error' stops on missing/non-finite inputs", {
  skip_on_cran()
  df_bad <- df_full
  df_bad$height[2] <- NA_real_
  expect_error(
    bone_markers(df_bad, col_map = cm_full, na_action = "error"),
    "missing/non-finite values"
  )
})

test_that("argument constraints are enforced (height > 0, ref_sd > 0)", {
  skip_on_cran()
  df_h0 <- df_full; df_h0$height[1] <- 0
  expect_error(bone_markers(df_h0, col_map = cm_full), "height' must be positive")
  df_sd0 <- df_full; df_sd0$BMD_ref_sd[1] <- 0
  expect_error(bone_markers(df_sd0, col_map = cm_full), "BMD_ref_sd' must be positive")
})

test_that("extra numeric columns in col_map are passed through", {
  skip_on_cran()
  df_extra <- df_full
  df_extra$ALMI_sds <- c(0, 2)
  cm_extra <- cm_full
  cm_extra$ALMI_sds <- "ALMI_sds"
  out <- bone_markers(df_extra, col_map = cm_extra)
  expect_equal(nrow(out), 2L)
})
