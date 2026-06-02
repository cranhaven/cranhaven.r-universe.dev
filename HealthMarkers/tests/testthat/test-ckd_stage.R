test_that("ckd_stage classifies G and A stages", {
  skip_on_cran()
  df <- data.frame(eGFR = c(95, 70, 50, 35, 10), UACR = c(10, 50, 200, 600, 100))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("CKD_stage", "Albuminuria_stage", "KDIGO_risk"))
  expect_equal(as.character(res$CKD_stage), c("G1","G2","G3a","G3b","G5"))
  expect_equal(as.character(res$Albuminuria_stage), c("A1","A2","A2","A3","A2"))
})

test_that("eGFR boundary values map to correct G stages", {
  skip_on_cran()
  df <- data.frame(
    eGFR = c(90, 89, 60, 59, 45, 44, 30, 29, 15, 14),
    UACR = rep(10, 10)
  )
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_equal(
    as.character(res$CKD_stage),
    c("G1","G2","G2","G3a","G3a","G3b","G3b","G4","G4","G5")
  )
})

test_that("UACR boundary values map to correct A stages", {
  skip_on_cran()
  df <- data.frame(eGFR = rep(65, 4), UACR = c(0, 29, 30, 300))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_equal(as.character(res$Albuminuria_stage), c("A1","A1","A2","A3"))
})

test_that("eGFR-only (no UACR) returns NA albuminuria and KDIGO assuming A1", {
  skip_on_cran()
  df <- data.frame(eGFR = c(95, 50, 12))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR"))
  expect_equal(as.character(res$CKD_stage), c("G1","G3a","G5"))
  expect_true(all(is.na(res$Albuminuria_stage)))
  # G1 + A1 (assumed) -> Low; G3a + A1 -> Moderate; G4/G5 -> Very High
  expect_equal(as.character(res$KDIGO_risk), c("Low","Moderate","Very High"))
})

test_that("ckd_stage default keep retains rows; omit drops any NA in mapped inputs", {
  skip_on_cran()
  df <- data.frame(eGFR = c(NA, 80), UACR = c(10, NA))
  res_keep <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_equal(nrow(res_keep), 2L)
  expect_true(is.na(res_keep$CKD_stage[1]))
  res_omit <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_action = "omit")
  expect_equal(nrow(res_omit), 0L)
})

test_that("ckd_stage na_action = error aborts on missing mapped inputs", {
  skip_on_cran()
  df <- data.frame(eGFR = c(NA, 80), UACR = c(10, NA))
  expect_error(
    ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_action = "error"),
    "missing/non-finite values"
  )
})

test_that("extreme eGFR/UACR values stage correctly without error", {
  skip_on_cran()
  df <- data.frame(eGFR = c(250, 60), UACR = c(10, 6000))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  # eGFR 250 > G1 threshold, maps to G1
  expect_equal(as.character(res$CKD_stage[1]), "G1")
  # UACR 6000 > 300 -> A3
  expect_equal(as.character(res$Albuminuria_stage[2]), "A3")
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df  <- data.frame(eGFR = c(95, 50), UACR = c(10, 200))
  cm2 <- list(eGFR = "eGFR", UACR = "UACR")
  expect_message(ckd_stage(df, col_map = cm2, verbose = TRUE), "ckd_stage")
  expect_message(ckd_stage(df, col_map = cm2, verbose = TRUE), "col_map")
  expect_message(ckd_stage(df, col_map = cm2, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- data.frame(eGFR = c(95, 50), UACR = c(10, 200))
  cm2  <- list(eGFR = "eGFR", UACR = "UACR")
  msgs <- testthat::capture_messages(
    ckd_stage(df, col_map = cm2, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
