test_that("liver_fat_markers computes HSI and NAFLD_LFS", {
  skip_on_cran()
  df <- data.frame(
    ALT = c(30, 40), AST = c(20, 35), BMI = c(25, 32),
    sex = c(1, 2), diabetes = c(0, 1), MetS = c(1, 0), insulin = c(15, 20)
  )
  res <- liver_fat_markers(df, col_map = list(
    ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex",
    diabetes = "diabetes", MetS = "MetS", insulin = "insulin"
  ))
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("HSI", "NAFLD_LFS"))
  exp_hsi <- 8 * (df$ALT / df$AST) + df$BMI + 2 * as.numeric(df$sex == 2) + 2 * as.numeric(df$diabetes == 1)
  exp_lfs <- -2.89 + 1.18 * df$MetS + 0.45 * df$diabetes + 0.15 * df$insulin + 0.04 * df$AST - 0.94 * (df$AST / df$ALT)
  expect_equal(res$HSI, as.numeric(exp_hsi), tolerance = 1e-10)
  expect_equal(res$NAFLD_LFS, as.numeric(exp_lfs), tolerance = 1e-10)
})

test_that("liver_fat_markers missing handling omit", {
  skip_on_cran()
  df <- data.frame(
    ALT = c(30, NA), AST = c(20, 35), BMI = c(25, 32),
    sex = c(1, 2), diabetes = c(0, 1), MetS = c(1, 0), insulin = c(15, 20)
  )
  res <- liver_fat_markers(
    df,
    col_map = list(
      ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex",
      diabetes = "diabetes", MetS = "MetS", insulin = "insulin"
    ),
    na_action = "omit",
    na_warn_prop = 0
  )
  expect_equal(nrow(res), 1L)
})

test_that("verbose = TRUE emits preparing, col_map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(ALT = 30, AST = 20, BMI = 25, sex = 2, diabetes = 1, insulin = 15)
  cm <- list(ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex",
             diabetes = "diabetes", insulin = "insulin")
  expect_message(liver_fat_markers(df, col_map = cm, verbose = TRUE), "liver_fat_markers")
  expect_message(liver_fat_markers(df, col_map = cm, verbose = TRUE), "col_map")
  expect_message(liver_fat_markers(df, col_map = cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- data.frame(ALT = 30, AST = 20, BMI = 25, sex = 2, diabetes = 1, insulin = 15)
  cm <- list(ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex",
             diabetes = "diabetes", insulin = "insulin")
  msgs <- testthat::capture_messages(
    liver_fat_markers(df, col_map = cm, verbose = TRUE)
  )
  expect_true(sum(grepl("col_map", msgs)) >= 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
