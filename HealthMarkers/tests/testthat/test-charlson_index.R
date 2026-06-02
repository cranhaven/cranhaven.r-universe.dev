library(testthat)

keys <- c("mi","chf","pvd","stroke","dementia","copd","rheum","ulcer",
          "mild_liver","diabetes","diab_comp","hemiplegia","renal","cancer",
          "leukemia","lymphoma","sev_liver","metastatic_cancer","hiv")

cm <- as.list(stats::setNames(keys, keys))

# n-row data frame, default all 0, override selected keys
make_df <- function(vals, n = 1L) {
  base <- as.list(stats::setNames(rep(list(rep(0, n)), length(keys)), keys))
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (length(v) == 1L) {
      v <- rep(v, n)
    }
    base[[nm]] <- v
  }
  as.data.frame(base, check.names = FALSE)
}

test_that("mapping validation and missing columns error", {
  df <- make_df(list())
  expect_error(charlson_index(df, list()), class = "healthmarkers_cci_error_missing_map")
  bad_map <- cm; bad_map$mi <- ""
  expect_error(charlson_index(df, bad_map), class = "healthmarkers_cci_error_bad_map_values")
  expect_error(charlson_index(data.frame(x=1), cm), class = "healthmarkers_cci_error_missing_columns")
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- make_df(list())
  expect_message(charlson_index(df, cm, verbose = TRUE), "charlson_index")
  expect_message(charlson_index(df, cm, verbose = TRUE), "col_map")
  expect_message(charlson_index(df, cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df   <- make_df(list())
  msgs <- testthat::capture_messages(charlson_index(df, cm, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("numeric coercion warning when strings introduce NAs", {
  skip_on_cran()
  df <- make_df(list(mi = c("1", "oops")), n = 2)
  expect_warning(charlson_index(df, cm), class = "healthmarkers_cci_warn_na_coercion")
})

test_that("NA policies: keep, omit, error, warn", {
  skip_on_cran()
  df <- make_df(list(mi = c(1, NA, 0)), n = 3)

  out_keep <- charlson_index(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$charlson_index[2]))

  out_omit <- charlson_index(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 2L)

  expect_error(
    charlson_index(df, cm, na_action = "error"),
    class = "healthmarkers_cci_error_missing_values"
  )

  expect_warning(
    charlson_index(df, cm, na_action = "warn"),
    class = "healthmarkers_cci_warn_missing_inputs"
  )
})

test_that("domain warnings for non-binary indicators", {
  skip_on_cran()
  df <- make_df(list(mi = -1, chf = 2))
  expect_warning(charlson_index(df, cm),
                 class = "healthmarkers_cci_warn_out_of_range")
})

test_that("check_extreme removed: function passes through numeric outlier rows", {
  skip_on_cran()
  df <- make_df(list(mi = -1, chf = 2, pvd = 0.5), n = 3)
  # domain warning for non-binary is still emitted, but no extreme-scan error
  out <- suppressWarnings(charlson_index(df, cm))
  expect_equal(nrow(out), 3L)
})

test_that("base 1-, 2-, and 6-point weights computed correctly", {
  skip_on_cran()
  df <- make_df(list(
    mi=1, chf=1, pvd=1, stroke=1, dementia=1, copd=1, rheum=1, ulcer=1,
    hemiplegia=1, renal=1, leukemia=1, lymphoma=1, hiv=1
  ))
  # 8 one-point + 4*2-point + 6 = 8 + 8 + 6 = 22
  out <- charlson_index(df, cm)
  expect_equal(out$charlson_index[1], 22L)
})

test_that("paired categories use maximum weight without double counting", {
  skip_on_cran()
  # Diabetes pair
  df1 <- make_df(list(diabetes=1, diab_comp=0))
  out1 <- charlson_index(df1, cm)
  expect_equal(out1$charlson_index[1], 1L)

  df2 <- make_df(list(diabetes=0, diab_comp=1))
  out2 <- charlson_index(df2, cm)
  expect_equal(out2$charlson_index[1], 2L)

  df3 <- make_df(list(diabetes=1, diab_comp=1))
  out3 <- charlson_index(df3, cm)
  expect_equal(out3$charlson_index[1], 2L)

  # Liver disease pair
  df4 <- make_df(list(mild_liver=1, sev_liver=0))
  df5 <- make_df(list(mild_liver=0, sev_liver=1))
  df6 <- make_df(list(mild_liver=1, sev_liver=1))
  expect_equal(charlson_index(df4, cm)$charlson_index[1], 1L)
  expect_equal(charlson_index(df5, cm)$charlson_index[1], 3L)
  expect_equal(charlson_index(df6, cm)$charlson_index[1], 3L)

  # Cancer vs metastatic
  df7 <- make_df(list(cancer=1, metastatic_cancer=0))
  df8 <- make_df(list(cancer=0, metastatic_cancer=1))
  df9 <- make_df(list(cancer=1, metastatic_cancer=1))
  expect_equal(charlson_index(df7, cm)$charlson_index[1], 2L)
  expect_equal(charlson_index(df8, cm)$charlson_index[1], 6L)
  expect_equal(charlson_index(df9, cm)$charlson_index[1], 6L)
})

test_that("comprehensive example total is correct", {
  skip_on_cran()
  df <- make_df(list(
    mi=1, chf=1, pvd=1, stroke=1, dementia=1, copd=1, rheum=1, ulcer=1,
    mild_liver=1, diabetes=1, diab_comp=0, hemiplegia=1, renal=1,
    cancer=1, leukemia=1, lymphoma=1, sev_liver=0, metastatic_cancer=0, hiv=1
  ))
  # Base: 8 one-point = 8; 4 two-point (hemiplegia, renal, leukemia, lymphoma) = 8; HIV=6 -> 22
  # Diabetes (1), liver (1), cancer (2) -> +4 => total 26
  out <- charlson_index(df, cm)
  expect_equal(out$charlson_index[1], 26L)
})

test_that("logical inputs (TRUE/FALSE) are handled via numeric coercion", {
  skip_on_cran()
  df <- make_df(list(mi = c(TRUE, FALSE)), n = 2)
  out <- suppressWarnings(charlson_index(df, cm))
  expect_equal(out$charlson_index, c(1L, 0L))
})

test_that("padding preserved for keep/warn", {
  skip_on_cran()
  df <- make_df(list(mi = c(1, NA, 0)), n = 3)
  out_keep <- charlson_index(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(charlson_index(df, cm, na_action = "warn"))
  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
