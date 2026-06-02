# tests/testthat/test_adipo_is.R

library(testthat)
library(tibble)
library(dplyr)
library(withr)

run_adipo <- function(df, normalize = "none", ...) {
  adipo_is(
    df,
    col_map = list(
      G0    = "G0",
      I0    = "I0",
      TG    = "TG",
      HDL_c = "HDL_c",
      FFA   = "FFA",
      waist = "waist",
      bmi   = "bmi"
    ),
    normalize = normalize,
    ...
  )
}

base_df <- tibble(
  G0    = 5.5,  # mmol/L
  I0    = 60,   # pmol/L
  TG    = 1.2,  # mmol/L
  HDL_c = 1.0,  # mmol/L
  FFA   = 0.45, # mmol/L
  waist = 80,   # cm
  bmi   = 24    # kg/m^2
)

test_that("partial col_map is supplemented by dictionary inference", {
  skip_on_cran()
  expect_no_error(
    adipo_is(base_df, col_map = list(G0 = "G0"))
  )
})

test_that("returns all 10 adipose-based indices", {
  skip_on_cran()
  out <- run_adipo(base_df)
  expected <- c(
    "Revised_QUICKI","VAI_Men_inv","VAI_Women_inv",
    "TG_HDL_C_inv","TyG_inv","LAP_Men_inv",
    "LAP_Women_inv","McAuley_index","Adipo_inv",
    "Belfiore_inv_FFA"
  )
  expect_named(out, expected)
  expect_equal(ncol(out), length(expected))
  expect_equal(nrow(out), 1L)
})

test_that("vectorized input produces matching row count", {
  skip_on_cran()
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out2 <- run_adipo(df2)
  expect_equal(nrow(out2), 2)
})

test_that("McAuley_index uses TG in mmol/L (not mg/dL)", {
  skip_on_cran()
  I0_u   <- base_df$I0 / 6          # pmol/L -> mU/L
  TG_mmol <- base_df$TG             # mmol/L: use raw, NOT * 88.57
  manual_mc <- exp(2.63 - 0.28 * log(I0_u) - 0.31 * log(TG_mmol))
  out <- run_adipo(base_df)
  expect_equal(out$McAuley_index, manual_mc, tolerance = 1e-8)
  # Sanity: typical adults -> McAuley ~3-10
  expect_gt(out$McAuley_index, 2)
  expect_lt(out$McAuley_index, 15)
})

test_that("LAP uses TG in mmol/L (not mg/dL)", {
  skip_on_cran()
  TG_mmol <- base_df$TG             # mmol/L: use raw
  manual_lap_m <- -((base_df$waist - 65) * TG_mmol)
  manual_lap_f <- -((base_df$waist - 58) * TG_mmol)
  out <- run_adipo(base_df)
  expect_equal(out$LAP_Men_inv,   manual_lap_m, tolerance = 1e-8)
  expect_equal(out$LAP_Women_inv, manual_lap_f, tolerance = 1e-8)
  # Typical men WC=80, TG=1.2: LAP_Men = (80-65)*1.2 = 18; _inv = -18
  expect_lt(out$LAP_Men_inv, 0)
})

test_that("VAI uses TG and HDL in mmol/L (Amato 2010 reference constants)", {
  skip_on_cran()
  TG_mmol  <- base_df$TG      # mmol/L
  HDL_mmol <- base_df$HDL_c   # mmol/L
  manual_vai_m <- -(
    (base_df$waist / (39.68 + 1.88 * base_df$bmi)) *
    (TG_mmol / 1.03) *
    (1.31 / HDL_mmol)
  )
  out <- run_adipo(base_df)
  expect_equal(out$VAI_Men_inv, manual_vai_m, tolerance = 1e-8)
})

test_that("Revised_QUICKI matches manual computation", {
  skip_on_cran()
  I0_u <- base_df$I0 / 6
  G0_mgdL <- base_df$G0 * 18
  FFA_v <- base_df$FFA
  manual <- 1 / (log10(I0_u) + log10(G0_mgdL) + log10(FFA_v))
  out <- run_adipo(base_df)
  expect_equal(out$Revised_QUICKI, manual, tolerance = 1e-8)
})

test_that("normalize='range' maps variable columns to [0,1]; constants allowed", {
  skip_on_cran()
  skip_if_not(exists("normalize_vec", where = asNamespace("HealthMarkers")),
              "normalize_vec not available")
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out_r <- suppressWarnings(run_adipo(df2, normalize = "range"))
  for (col in names(out_r)) {
    vals <- out_r[[col]]
    finite <- vals[is.finite(vals)]
    if (length(unique(finite)) > 1) {
      expect_gte(min(finite), 0)
      expect_lte(max(finite), 1)
    } else {
      expect_true(length(unique(finite)) == 1)
    }
  }
})

test_that("normalize='z' gives mean≈0 / sd≈1 when variable; constants -> zeros", {
  skip_on_cran()
  skip_if_not(exists("normalize_vec", where = asNamespace("HealthMarkers")),
              "normalize_vec not available")
  df3 <- bind_rows(
    base_df,
    mutate(base_df, G0 = 6),
    mutate(base_df, G0 = 8)
  )
  out_z <- suppressWarnings(run_adipo(df3, normalize = "z"))
  for (col in names(out_z)) {
    vals <- out_z[[col]]
    finite <- vals[is.finite(vals)]
    if (length(unique(finite)) > 1) {
      expect_equal(mean(finite), 0, tolerance = 1e-6)
      expect_equal(sd(finite), 1, tolerance = 1e-6)
    } else {
      expect_true(all(finite == 0))
    }
  }
})

test_that("invalid normalize argument handling depends on normalize_vec", {
  skip_on_cran()
  if (exists("normalize_vec", where = asNamespace("HealthMarkers"))) {
    expect_error(run_adipo(base_df, normalize = "foo"),
                 "`normalize` must be one of|invalid 'method'")
  } else {
    expect_error(run_adipo(base_df, normalize = "foo"), NA)
  }
})

test_that("na_action policies", {
  skip_on_cran()
  df_na <- bind_rows(base_df, mutate(base_df, TG = NA_real_))
  expect_error(
    suppressWarnings(adipo_is(df_na,
                              col_map = list(G0="G0",I0="I0",TG="TG",HDL_c="HDL_c",FFA="FFA",waist="waist",bmi="bmi"),
                              na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(adipo_is(df_na,
                                        col_map = list(G0="G0",I0="I0",TG="TG",HDL_c="HDL_c",FFA="FFA",waist="waist",bmi="bmi"),
                                        na_action = "omit"))
  expect_equal(nrow(out_omit), 1L)
})

test_that("extreme inputs produce NA in sensitive outputs", {
  skip_on_cran()
  df_ext <- mutate(base_df, HDL_c = 0, TG = 0)
  out_zero <- run_adipo(df_ext, verbose = FALSE)
  expect_true(is.na(out_zero$TG_HDL_C_inv))
})

test_that("verbose = TRUE emits column map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(run_adipo(base_df, verbose = TRUE), "col_map")
  expect_message(run_adipo(base_df, verbose = TRUE), "adipo_is\\(\\): results")
})

test_that("verbose column map lists all required keys", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(run_adipo(base_df, verbose = TRUE))
  map_msgs <- msgs[grepl("col_map", msgs)]
  expect_gte(length(map_msgs), 1L)
  all_map_text <- paste(map_msgs, collapse = " ")
  for (key in c("G0", "I0", "TG", "HDL_c", "FFA", "waist", "bmi")) {
    expect_true(grepl(key, all_map_text), info = paste("key missing from column map msg:", key))
  }
})

test_that("verbose results line reports correct non-NA counts", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  # base_df has 1 complete row -> expect all columns show 1/1
  msgs <- testthat::capture_messages(run_adipo(base_df, verbose = TRUE))
  res_msg <- msgs[grepl("results:", msgs)]
  expect_length(res_msg, 1L)
  expect_true(grepl("Revised_QUICKI 1/1", res_msg))
  expect_true(grepl("McAuley_index 1/1", res_msg))
})

test_that("verbose = TRUE with inform option emits messages exactly once (no double-fire)", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(run_adipo(base_df, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",      msgs)), 1L)
})

test_that("verbose = FALSE suppresses messages even at inform global level", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_no_message(run_adipo(base_df, verbose = FALSE))
})
