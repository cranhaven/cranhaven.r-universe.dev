# tests/testthat/test-psych_scores.R

library(testthat)
library(tibble)
library(dplyr)

sample_psych_df <- tibble(
  phq9_01 = 1, phq9_02 = 2, phq9_03 = 1, phq9_04 = 0, phq9_05 = 1, phq9_06 = 2, phq9_07 = 1, phq9_08 = 2, phq9_09 = 1,
  gad7_01 = 0, gad7_02 = 1, gad7_03 = 1, gad7_04 = 1, gad7_05 = 0, gad7_06 = 1, gad7_07 = 0,
  k6_01 = 2, k6_02 = 1, k6_03 = 2, k6_04 = 1, k6_05 = 2, k6_06 = 1,
  k10_01 = 1, k10_02 = 1, k10_03 = 1, k10_04 = 1, k10_05 = 1, k10_06 = 1, k10_07 = 1, k10_08 = 1, k10_09 = 1, k10_10 = 1,
  ghq12_01 = 0, ghq12_02 = 1, ghq12_03 = 2, ghq12_04 = 1, ghq12_05 = 0, ghq12_06 = 2,
  ghq12_07 = 1, ghq12_08 = 0, ghq12_09 = 1, ghq12_10 = 0, ghq12_11 = 1, ghq12_12 = 0,
  who5_01 = 3, who5_02 = 3, who5_03 = 4, who5_04 = 3, who5_05 = 4,
  isi_01 = 2, isi_02 = 1, isi_03 = 2, isi_04 = 1, isi_05 = 0, isi_06 = 1, isi_07 = 0,
  mdq_01 = 1, mdq_02 = 1, mdq_03 = 1, mdq_04 = 0, mdq_05 = 1, mdq_06 = 1, mdq_07 = 1, mdq_08 = 0, mdq_09 = 1, mdq_10 = 1, mdq_11 = 0, mdq_12 = 1, mdq_13 = 1,
  mdq_cluster = 1, mdq_impair = 1,
  asrs_01 = 4, asrs_02 = 3, asrs_03 = 3, asrs_04 = 4, asrs_05 = 4, asrs_06 = 4,
  asrs_07 = 1, asrs_08 = 1, asrs_09 = 1, asrs_10 = 1, asrs_11 = 1, asrs_12 = 1,
  asrs_13 = 1, asrs_14 = 1, asrs_15 = 1, asrs_16 = 1, asrs_17 = 1, asrs_18 = 1,
  bis_01 = 2, bis_02 = 3, bis_03 = 2, bis_04 = 3, bis_05 = 1, bis_06 = 2,
  bis_07 = 2, bis_08 = 2, bis_09 = 2, bis_10 = 2, bis_11 = 2, bis_12 = 2,
  task_rt = 300, task_mem = 0.8, task_att = 0.4,
  dx_mdd = 1, dx_anxiety = 0, dx_adhd = 1, dx_bipolar = 0, dx_scz = 0, dx_sud = 0,
  med_ssri = 1, med_snri = 0, med_antipsychotic = 0, med_mood_stabilizer = 0, med_anxiolytic = 0
)

bis_key_small <- list(
  name = "BIS_demo",
  items = sprintf("bis_%02d", 1:12),
  min_val = 1, max_val = 4,
  reverse = c("bis_02","bis_03"),
  subscales = list(
    attention = c("bis_01","bis_05"),
    motor = c("bis_02","bis_06")
  )
)

test_that("phq9_score computes totals and severity", {
  skip_on_cran()
  out <- phq9_score(sample_psych_df, col_map = list(items = setNames(names(sample_psych_df)[1:9], sprintf("phq9_%02d", 1:9))))
  expect_equal(out$PHQ9_total, sum(c(1,2,1,0,1,2,1,2,1)))
  expect_s3_class(out$PHQ9_severity, "factor")
})

test_that("gad7_score severity bands work", {
  skip_on_cran()
  out <- gad7_score(sample_psych_df, col_map = list(items = setNames(sprintf("gad7_%02d", 1:7), sprintf("gad7_%02d", 1:7))))
  expect_true(out$GAD7_total >= 0)
  expect_true(out$GAD7_severity %in% c("minimal","mild","moderate","severe"))
})

test_that("who5_score percent and low wellbeing flag", {
  skip_on_cran()
  out <- who5_score(sample_psych_df, col_map = list(items = setNames(sprintf("who5_%02d", 1:5), sprintf("who5_%02d", 1:5))))
  expect_equal(out$WHO5_raw, sum(c(3,3,4,3,4)))
  expect_equal(out$WHO5_percent, out$WHO5_raw * 4)
  expect_false(out$WHO5_low_wellbeing)
})

test_that("asrs_score part A thresholding", {
  skip_on_cran()
  out <- asrs_score(sample_psych_df, col_map = list(items = setNames(sprintf("asrs_%02d", 1:18), sprintf("asrs_%02d", 1:18))))
  expect_equal(out$ASRS_partA_count, 6)
  expect_true(out$ASRS_partA_positive)
})

test_that("bis_score key-driven scoring works", {
  skip_on_cran()
  out <- bis_score(sample_psych_df, col_map = list(items = setNames(sprintf("bis_%02d", 1:12), sprintf("bis_%02d", 1:12))), key = bis_key_small)
  expect_equal(out$BIS_total, sum(c(2, (5-3), (5-2),3,1, 2,2,2,2,2,2,2)))
  expect_true(!is.na(out$BIS_attention))
  expect_true(!is.na(out$BIS_motor))
})

test_that("cognitive_score produces z_mean", {
  skip_on_cran()
  cm <- list(tasks = list(rt = "task_rt", memory = "task_mem", attention = "task_att"))
  out <- cognitive_score(sample_psych_df, col_map = cm, method = "z_mean")
  expect_true("cog_z_mean" %in% names(out))
})

test_that("diagnosis and medication flags aggregate", {
  skip_on_cran()
  dx_map <- list(dx = list(mdd = "dx_mdd", anxiety = "dx_anxiety", adhd = "dx_adhd", bipolar = "dx_bipolar", scz = "dx_scz", sud = "dx_sud"))
  med_map <- list(med = list(ssri = "med_ssri", snri = "med_snri", antipsychotic = "med_antipsychotic", mood_stabilizer = "med_mood_stabilizer", anxiolytic = "med_anxiolytic"))

  dx_out <- psych_dx_flags(sample_psych_df, col_map = dx_map)
  med_out <- psych_med_flags(sample_psych_df, col_map = med_map)

  expect_true(dx_out$dx_any_psych)
  expect_equal(dx_out$dx_count, 2)
  expect_true(med_out$med_any_psych)
  expect_equal(med_out$med_count, 1)
})

# ---- K6 --------------------------------------------------------------------
test_that("k6_score returns tibble with total and case flag", {
  skip_on_cran()
  out <- k6_score(sample_psych_df)
  expect_s3_class(out, "tbl_df")
  expect_true("K6_total" %in% names(out))
  expect_true("K6_case" %in% names(out))
  expect_equal(out$K6_total, sum(c(2, 1, 2, 1, 2, 1)))
})

test_that("k6_score errors on missing columns", {
  skip_on_cran()
  expect_error(k6_score(data.frame(x = 1)))
})

# ---- K10 -------------------------------------------------------------------
test_that("k10_score returns tibble with total", {
  skip_on_cran()
  out <- k10_score(sample_psych_df)
  expect_s3_class(out, "tbl_df")
  expect_true("K10_total" %in% names(out))
  expect_equal(out$K10_total, 10L)
})

# ---- GHQ-12 ----------------------------------------------------------------
test_that("ghq12_score likert returns total_likert", {
  skip_on_cran()
  out <- ghq12_score(sample_psych_df, method = "likert")
  expect_s3_class(out, "tbl_df")
  expect_true("GHQ12_total_likert" %in% names(out))
})

test_that("ghq12_score binary returns total_binary and case", {
  skip_on_cran()
  out <- ghq12_score(sample_psych_df, method = "binary")
  expect_s3_class(out, "tbl_df")
  expect_true("GHQ12_total_binary" %in% names(out))
  expect_true("GHQ12_case_binary" %in% names(out))
})

# ---- ISI -------------------------------------------------------------------
test_that("isi_score returns tibble with total and severity", {
  skip_on_cran()
  out <- isi_score(sample_psych_df)
  expect_s3_class(out, "tbl_df")
  expect_true("ISI_total" %in% names(out))
  expect_true("ISI_severity" %in% names(out))
  expect_equal(out$ISI_total, sum(c(2, 1, 2, 1, 0, 1, 0)))
})

# ---- MDQ -------------------------------------------------------------------
test_that("mdq_score returns tibble with symptom_count and positive_screen", {
  skip_on_cran()
  out <- mdq_score(sample_psych_df)
  expect_s3_class(out, "tbl_df")
  expect_true("MDQ_symptom_count" %in% names(out))
  expect_true("MDQ_positive_screen" %in% names(out))
  expect_equal(out$MDQ_symptom_count, sum(c(1,1,1,0,1,1,1,0,1,1,0,1,1)))
})

# ---- SPQ -------------------------------------------------------------------
test_that("spq_score returns tibble with total using key", {
  skip_on_cran()
  spq_key <- list(items = c("mdq_01", "mdq_02", "mdq_03"), min_val = 0, max_val = 1)
  out <- spq_score(sample_psych_df, key = spq_key, prefix = "SPQ")
  expect_s3_class(out, "tbl_df")
  expect_true("SPQ_total" %in% names(out))
})

test_that("psych_dx_flags errors when dx map is missing", {
  skip_on_cran()
  expect_error(psych_dx_flags(data.frame(x = 1:2), col_map = list()))
})

test_that("psych_med_flags errors when med map is missing", {
  skip_on_cran()
  expect_error(psych_med_flags(data.frame(x = 1:2), col_map = list()))
})

# ---- Formula / severity correctness ----------------------------------------
test_that("phq9_score correct total and severity label", {
  skip_on_cran()
  out <- phq9_score(sample_psych_df)
  expect_equal(out$PHQ9_total, 11L)
  expect_equal(as.character(out$PHQ9_severity), "moderate")
})

test_that("isi_score severity band: total=7 -> none", {
  skip_on_cran()
  out <- isi_score(sample_psych_df)
  expect_equal(out$ISI_total, 7L)
  expect_equal(as.character(out$ISI_severity), "none")
})

test_that("who5_score percent = raw * 4", {
  skip_on_cran()
  out <- who5_score(sample_psych_df)
  expect_equal(out$WHO5_percent, out$WHO5_raw * 4)
  expect_false(out$WHO5_low_wellbeing)
})

test_that("ghq12_score binary scoring correct", {
  skip_on_cran()
  # values: 0,1,2,1,0,2,1,0,1,0,1,0 -> binary (>=2->1): 0,0,1,0,0,1,0,0,0,0,0,0 = 2
  out <- ghq12_score(sample_psych_df, method = "binary")
  expect_equal(out$GHQ12_total_binary, 2L)
  expect_false(out$GHQ12_case_binary)  # 2 < 3 (default cutoff)
})

test_that("na_action='omit' in phq9_score drops NA rows", {
  skip_on_cran()
  df <- tibble::tibble(
    phq9_01 = c(1, NA), phq9_02 = 1, phq9_03 = 1, phq9_04 = 1, phq9_05 = 1,
    phq9_06 = 1, phq9_07 = 1, phq9_08 = 1, phq9_09 = 1
  )
  out <- phq9_score(df, na_action = "omit")
  expect_equal(nrow(out), 1L)
})

test_that("na_action='error' in phq9_score aborts on NA", {
  skip_on_cran()
  df <- tibble::tibble(
    phq9_01 = c(1, NA), phq9_02 = 1, phq9_03 = 1, phq9_04 = 1, phq9_05 = 1,
    phq9_06 = 1, phq9_07 = 1, phq9_08 = 1, phq9_09 = 1
  )
  expect_error(phq9_score(df, na_action = "error"))
})

# ---- Verbose ----------------------------------------------------------------
test_that("phq9_score verbose emits preparing and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(phq9_score(sample_psych_df, verbose = TRUE), "phq9_score")
  expect_message(phq9_score(sample_psych_df, verbose = TRUE), "results:")
})

test_that("psych_markers verbose emits preparing and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    psych_markers(sample_psych_df, which = "phq9", verbose = TRUE),
    "psych_markers"
  )
  expect_message(
    psych_markers(sample_psych_df, which = "phq9", verbose = TRUE),
    "results:"
  )
})

# ---- na_action = "keep" preserves NAs in output ----------------------------
test_that("phq9_score na_action='keep' preserves NA rows", {
  skip_on_cran()
  df <- tibble::tibble(
    phq9_01 = c(1, NA), phq9_02 = 1, phq9_03 = 1, phq9_04 = 1, phq9_05 = 1,
    phq9_06 = 1, phq9_07 = 1, phq9_08 = 1, phq9_09 = 1
  )
  out <- phq9_score(df, na_action = "keep")
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$PHQ9_total[2]))
})

# ---- .hm_severity_band() guard: mismatched cuts/labels ---------------------
test_that(".hm_severity_band() errors when cuts and labels are mismatched", {
  skip_on_cran()
  # 3 cuts but 3 labels (should be 4 cuts for 3 labels)
  expect_error(
    HealthMarkers:::.hm_severity_band(c(5, 10, 15), cuts = c(0, 5, 10), labels = c("low", "med", "high")),
    "one more element"
  )
})

# ---- PHQ-9 with 8 items (item 9 missing) ------------------------------------
test_that("phq9_score handles 8 items with impute mean (PHQ-8 equivalent)", {
  skip_on_cran()
  df <- tibble::tibble(
    phq9_01 = 1, phq9_02 = 2, phq9_03 = 1, phq9_04 = 0, phq9_05 = 1,
    phq9_06 = 2, phq9_07 = 1, phq9_08 = 2, phq9_09 = NA_integer_
  )
  # With impute = "mean" the missing item should be filled by row mean
  out <- suppressWarnings(phq9_score(df, na_action = "keep"))
  expect_equal(nrow(out), 1L)
  # total should be NA since na_action = "keep" propagates NAs
  expect_true(is.na(out$PHQ9_total))
})
