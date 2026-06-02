# tests/testthat/test-psych_markers.R

library(testthat)
library(tibble)

mini_df <- tibble(
  phq9_01 = 0, phq9_02 = 1, phq9_03 = 2, phq9_04 = 1, phq9_05 = 1, phq9_06 = 1, phq9_07 = 1, phq9_08 = 1, phq9_09 = 0,
  gad7_01 = 0, gad7_02 = 0, gad7_03 = 0, gad7_04 = 1, gad7_05 = 1, gad7_06 = 0, gad7_07 = 0,
  k6_01 = 1, k6_02 = 1, k6_03 = 1, k6_04 = 1, k6_05 = 1, k6_06 = 1,
  k10_01 = 1, k10_02 = 1, k10_03 = 1, k10_04 = 1, k10_05 = 1, k10_06 = 1, k10_07 = 1, k10_08 = 1, k10_09 = 1, k10_10 = 1,
  ghq12_01 = 0, ghq12_02 = 1, ghq12_03 = 1, ghq12_04 = 0, ghq12_05 = 1, ghq12_06 = 0,
  ghq12_07 = 0, ghq12_08 = 0, ghq12_09 = 1, ghq12_10 = 0, ghq12_11 = 1, ghq12_12 = 0,
  who5_01 = 4, who5_02 = 4, who5_03 = 4, who5_04 = 4, who5_05 = 4,
  isi_01 = 0, isi_02 = 0, isi_03 = 0, isi_04 = 0, isi_05 = 0, isi_06 = 0, isi_07 = 0,
  mdq_01 = 1, mdq_02 = 1, mdq_03 = 1, mdq_04 = 1, mdq_05 = 1, mdq_06 = 1, mdq_07 = 1, mdq_08 = 1, mdq_09 = 1, mdq_10 = 1, mdq_11 = 1, mdq_12 = 1, mdq_13 = 1,
  mdq_cluster = 1, mdq_impair = 1,
  asrs_01 = 3, asrs_02 = 3, asrs_03 = 3, asrs_04 = 3, asrs_05 = 3, asrs_06 = 3,
  asrs_07 = 1, asrs_08 = 1, asrs_09 = 1, asrs_10 = 1, asrs_11 = 1, asrs_12 = 1,
  asrs_13 = 1, asrs_14 = 1, asrs_15 = 1, asrs_16 = 1, asrs_17 = 1, asrs_18 = 1,
  bis_01 = 1, bis_02 = 2, bis_03 = 3, bis_04 = 4, bis_05 = 2, bis_06 = 1,
  bis_07 = 2, bis_08 = 2, bis_09 = 2, bis_10 = 2, bis_11 = 2, bis_12 = 2,
  spq_01 = 1, spq_02 = 1, spq_03 = 1, spq_04 = 1,
  task_rt = 350, task_mem = 0.7, task_att = 0.5,
  dx_mdd = 0, dx_anxiety = 0, dx_adhd = 0, dx_bipolar = 0, dx_scz = 0, dx_sud = 0,
  med_ssri = 0, med_snri = 0, med_antipsychotic = 0, med_mood_stabilizer = 0, med_anxiolytic = 0
)

col_map_all <- list(
  phq9 = list(items = setNames(sprintf("phq9_%02d", 1:9), sprintf("phq9_%02d", 1:9))),
  gad7 = list(items = setNames(sprintf("gad7_%02d", 1:7), sprintf("gad7_%02d", 1:7))),
  k6   = list(items = setNames(sprintf("k6_%02d", 1:6), sprintf("k6_%02d", 1:6))),
  k10  = list(items = setNames(sprintf("k10_%02d", 1:10), sprintf("k10_%02d", 1:10))),
  ghq12 = list(items = setNames(sprintf("ghq12_%02d", 1:12), sprintf("ghq12_%02d", 1:12))),
  who5 = list(items = setNames(sprintf("who5_%02d", 1:5), sprintf("who5_%02d", 1:5))),
  isi  = list(items = setNames(sprintf("isi_%02d", 1:7), sprintf("isi_%02d", 1:7))),
  mdq  = list(items = setNames(sprintf("mdq_%02d", 1:13), sprintf("mdq_%02d", 1:13)), cluster = "mdq_cluster", impairment = "mdq_impair"),
  asrs = list(items = setNames(sprintf("asrs_%02d", 1:18), sprintf("asrs_%02d", 1:18))),
  bis  = list(items = setNames(sprintf("bis_%02d", 1:12), sprintf("bis_%02d", 1:12))),
  spq  = list(items = setNames(sprintf("spq_%02d", 1:4), sprintf("spq_%02d", 1:4))),
  cognitive = list(tasks = list(rt = "task_rt", memory = "task_mem", attention = "task_att")),
  dx_flags = list(dx = list(mdd = "dx_mdd", anxiety = "dx_anxiety", adhd = "dx_adhd", bipolar = "dx_bipolar", scz = "dx_scz", sud = "dx_sud")),
  med_flags = list(med = list(ssri = "med_ssri", snri = "med_snri", antipsychotic = "med_antipsychotic", mood_stabilizer = "med_mood_stabilizer", anxiolytic = "med_anxiolytic"))
)

bis_key_demo <- list(
  name = "BIS_demo",
  items = sprintf("bis_%02d", 1:12),
  min_val = 1, max_val = 4,
  reverse = c("bis_02","bis_03"),
  subscales = list(motor = c("bis_02","bis_06"))
)

spq_key_demo <- list(
  name = "SPQ_demo",
  items = sprintf("spq_%02d", 1:4),
  min_val = 0, max_val = 1
)

test_that("psych_markers returns combined outputs for requested scales", {
  out <- psych_markers(
    mini_df,
    col_map = col_map_all,
    which = c("phq9","gad7","k6","ghq12_binary","who5","isi","mdq","asrs","bis","spq","cognitive","dx_flags","med_flags"),
    bis_key = bis_key_demo,
    spq_key = spq_key_demo,
    na_action = "keep",
    missing_prop_max = 0.3,
    impute = "mean"
  )

  expect_true("PHQ9_total" %in% names(out))
  expect_true("GAD7_total" %in% names(out))
  expect_true("K6_total" %in% names(out))
  expect_true("GHQ12_total_binary" %in% names(out))
  expect_true("WHO5_percent" %in% names(out))
  expect_true("ISI_total" %in% names(out))
  expect_true("MDQ_positive_screen" %in% names(out))
  expect_true("ASRS_partA_positive" %in% names(out))
  expect_true("BIS_total" %in% names(out))
  expect_true("SPQ_total" %in% names(out))
  expect_true("cog_z_mean" %in% names(out))
  expect_true("dx_any_psych" %in% names(out))
  expect_true("med_any_psych" %in% names(out))
})

test_that("missingness policy propagate NA when too much is missing", {
  skip_on_cran()
  df_na <- mini_df
  df_na$phq9_01 <- NA
  df_na$phq9_02 <- NA
  out <- phq9_score(df_na, col_map = col_map_all$phq9, missing_prop_max = 0.1, impute = "none", na_action = "keep")
  expect_true(is.na(out$PHQ9_total))
})

test_that("psych_markers verbose emits preparing and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(psych_markers(mini_df, which = "phq9", verbose = TRUE), "psych_markers")
  expect_message(psych_markers(mini_df, which = "phq9", verbose = TRUE), "results:")
})

test_that("psych_markers verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(psych_markers(mini_df, which = "phq9", verbose = TRUE))
  expect_equal(sum(grepl("results:", msgs)), 1L)
})
