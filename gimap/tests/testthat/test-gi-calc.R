test_that("Test Genetic Interaction score calculations", {
  testthat::skip_on_cran()
  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi()

  results <- data.frame(
    rep = c("Day05_RepA_early", "Day22_RepA_late", "Day22_RepB_late", "Day22_RepC_late"),
    intercept = as.numeric(round(c(-0.5110, 0.00210, 0.0105, 0.00501), 3)),
    slope = as.numeric(round(c(0.483, 0.659, 0.646, 0.650), 3))
  )

  testthat::expect_type(gimap_dataset$linear_model, "list")

  testthat::expect_identical(
    round(gimap_dataset$gi_scores$mean_expected_cs[1], 3),
    round(-0.4160, 3)
  )
  testthat::expect_identical(
    round(gimap_dataset$gi_scores$mean_observed_cs[1], 3),
    round(-0.5490, 3)
  )
  testthat::expect_identical(
    round(gimap_dataset$gi_scores$gi_score[1], 3),
    round(-0.1470, 3)
  )
  testthat::expect_identical(
    round(gimap_dataset$gi_scores$p_val[1], 3),
    round(0.2050, 3)
  )
})


test_that("Test Genetic Interaction score calculations using LFC", {
  testthat::skip_on_cran()
  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      adj_method = "no_adjustment",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi(use_lfc = TRUE)

  testthat::expect_true(class(gimap_dataset)[1] == "list")
})


test_that("Test Genetic Interaction score without normalization", {
  testthat::skip_on_cran()
  gimap_dataset_wo <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      normalize_by_unexpressed = FALSE,
      timepoints = "day",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi()

  gimap_dataset_w <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      normalize_by_unexpressed = TRUE,
      timepoints = "day",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi()


  # Are the GI scores the same? No
  testthat::expect_false(
    all(gimap_dataset_wo$gi_scores$gi_score == gimap_dataset_w$gi_scores$gi_score)
  )

  # Are the log fold change values the same? No
  testthat::expect_false(
    all(gimap_dataset_wo$normalized_log_fc$lfc == gimap_dataset_w$normalized_log_fc$lfc)
  )

  # Are the CRISPR scores the same? No
  testthat::expect_false(
    all(gimap_dataset_wo$normalized_log_fc$crispr_score == gimap_dataset_w$normalized_log_fc$crispr_score)
  )

  table(
    gimap_dataset_w$normalized_log_fc$unexpressed_ctrl_flag,
    gimap_dataset_w$normalized_log_fc$norm_ctrl_flag
  )
})
