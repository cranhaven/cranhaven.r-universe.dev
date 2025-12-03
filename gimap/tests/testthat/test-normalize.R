test_that("Test normalization", {
  testthat::skip_on_cran()
  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      missing_ids_file = tempfile()
    )

  # make sure the important columns are there
  testthat::expect_true(
    all(c("target_type", "lfc", "rep", "crispr_score", "unexpressed_ctrl_flag")
    %in% colnames(gimap_dataset$normalized_log_fc))
  )

  neg_controls <- gimap_dataset$normalized_log_fc %>%
    dplyr::filter(norm_ctrl_flag == "negative_control") %>%
    dplyr::group_by(rep) %>%
    dplyr::summarize(neg_ctrl_med = median(crispr_score)) %>%
    dplyr::pull(neg_ctrl_med)

  # We expect negative controls to be now equal to 0
  testthat::expect_equal(neg_controls[1:3], c(0, 0, 0))

  pos_controls <- gimap_dataset$normalized_log_fc %>%
    dplyr::filter(norm_ctrl_flag == "positive_control") %>%
    dplyr::group_by(rep) %>%
    dplyr::summarize(pos_ctrl_med = median(crispr_score)) %>%
    dplyr::pull(pos_ctrl_med)

  # We expect positive controls to be now equal to -1
  testthat::expect_equal(
    round(pos_controls),
    round(c(-1, -1, -1))
  )
})

test_that("Test normalization without expression cutoff", {
  testthat::skip_on_cran()

  gimap_dataset_true <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = TRUE,
      missing_ids_file = tempfile()
    )

  gimap_dataset_false <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = FALSE,
      missing_ids_file = tempfile()
    )

  testthat::expect_true(
    all(gimap_dataset_true$normalized_log_fc$lfc[1:6] !=
          gimap_dataset_false$normalized_log_fc$lfc[1:6]))

})
