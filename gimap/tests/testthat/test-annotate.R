test_that("Annotation options", {
  testthat::skip_on_cran()
  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA")

  # We should see these columns
  testthat::expect_true(all(c(
    "norm_ctrl_flag",
    "log2_tpm_gene1", "log2_tpm_gene2",
    "log2_cn_gene1", "log2_cn_gene2",
    "gene1_expressed_flag", "gene2_expressed_flag"
  ) %in%
    colnames(gimap_dataset$annotation)))

  # It should warn you if you try to say FALSE for cell line_annotate but
  # don't provide a custom_tpm or use normalize_by_unexpressed = FALSE
  testthat::expect_error(
    gimap_dataset <- get_example_data("gimap") %>%
      gimap_filter() %>%
      gimap_annotate(cell_line_annotate = FALSE) %>%
      gimap_normalize(timepoints = "day")
  )

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line_annotate = FALSE) %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = FALSE
    )

  # We should see these columns
  testthat::expect_true(all(c("lfc", "crispr_score", "norm_ctrl_flag") %in%
    colnames(gimap_dataset$normalized_log_fc)))

  ## Try out using custom TPM data
  custom_tpm <- vroom::vroom(
    file.path(
      system.file("extdata", package = "gimap"),
      "CCLE_expression.csv"
    ),
    show_col_types = FALSE,
    col_select = c("genes", "ACH-001086")
  ) %>%
    dplyr::rename(log2_tpm = `ACH-001086`)

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(
      custom_tpm = custom_tpm,
      cell_line = "HELA"
    )

  gimap_dataset <- gimap_dataset %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = FALSE,
      overwrite = TRUE
    )

  # We should see these columns
  testthat::expect_true(all(c("lfc", "crispr_score", "norm_ctrl_flag") %in%
    colnames(gimap_dataset$normalized_log_fc)))
})
