test_that("plot_QC_Tree generates the correct plot", {

  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta <- data.table(read_csv(test_path("exttestdata",
                                        "102623_metadata_correct.csv"),
                              show_col_types = FALSE))
  pt_list <- progenesis_formatter(test_path("exttestdata", peak_table_name))

  mpactr_class <- mpactr$new(
    pt_list,
    meta
  )
  mpactr_class$setup()
  filter_class <- filter_pactr$new(mpactr_class)
  filter_class$check_mismatched_peaks(
    ringwin = 0.5,
    isowin = 0.01,
    trwin = 0.005,
    max_iso_shift = 3,
    merge_peaks = TRUE,
    merge_method = "sum"
  )
  filter_class$filter_blank()
  filter_class$parse_ions_by_group(group_threshold = 0.01)
  filter_class$apply_group_filter("Blanks", remove_ions = TRUE)
  filter_class$filter_insource_ions(cluster_threshold = 0.95)

  graph_qc_pactr_class <- graph_qc_pactr$new(filter_class)
  graph_qc_pactr_class$generate_QC_Summary()
  plot <- graph_qc_pactr_class$plot_QC_Tree()

  expect_equal(class(plot), c("gg", "ggplot"))
})
