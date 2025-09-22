test_that("qc_summary returns a summary data.table", {
  data <- import_data(test_path("exttestdata",
                                "102623_peaktable_coculture_simple.csv"),
    test_path("exttestdata", "102623_metadata_correct.csv"),
    format = "Progenesis"
  )

  data_mpactr <- filter_mispicked_ions(data,
    ringwin = 0.5,
    isowin = 0.01,
    trwin = 0.005,
    max_iso_shift = 3,
    merge_peaks = TRUE
  )
  data_mpactr <- filter_group(data, 0.01, "Blanks", TRUE)
  data_mpactr <- filter_group(data, 0.01, "Coculture", TRUE)
  data_mpactr <- filter_insource_ions(data, cluster_threshold = 0.95)

  data_mpactr_summary <- qc_summary(data)
  expect_true(nrow(data_mpactr_summary) > 1)
  expect_equal(unique(data_mpactr_summary$status), c(
    "Passed", "mispicked", "group-Blanks",
    "group-Coculture", "insource"
  ))
})

test_that("qc plot returns a generates a plot", {
  data <- import_data(test_path("exttestdata",
                                "102623_peaktable_coculture_simple.csv"),
    test_path("exttestdata", "102623_metadata_correct.csv"),
    format = "Progenesis"
  )

  data_mpactr <- filter_mispicked_ions(data,
    ringwin = 0.5,
    isowin = 0.01,
    trwin = 0.005,
    max_iso_shift = 3,
    merge_peaks = TRUE
  )
  data_mpactr <- filter_group(data, 0.01, "Blanks", TRUE)
  data_mpactr <- filter_insource_ions(data, cluster_threshold = 0.95)
  plot <- plot_qc_tree(data)
  expect_true(inherits(plot, c("ggplot", "ggplot2::ggplot")))
})
