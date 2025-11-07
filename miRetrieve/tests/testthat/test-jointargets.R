library(miRetrieve)
library(testthat)

excel_target <- "excel_target_test.xlsx"

set.seed(42)

toy_df <- data.frame("PMID" = sample(seq(1:5), size = 16, replace = TRUE),
                     "miRNA_" = sample(c("miR-20", "miR-181", "miR-24", "miR-34"),
                                       size = 16, replace = TRUE))

df_join <- join_targets(toy_df,
                        excel_target,
                        col.pmid.excel = "PMID",
                        col.target.excel = "Target")

test_that("Tests that targets can simply be joined from excel", {
    expect_equal(typeof(df_join), "list")
    expect_gte(ncol(df_join), ncol(toy_df))
})

df_join_mir <- join_targets(toy_df,
                        excel_target,
                        col.pmid.excel = "PMID",
                        col.target.excel = "Target",
                        col.mir.excel = "miRNA")

test_that("Tests that miRNAs from excel targets can be joined from excel", {
    expect_equal(typeof(df_join_mir), "list")
    expect_gte(ncol(df_join_mir), ncol(df_join))
})

df_join_mir_stem <- join_targets(toy_df,
                                 excel_target,
                                 col.pmid.excel = "PMID",
                                 col.target.excel = "Target",
                                 col.mir.excel = "miRNA",
                                 stem_mir_excel = TRUE)

reduced_mir <- df_join_mir_stem[["miRNA_excel"]][grepl("\\d[a-z]",
                                                       df_join_mir_stem[["miRNA_excel"]])]

test_that("Tests that miRNAs from excel are correctly trimmed", {
    expect_equal(length(reduced_mir), 0)
})

df_join_mir_red <- join_targets(toy_df,
                                excel_target,
                                col.pmid.excel = "PMID",
                                col.target.excel = "Target",
                                col.mir.excel = "miRNA",
                                reduce = TRUE)

test_that("Tests that excel targets are reduced", {
    expect_equal(typeof(df_join_mir_red), "list")
    expect_lte(nrow(df_join_mir_red), nrow(df_join))
})

plot_target <- plot_target_count(df_join_mir_red,
                                 title = "Test_target")

plot_target_top <- plot_target_count(df_join_mir_red,
                                     top = 1)

plot_target_thresh <- plot_target_count(df_join_mir_red,
                                     threshold = 2)

test_that("Tests that targets can be plotted", {
    expect_equal(typeof(plot_target), "list")
    expect_equal(plot_target$labels$title, "Test_target")
    expect_lte(length(plot_target_top$data$Target), length(plot_target$data$Target))
    expect_lte(length(plot_target_thresh$data$Target), length(plot_target$data$Target))
})

plot_target_scatter <- plot_target_mir_scatter(df_join_mir_red,
                                               col.mir = miRNA_excel)

plot_target_scatter_mir <- plot_target_mir_scatter(df_join_mir_red,
                                                   mir = "miR-20",
                                                   col.mir = miRNA_excel)

plot_target_scatter_target <- plot_target_mir_scatter(df_join_mir_red,
                                                      target = "TARGET1",
                                                      col.mir = miRNA_excel)

plot_target_scatter_target_top <- plot_target_mir_scatter(df_join_mir_red,
                                                          top = 1,
                                                          col.mir = miRNA_excel)

plot_target_scatter_mir_top_filter <- plot_target_mir_scatter(df_join_mir_red,
                                                                 top = 1,
                                                                 filter_for = "miRNA",
                                                                 col.mir = miRNA_excel)

plot_target_scatter_target_thresh_filter <- plot_target_mir_scatter(df_join_mir_red,
                                                                    threshold = 3,
                                                                    filter_for = "miRNA",
                                                                    col.mir = miRNA_excel)

test_that("Tests target/miRNA scatter plot functions", {
    expect_s3_class(plot_target_scatter, "ggplot")
    expect_s3_class(plot_target_scatter_mir, "ggplot")
    expect_s3_class(plot_target_scatter_target, "ggplot")
    expect_s3_class(plot_target_scatter_target_top, "ggplot")
    expect_s3_class(plot_target_scatter_mir_top_filter, "ggplot")
    expect_s3_class(plot_target_scatter_target_thresh_filter, "ggplot")
    expect_lte(length(plot_target_scatter_mir$data$miRNA),
               length(plot_target_scatter$data$miRNA))
    expect_lte(length(plot_target_scatter_target$data$Target),
               length(plot_target_scatter$data$Target))
    expect_lte(length(plot_target_scatter_target_top$data$Target),
               length(plot_target_scatter$data$Target))
    expect_lte(length(plot_target_scatter_mir_top_filter$data$miRNA),
               length(plot_target_scatter$data$miRNA))
    expect_lte(length(plot_target_scatter_target_thresh_filter$data$Target),
               length(plot_target_scatter$data$Target))

})
