library(miRetrieve)
library(testthat)

toy_df <- data.frame("PMID" = c(34051305, 34033143, 34032694, 34028994, 34024846,
                                34017372, 34016957, 34014023, 34009437, 34009437,
                                34007244, 34006268, 34006268, 1, 2, 3),
                     "miRNA_" = c("miR-16", "miR-1", "miR-96", "miR-205",
                                 "miR-125", "miR-423", "let-7", "miR-17",
                                 "miR-195", "miR-27", "miR-6869", "miR-1275",
                                 "miR-181", "miR-181", "miR-181", "miR-181"))

df_mir_count <- count_mir(toy_df,
                          col.mir = miRNA_)

test_that("Tests that miRNAs are counted in a data frame", {
    expect_equal(typeof(df_mir_count), "list")
    expect_equal(ncol(df_mir_count), 2)
    expect_equal(df_mir_count[df_mir_count$miRNA_ == "miR-181", ]$Mentioned_n, 4)
    expect_equal(df_mir_count[df_mir_count$miRNA_ == "miR-16", ]$Mentioned_n, 1)
})

df_mir_count_thresh_abs <- count_mir_threshold(toy_df,
                                               col.mir = miRNA_,
                                               threshold = 2)

df_mir_count_thresh_abs_none <- count_mir_threshold(toy_df,
                                               col.mir = miRNA_,
                                               threshold = 1)

df_mir_count_thresh_rel <- count_mir_threshold(toy_df,
                                               col.mir = miRNA_,
                                               threshold = 0.2)

df_mir_count_thresh_rel_none <- count_mir_threshold(toy_df,
                                               col.mir = miRNA_,
                                               threshold = 0.0002)

test_that("Tests that miRNAs are counted based on threshold", {
    expect_equal(typeof(df_mir_count_thresh_abs), "integer")
    expect_equal(typeof(df_mir_count_thresh_rel), "integer")
    expect_equal(length(df_mir_count_thresh_abs), 1)
    expect_equal(length(df_mir_count_thresh_rel), 1)
    expect_gte(df_mir_count_thresh_abs_none, df_mir_count_thresh_abs)
    expect_gte(df_mir_count_thresh_rel_none, df_mir_count_thresh_rel)
})
