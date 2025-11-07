library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("miRNA_" = sample(c("miR-24", "miR-25", "miR-26"),
                                       size = 20,
                                       replace = TRUE),
                     "PMID" = seq(1:20))

subset_df <- subset_mir(toy_df,
                        mir.retain = "miR-24",
                        col.mir = miRNA_)

test_that("Tests subsetting data.frames for miRNAs", {
    expect_equal(nrow(subset_df), 9)
})

subset_df_abs <- subset_mir_threshold(toy_df,
                                      col.mir = miRNA_,
                                      threshold = 9)

subset_df_rel <- subset_mir_threshold(toy_df,
                                      col.mir = miRNA_,
                                      threshold = 0.3)

test_that("Tests subsetting data.frames for a miRNA threshold", {
    expect_equal(nrow(subset_df_abs), 9)
    expect_equal(nrow(subset_df_rel), 15)
})

