library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("miRNA_" = c(sample(c("miR-1", "miR-2", "miR-3", "miR-4", "miR-5"),
                                         size = 20,
                                         replace = TRUE),
                                  sample(c("miR-3", "miR-4", "miR-5", "miR-6", "miR-7"),
                                         size = 20,
                                         replace = TRUE)),
                     "Topic_" = rep(c("Topic1", "Topic2"), each = 20),
                     "PMID_" = seq(1:40),
                     stringsAsFactors = FALSE)

gt_mir <- get_mir(toy_df,
                  col.mir = miRNA_,
                  col.pmid = PMID_)

test_that("Tests that miRNAs are captured from a dataframe", {
    expect_type(gt_mir, "character")
    expect_length(gt_mir, 5)
})

gt_mir_top <- get_mir(toy_df,
                      top = 2,
                      col.mir = miRNA_,
                      col.pmid = PMID_)

test_that("Tests that top miRNAs are captured from a dataframe", {
    expect_type(gt_mir_top, "character")
    expect_lte(length(gt_mir_top), length(gt_mir))
})

gt_mir_thresh <- get_mir(toy_df,
                         threshold = 2,
                         col.mir = miRNA_,
                         col.pmid = PMID_)

test_that("Tests that miRNAs are captured from a dataframe by absolute threshold", {
    expect_type(gt_mir_thresh, "character")
    expect_gte(length(gt_mir_thresh), length(gt_mir_top))
})

gt_mir_thresh_rel <- get_mir(toy_df,
                             threshold = 0.2,
                             col.mir = miRNA_,
                             col.pmid = PMID_)

test_that("Tests that miRNAs are captured from a dataframe by relative threshold", {
    expect_type(gt_mir_thresh_rel, "character")
    expect_gte(length(gt_mir_thresh), length(gt_mir_thresh_rel))
})

gt_mir_topic <- get_mir(toy_df,
                        topic = "Topic1",
                        col.mir = miRNA_,
                        col.pmid = PMID_,
                        col.topic = Topic_)

test_that("Tests that miRNAs are captured from a dataframe by topic", {
    expect_type(gt_mir_topic, "character")
    expect_equal(length(gt_mir_topic), 5)
})
