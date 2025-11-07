library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("miRNA_" = sample(c("miR-30", "miR-29", "miR-28",
                                         "miR-27", "miR-26"),
                                       size = 30,
                                       replace = TRUE),
                     "PMID_" = sample(seq(1:5),
                                      size = 30,
                                      replace = TRUE),
                     stringsAsFactors = FALSE)

plot_mir <- plot_mir_count(toy_df,
                           col.mir = miRNA_,
                           title = "Test_title")

plot_mir_top <- plot_mir_count(toy_df,
                               top = 3,
                               col.mir = miRNA_)

test_that("Tests that count plot is created", {
    expect_s3_class(plot_mir, "ggplot")
    expect_equal(plot_mir$labels$title, "Test_title")
    expect_lte(length(plot_mir_top$data$miRNA_),
               length(plot_mir$data$miRNA_))
    expect_equal(plot_mir_top$labels$title,
                 "Most frequently mentioned miRNAs")
})

plot_thresh_mir <- plot_mir_count_threshold(toy_df,
                                            start = 1,
                                            end = 3,
                                            col.mir = miRNA_,
                                            col.pmid = PMID_,
                                            title = "Test_title")

plot_thresh_mir_less <- plot_mir_count_threshold(toy_df,
                                                 start = 2,
                                                 end = 3,
                                                 col.mir = miRNA_,
                                                 col.pmid = PMID_)

test_that("Tests that miRNAs are counted according to threshold", {
    expect_s3_class(plot_thresh_mir, "ggplot")
    expect_equal(plot_thresh_mir$labels$title, "Test_title")
    expect_lte(length(plot_thresh_mir_less$data$number_mirnas),
               length(plot_thresh_mir$data$number_mirnas))
    expect_equal(plot_thresh_mir_less$labels$title,
                 "Number of miRNAs/count threshold")
})
