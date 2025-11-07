library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("miRNA_" = sample(c("miR-1", "miR-2", "miR-3", "miR-4",
                                         "miR-5", "miR-6", "miR-7"),
                                       size = 70,
                                       replace = TRUE),
                     "PMID_" = seq(1:70),
                     "Year_" = sample(seq(from = 2010, to = 2019),
                                      size = 70,
                                      replace = TRUE),
                     stringsAsFactors = FALSE)

plot_dev <- plot_mir_development(toy_df,
                                 mir = "miR-3",
                                 col.mir = miRNA_,
                                 col.year = Year_)

plot_dev_subset <- plot_mir_development(toy_df,
                                 mir = "miR-3", start = 2013, end = 2015,
                                 col.mir = miRNA_,
                                 col.year = Year_,
                                 title = "Test_title")

test_that("Tests that miRNA development can be plotted", {
    expect_s3_class(plot_dev, "ggplot")
    expect_lte(length(plot_dev_subset$data$Year_),
               length(plot_dev$data$Year_))
    expect_equal(plot_dev$labels$title,
                 "Development of miR-3 in PubMed abstracts")
    expect_equal(plot_dev_subset$labels$title, "Test_title")
})


plot_mir_n <- plot_mir_new(toy_df,
                           col.mir = miRNA_,
                           col.year = Year_)

plot_mir_n_thresh <- plot_mir_new(toy_df,
                                  threshold = 3,
                                  col.mir = miRNA_,
                                  col.year = Year_,
                                  title = "Test_title")

plot_mir_n_startend <- plot_mir_new(toy_df,
                                    start = 2013,
                                    end = 2015,
                                    col.mir = miRNA_,
                                    col.year = Year_)

test_that("Tests that new miRNAs/year are plotted", {
    expect_s3_class(plot_mir_n, "ggplot")
    expect_lte(plot_mir_n_thresh$data$new_miRNAs[1],
               plot_mir_n$data$new_miRNAs[1])
    expect_equal(plot_mir_n$labels$title,
                 "Number of new miRNAs per year")
    expect_equal(plot_mir_n_thresh$labels$title, "Test_title")
    expect_lte(length(plot_mir_n_startend$data$first_mentioned),
               length(plot_mir_n_thresh$data$first_mentioned))
})
