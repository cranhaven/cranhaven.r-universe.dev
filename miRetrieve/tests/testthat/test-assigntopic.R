library(miRetrieve)
library(testthat)

# Creates a toy data frame
# "Score_1" and "Score_2" are equal/have ties for the first 8 rows
# "Score_2" superseeds "Score_1" in the last two rows
toy_df <- data.frame("PMID" = seq(1:10),
           "Score_1" = c(seq(1:7), 8, 8, 8),
           "Score_2" = seq(1:10))

toy_result_keep <- assign_topic(toy_df,
                                col.topic = c("Score_1", "Score_2"),
                                threshold = c(5,7),
                                col.topic.name = "Topic_")

toy_result_discard <- assign_topic(toy_df,
                                   col.topic = c("Score_1", "Score_2"),
                                   threshold = c(5, 7),
                                   discard = TRUE)

score_1_sum <- sum(toy_result_keep$Topic_ == "Score_1")

score_2_sum <- sum(toy_result_keep$Topic_ == "Score_2")

test_that("Tests that topics are assigned based on scores", {
    expect_equal(nrow(toy_result_keep), 12)
    expect_equal(ncol(toy_result_keep), 4)
    expect_equal(colnames(toy_result_keep)[4], "Topic_")
    expect_equal(score_1_sum, 4)
    expect_equal(score_2_sum, 4)
    expect_equal(toy_result_keep[["Topic_"]][1], "Unknown")
    expect_equal(nrow(toy_result_discard), 8)
})
