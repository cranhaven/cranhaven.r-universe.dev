library(miRetrieve)
library(testthat)

set.seed(42)
toy_df <- data.frame("PMID" = seq(1:30),
                     "Type" = sample(c("Journal Article", "Review"),
                                     size = 30,
                                     replace = TRUE))




test_that("Tests that articles are subset for the correct type", {
    expect_equal(nrow(subset_research(toy_df)), 18)
    expect_equal(nrow(subset_review(toy_df)), 12)
})
