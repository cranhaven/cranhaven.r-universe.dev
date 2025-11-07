library(miRetrieve)
library(testthat)

set.seed(42)

toy_df <- data.frame("Subset" = sample(c("Yes", "No"),
                                       size = 10,
                                       replace = TRUE))

subset_df <- subset_df(toy_df,
                       "Subset")

test_that("Tests subsetting data.frames for a column and a condition", {
    expect_equal(nrow(subset_df), 5)
})
