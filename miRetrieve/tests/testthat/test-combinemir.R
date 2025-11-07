library(miRetrieve)
library(testthat)

mir1_ <- c("miR-146", "miR-24", "miR-34")
mir2_ <- c("miR-146", "miR-24", "miR-21")

combined_mir <- combine_mir(mir1_, mir2_)

test_that("Tests that miRNA vectors are combined", {
    expect_equal(typeof(combined_mir), "character")
    expect_lte(length(combined_mir), length(mir1_) + length(mir2_))
})
