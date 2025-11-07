library(miRetrieve)
library(testthat)

string_test <- "miR-146a and miR-24 are interesting miRNAs"
no_letters <- c("miR-146", "miR-24")
letters <- c("miR-146a", "miR-24")

test_that("Tests that miRNAs are extracted from strings", {
    expect_equal(extract_mir_string(string_test), no_letters)
    expect_equal(extract_mir_string(string_test,
                                    extract_letters = TRUE), letters)
})
