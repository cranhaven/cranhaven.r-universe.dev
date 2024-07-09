library(testthat)

test_that("preprocess_data works correctly", {
    retlist <- preprocess_data(RP, verbose = 0)
    expect_true(class(retlist) == "data.frame")
    expect_true(all(c("RT", "NAME", "SMILES") %in% colnames(retlist)))
    expect_true(all(colnames(retlist) %in% c("RT", "NAME", "SMILES", CDFeatures)))
})
