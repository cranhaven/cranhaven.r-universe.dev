library(testthat)

test_that("getCDs works correctly", {
    x <- getCDsFor1Molecule("O=C(O)CCCCCCCCCO", cache = TRUE, verbose = 0)
    y <- getCDsFor1Molecule("O=C(O)CCCCCCCCCO", cache = FALSE, verbose = 0)
    rownames(y) <- "O=C(O)CCCCCCCCCO"
    testthat::expect_equal(x, y)
    testthat::expect_equal(colnames(x), CDFeatures)
    testthat::expect_equal(which(is.na(x)), 29) # only descriptor 29 (`geomShape`) is NA
})
