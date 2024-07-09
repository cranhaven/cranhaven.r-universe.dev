library(testthat)

test_that("getCDs works correctly", {
    df <- RP[1:5, ] # Only check first 5 molecules for speedup
    y2 <- getCDs(df, verbose = 0)
    nc <- ncol(y2)
    expect_true(all.equal(y2[, 1:3], df))
    expect_true(all(colnames(y2)[4:nc] %in% CDFeatures))
})
