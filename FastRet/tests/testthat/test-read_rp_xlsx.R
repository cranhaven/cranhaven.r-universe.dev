library(testthat)

test_that("read_rp_xlsx works", {
    x <- read_rp_xlsx()
    expect_equal(x, RP)
})
