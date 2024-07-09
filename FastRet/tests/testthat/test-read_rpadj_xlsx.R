library(testthat)

test_that("read_rpadj_xlsx works", {
    x <- read_rpadj_xlsx()
    expect_equal(dim(x), c(25, 3))
})
