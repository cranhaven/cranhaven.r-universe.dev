context("Greedy Solve test")
library(maxnodf)

test_that("greedy solve",{
    mtx1 <- greedy_solve2(14, 13, 52)
    mtx2 <- greedy_solve2(131, 666, 2933)
    expect_equal(nodf_cpp(mtx1), 0.875739644970414)
    expect_equal(nodf_cpp(mtx2), 0.871077578709341)
})

test_that("greedy hill climb",{
    mtx1 <- greedy_solve2(14, 13, 52)
    mtx2 <- full_hill_climb(mtx1)
    mtx3 <- full_hill_climb(mtx1, R=2)
    expect_equal(nodf_cpp(mtx2), nodf_cpp(mtx1))
    expect_true(nodf_cpp(mtx3) > 0.877)
})
