testthat::test_that("Test that weights and adjacency generics work as expected", {

    temp <- list(weights=1, adj=1)
    class(temp) <- "rwclust"

    expect_equal(weights(temp), 1)
    expect_equal(adjacency(temp), 1)

})