# setup ------------------------------------------------------------------------
set.seed(123L)
mat <- matrix(rnorm(1e3L), ncol = 10L)
ARGS <- ERR <- list(xt = mat[1L:4L, ], xtp1 = mat[1L:4L + 1L, ], 0.1)
ERR$xtp1 <- ERR$xtp1[, -1L]

# fragilityRow -------------------------------------------------------
test_that("fragilityRow", {
    input <- do.call(ridge, ARGS)
    fragilityRow(input) |> expect_no_error()
})

# ridge ------------------------------------------------------------------------
test_that("ridge/ridgeR2", {
    do.call(ridge, ERR) |> expect_error()
    A <- do.call(ridge, ARGS) |> expect_no_error()
    do.call(ridgeR2, c(ARGS[-3L], list(A))) |> expect_no_error()
})

# ridgeSearch -------------------------------------------------
test_that("ridgeSearch", {
    do.call(ridgeSearch, c(ERR[-3L])) |> expect_error()
    do.call(ridgeSearch, c(ARGS[-3L])) |> expect_no_error()
})


# internal data -------------------------------------------------
test_that("Data consistency across versions", {
    set.seed(1)
    data <- matrix(rnorm(800), ncol = 40)
    window <- 10
    step <- 5
    lambda <- 0.1
    frag <- calcAdjFrag(epoch = data, window = window, step = step, lambda = lambda)
    expect_equal(frag, testFrag)
})


test_that("Data consistency across versions for parallel computing", {
    cl <- parallel::makeCluster(2, type = "SOCK")
    doSNOW::registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl))

    set.seed(1)
    data <- matrix(rnorm(800), ncol = 40)
    window <- 10
    step <- 5
    lambda <- 0.1
    frag <- calcAdjFrag(epoch = data, window = window, step = step, lambda = lambda, parallel = TRUE)
    expect_equal(frag, testFrag)
})
