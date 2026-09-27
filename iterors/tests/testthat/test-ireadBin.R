library(foreach)

test_that("test01", {

    n <- 1000
    zz <- file("testbin", "wb")
    expected <- foreach(1:1000) %do% {
        x <- rnorm(n)
        writeBin(x, zz)
        mean(x)
    }
    close(zz)
    it <- ireadBin("testbin", "double", n = n)
    actual <- foreach(x = it) %do% {
        mean(x)
    }
    expect_equal(expected, actual)
    it <- i_repeat(list(where = 0, origin = "current"), n)
    it <- ireadBin("testbin", double(), n = n, ipos = it)
    actual <- foreach(x = it) %do% {
        mean(x)
    }
    expect_equal(expected, actual)

})

test_that("test02", {
    n <- 1000
    zz <- file("testbin", "wb")
    expected <- foreach(1:1000) %do% {
        x <- rnorm(n)
        writeBin(x, zz)
        mean(x)
    }
    close(zz)
    it <- i_repeat(0, n)
    it <- ireadBin("testbin", "double", n = n, ipos = it)
    actual <- foreach(x = it) %do% {
        mean(x)
    }
    expected <- rep(expected[1], n)
    expect_equal(expected, actual)
    it <- i_repeat(list(where = 0, origin = "start"), n)
    it <- ireadBin("testbin", double(), n = n, ipos = it)
    actual <- foreach(x = it) %do% {
        mean(x)
    }
    expect_equal(expected, actual)
})

test_that("test03", {
    x <- rnorm(100)
    expected <- matrix(x, 10)
    zz <- file("testbin", "wb")
    writeBin(x, zz)
    close(zz)
    it <- ireadBin("testbin", "double", nrow(expected))
    actual <- foreach(col = it, .combine = "cbind") %do% col
    colnames(actual) <- NULL
    dimnames(actual) <- NULL
    expect_equal(expected, actual)
    it <- ireadBin("testbin", double(), nrow(expected))
    actual <- do.call("cbind", as.list(it))
    expect_equal(expected, actual)
})

test_that("test99", {
    expect_silent(unlink("testbin"))
})
