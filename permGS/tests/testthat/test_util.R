context("util")

test_that("parseFormula parses correctly", {
    time <- rexp(100, 1)
    status <- rbinom(100, 1, 0.6)
    trt <- rbinom(100, 1, 0.5)
    blocks <- sample(rep(0:3, c(40, 30, 20, 10)))
    
    x <- parseFormula(Surv(time, status) ~ trt + strata(blocks))

    expect_equal(time, x$time)
    expect_equal(status, x$status)
    expect_equal(trt, x$trt)
    expect_equal(length(unique(blocks)), length(unique(x$strata)))
})

test_that("parseFormula parses trt correctly when passing factor", {
    time <- rexp(100, 1)
    status <- rbinom(100, 1, 0.6)
    trt <- factor(sample(rep(c("A", "B"), c(60, 40))))

    x <- parseFormula(Surv(time, status) ~ trt)$trt

    expect_equal(sum(x == 0), 60)
    expect_equal(sum(x == 1), 40)
    expect_true(is.numeric(x))
})
