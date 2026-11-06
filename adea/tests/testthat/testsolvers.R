library('adea')
library('ROI.plugin.glpk')
library('ROI.plugin.lpsolve')
library('ROI.plugin.symphony')
library('testthat')

data('cardealers4')
input = cardealers4[, c('Employees', 'Depreciation')]
output = cardealers4[, c('CarsSold', 'WorkOrders')]

test_that("Test solvers: solver = 'auto'", {
    sol <- try(dea(input = input, output = output))
    expect_s3_class(sol, "dea")
    expect_equal(sol$eff, c('Dealer A' = 0.991592920353982, 'Dealer B' = 1, 'Dealer C' = 0.892857142857143, 'Dealer D' = 0.865384615384615, 'Dealer E' = 1, 'Dealer F' = 0.651504424778761))
})


test_that("Test solvers: solver = 'nonsolver'", {
    skip_on_cran()
    sol <- try(dea(input = input, output = output, solver = 'nonsolver'), silent = TRUE)
    expect_s3_class(sol, "try-error")
})


test_that("Test solvers: solver = 'glpk'", {
    skip_on_cran()
    sol <- try(dea(input = input, output = output, solver = 'glpk'))
    expect_s3_class(sol, "dea")
    expect_equal(sol$eff, c('Dealer A' = 0.991592920353982, 'Dealer B' = 1, 'Dealer C' = 0.892857142857143, 'Dealer D' = 0.865384615384615, 'Dealer E' = 1, 'Dealer F' = 0.651504424778761))
})


test_that("Test solvers: solver = 'lpsolve'", {
    skip_on_cran()
    sol <- try(dea(input = input, output = output, solver = 'lpsolve'))
    expect_s3_class(sol, "dea")
    expect_equal(sol$eff, c('Dealer A' = 0.991592920353982, 'Dealer B' = 1, 'Dealer C' = 0.892857142857143, 'Dealer D' = 0.865384615384615, 'Dealer E' = 1, 'Dealer F' = 0.651504424778761))
})


test_that("Test solvers: solver = 'symphony'", {
    skip_on_cran()
    sol <- try(dea(input = input, output = output, solver = 'symphony'))
    expect_s3_class(sol, "dea")
    expect_equal(sol$eff, c('Dealer A' = 0.991592920353982, 'Dealer B' = 1, 'Dealer C' = 0.892857142857143, 'Dealer D' = 0.865384615384615, 'Dealer E' = 1, 'Dealer F' = 0.651504424778761))
})

