library('adea')
library('ROI.plugin.lpsolve')
library('testthat')

data('cardealers4')

test_that("Test adea_load_leverage: General case (default orientation)", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')]))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(sol$loads, c(1, .8, .8, .8, .8, .73582309))
    expect_equal(sol$loads.diff, c(.333333333, .133333333, .133333333, .133333333, .133333333,  .069156429))
    expect_equal(sol$dmu.indexs, as.matrix(c(2, 6, 4, 1, 3, 5), ncol = 1))
})

test_that("Test adea_load_leverage: General case with ndel = 2 (default orientation)", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], ndel = 2))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(sol$loads, c(1, 1, 1, 1, 1, 1, 1, 1, 1, .96356275, .87432432, .84799402, .8420551, .82432432, .80, .80, .80, .80, .74617707, .73582309))
    expect_equal(sol$loads.diff, c(.33333333, .33333333, .33333333, .33333333, .33333333, .33333333, .33333333, .33333333, .33333333, .296896086, .207657657, .181327356, .175388434, .15765765, .1333333333, .13333333, .13333333, .13333333, .07951040, .06915642))
    expect_equal(sol$dmu.indexs, matrix(c(1, 3, 2, 2, 4, 2, 1, 2, 1, 2, 5, 1, 3, 1, 6, 4, 1, 3, 3, 5, 6, 4, 3, 5, 6, NA, 4, 6, 2, 4, 6, 3, 6, 5, NA, NA, NA, NA, 5, NA), ncol = 2))
})

test_that("Test adea_load_leverage: General case with ndel = 2 and nmax = 5 (default orientation)", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], ndel = 2, nmax = 5))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(sol$loads, c(1, 1, 1, 1, 1))
    expect_equal(sol$loads.diff, c(.33333333, .33333333, .33333333, .33333333, .33333333))
    expect_equal(sol$dmu.indexs, matrix(c(1, 3, 2, 2, 4, 6, 4, 3, 5, 6), ncol = 2))
})

test_that("Test adea_load_leverage: General case (output orientation)", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], orientation = 'output'))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(sol$loads, c(1, .7569974, .70629108, .70188455, .68841311, .67374056))
    expect_equal(sol$loads.diff, c(.41339881, .170396225, .119689899, .115283367, .101811928, .087139379))
    expect_equal(sol$dmu.indexs, as.matrix(c(2, 6, 4, 3, 1, 5), ncol = 1))
})

test_that("Test adea_load_leverage: Single input case", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, 'Employees'], output = cardealers4[, c('CarsSold', 'WorkOrders')]))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(sol$loads, c(.81429179, .89923525))
    expect_equal(sol$loads.diff, c(.1857082, .100764747))
    expect_equal(sol$dmu.indexs, as.matrix(c(5, 2), ncol = 1))
})

test_that("Test adea_load_leverage: Single output case", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(sol$loads, c(.91660231, .40, .40, .40, .40, .40))
    expect_equal(sol$loads.diff, c(.58326898, .066666666, .066666666, .066666666, .066666666, .066666666))
    expect_equal(sol$dmu.indexs, as.matrix(c(5, 3, 6, 1, 4, 2), ncol = 1))
})


test_that("Test adea_load_leverage: Single input and output case", {
    skip_on_cran()
    sol <- try(adea_load_leverage(input = cardealers4[, 'Employees'], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, "adealoadleverage")
    expect_length(sol, 3)
    expect_equal(length(sol$loads), 0)
    expect_equal(length(sol$loads.diff), 0)
    expect_equal(sol$dmu.indexs, numeric(0))
})
