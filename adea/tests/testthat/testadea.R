library('adea')
library('ROI.plugin.glpk')
library('testthat')

data('cardealers4')

test_that("Test ADEA: General case (default orientation)", {
    ## skip_on_cran()
    sol <- try(adea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test adea'))
    expect_s3_class(sol, "adea")
    expect_length(sol, 12)
    expect_equal(names(sol), c('name', 'orientation', 'load.orientation', 'inputnames', 'outputnames', 'eff', 'loads', 'ux', 'vy', 'vinput', 'voutput', 'solver'))
    expect_equal(sol$name , 'Test adea')
    expect_equal(sol$orientation , 'input')
    expect_equal(sol$load.orientation , 'inoutput')
    expect_equal(sol$inputnames , c('Employees', 'Depreciation'))
    expect_equal(sol$outputnames , c('CarsSold', 'WorkOrders'))
    expect_equal(sol$eff, c('Dealer A' = 0.991592920353982, 'Dealer B' = 1, 'Dealer C' = 0.892857142857143, 'Dealer D' = 0.865384615384615, 'Dealer E' = 1, 'Dealer F' = 0.651504424778761))
    expect_length(sol$loads, 5)
    expect_equal(sol$loads$load, .66666666)
    expect_equal(sol$loads$input, c('Employees' = .66666666, 'Depreciation' = 1.333333333))
    expect_equal(sol$loads$iinput, c('Employees' = 1))
    expect_equal(sol$loads$output, c('CarsSold' = 1.10250752, 'WorkOrders' = .89749247))
    expect_equal(sol$loads$ioutput, c('WorkOrders' = 2))
    expect_equal(sol$ux, matrix(c(0, .0909090909091, 0, 0, .0909090909091, 0, .125, 0, .083333333333, .0769230769231, 0, .05), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('Employees', 'Depreciation'))))
    expect_equal(sol$vy, matrix(c(.0471238938, 0, 0, .0346153846, .025, .01884956, .01659292035, .02380952381, .02976190476, 0, 0, .006637168), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('CarsSold', 'WorkOrders'))))
    expect_equal(sol$vinput , c('Dealer A' = 8, 'Dealer B' = 11, 'Dealer C' = 12, 'Dealer D' = 13, 'Dealer E' = 11, 'Dealer F' = 20))
    expect_equal(sol$voutput , c('Dealer A' = 7.93274336, 'Dealer B' = 11, 'Dealer C' = 10.7142857, 'Dealer D' = 11.25, 'Dealer E' = 11, 'Dealer F' = 13.0300884))
    expect_equal(sol$solver, 'glpk')
})

test_that("Test Summary", {
    skip_on_cran()
    sol <- try(adea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test adea'))
    ssol <- summary(sol)
    expect_s3_class(ssol, 'summary.adea')
    expect_equal(unclass(ssol),
                 list("Model name" = "Test adea",
                      "Orientation" = "input",
                      "Load orientation" = "inoutput",
                      "Model load" = .66666667,
                      "Input load" = c("Employees" = .66666666667, "Depreciation" = 1.333333333),
                      "Output load" = c("CarsSold" = 1.102507527, "WorkOrders" = .897492473),
                      "Inputs" = "Employees Depreciation",
                      "Outputs" = "CarsSold WorkOrders",
                      "nInputs" = 2,
                      "nOutputs" = 2,
                      "nVariables" = 4,
                      "nEfficients" = 2,
                      "Eff. Mean" = 0.90022318389575,
                      "Eff. sd" = 0.135194867030839,
                      "Eff. Min." = 0.651504424778761,
                      "Eff. 1st Qu."= 0.872252747252747,
                      "Eff. Median" = 0.942225031605562,
                      "Eff. 3rd Qu." = 0.99789823,
                      "Eff. Max." = 1))
})


test_that("Test ADEA: General case (output orientation)", {
    skip_on_cran()
    sol <- try(adea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], orientation = 'output'))
    expect_s3_class(sol, "adea")
    expect_equal(sol$eff, c('Dealer A' = 1.0084783578759480, 'Dealer B' = 1, 'Dealer C' = 1.1199999999999999, 'Dealer D' = 1.1555555555555559, 'Dealer E' = 0.9999999999999999, 'Dealer F' = 1.5349089921217061))
})


test_that("Test ADEA: Single input case", {
    skip_on_cran()
    sol <- try(adea(input = cardealers4[, 'Employees'], output = cardealers4[, c('CarsSold', 'WorkOrders')]))
    expect_s3_class(sol, "adea")
    expect_equal(sol$eff, c('Dealer A' = 0.70575221238938, 'Dealer B' = 1, 'Dealer C' = 0.561224489795918, 'Dealer D' = 0.572916666666667, 'Dealer E' = 1, 'Dealer F' = 0.502949852507375))
})


test_that("Test ADEA: Single output case", {
    skip_on_cran()
    sol <- try(adea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, "adea")
    expect_equal(sol$eff, c('Dealer A' = 0.7875, 'Dealer B' = .75, 'Dealer C' = 0.30, 'Dealer D' = 0.86538462, 'Dealer E' = 1, 'Dealer F' = 0.54))
})


test_that("Test ADEA: Single input and output case with names", {
    skip_on_cran()
    sol <- try(adea(input = cardealers4[, 'Employees', drop = FALSE], output = cardealers4[, 'CarsSold', drop = FALSE]))
    expect_s3_class(sol, "adea")
    expect_equal(sol$eff, c('Dealer A' = 0.48125000000, 'Dealer B' = .625, 'Dealer C' = 0.1571428572, 'Dealer D' = 0.5729166666, 'Dealer E' = 1, 'Dealer F' = 0.36666666))
})


test_that("Test ADEA: Single input and output case without names", {
    skip_on_cran()
    sol <- try(adea(input = cardealers4[, 'Employees'], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, "adea")
    expect_equal(sol$eff, c(0.48125000000, .625, 0.1571428572, 0.5729166666, 1, 0.36666666))
})
