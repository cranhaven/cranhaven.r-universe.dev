library('adea')
library('ROI.plugin.glpk')
library('testthat')

data('cardealers4')

test_that('Test DEA: Error detections', {
    skip_on_cran()
    ## NAs in input
    input = cardealers4[, c('Employees', 'Depreciation')]
    output = cardealers4[, c('CarsSold', 'WorkOrders')]
    input[1, 1] <- NA
    sol <- try(dea(input, output), silent = TRUE)
    expect_s3_class(sol, 'try-error')
    ## NAs in output
    input = cardealers4[, c('Employees', 'Depreciation')]
    output = cardealers4[, c('CarsSold', 'WorkOrders')]
    output[1, 1] <- NA
    sol <- try(dea(input, output), silent = TRUE)
    expect_s3_class(sol, 'try-error')
})

test_that('Test DEA: General case (default orientation)', {
    ## skip_on_cran()
    sol <- try(dea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test dea'))
    expect_s3_class(sol, 'dea')
    expect_length(sol, 10)
    expect_equal(names(sol), c('name', 'orientation', 'inputnames', 'outputnames', 'eff', 'ux', 'vy', 'vinput', 'voutput', 'solver'))
    expect_equal(sol$name , 'Test dea')
    expect_equal(sol$orientation , 'input')
    expect_equal(sol$inputnames , c('Employees', 'Depreciation'))
    expect_equal(sol$outputnames , c('CarsSold', 'WorkOrders'))
    expect_equal(sol$eff, c('Dealer A' = 0.991592920353982, 'Dealer B' = 1, 'Dealer C' = 0.892857142857143, 'Dealer D' = 0.865384615384615, 'Dealer E' = 1, 'Dealer F' = 0.651504424778761))
    expect_equal(sol$ux, matrix(c(0, 0, 0, 0, 0, 0, .125, .06666666667, .08333333333, .07692307692, .05555555556, .05), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('Employees', 'Depreciation'))))
    expect_equal(sol$vy, matrix(c(.04712389381, 0, 0, .03461538462, .025, .01884955752, .016592920354, .023809523810, .029761904762, 0, 0, .006637168142), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('CarsSold', 'WorkOrders'))))
    expect_equal(sol$vinput , c('Dealer A' = 8, 'Dealer B' = 15, 'Dealer C' = 12, 'Dealer D' = 13, 'Dealer E' = 18, 'Dealer F' = 20))
    expect_equal(sol$voutput , c('Dealer A' = 7.93274336, 'Dealer B' = 15, 'Dealer C' = 10.7142857, 'Dealer D' = 11.25, 'Dealer E' = 18, 'Dealer F' = 13.030088))
    expect_equal(sol$solver, 'glpk')
})

test_that('Test DEA: Summary', {
    skip_on_cran()
    sol <- try(dea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test dea'))
    ssol <- summary(sol)
    expect_s3_class(ssol, 'summary.dea')
    expect_equal(unclass(ssol),
                 list('Model name' = 'Test dea',
                      'Orientation' = 'input',
                      'Inputs' = 'Employees Depreciation',
                      'Outputs' = 'CarsSold WorkOrders',
                      'nInputs' = 2,
                      'nOutputs' = 2,
                      'nVariables' = 4,
                      'nEfficients' = 2,
                      'Eff. Mean' = 0.90022318389575,
                      'Eff. sd' = 0.135194867030839,
                      'Eff. Min.' = 0.651504424778761,
                      'Eff. 1st Qu.'= 0.872252747252747,
                      'Eff. Median' = 0.942225031605562,
                      'Eff. 3rd Qu.' = 0.99789823,
                      'Eff. Max.' = 1))
})


test_that('Test DEA: General case (output orientation)', {
    skip_on_cran()
    sol <- try(dea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], orientation = 'output'))
    expect_s3_class(sol, 'dea')
    expect_equal(sol$eff, c('Dealer A' = 1.0084783578759480, 'Dealer B' = 1, 'Dealer C' = 1.1199999999999999, 'Dealer D' = 1.1555555555555559, 'Dealer E' = 0.9999999999999999, 'Dealer F' = 1.5349089921217061))
})


test_that('Test DEA: Single input case', {
    skip_on_cran()
    sol <- try(dea(input = cardealers4[, 'Employees'], output = cardealers4[, c('CarsSold', 'WorkOrders')]))
    expect_s3_class(sol, 'dea')
    expect_equal(sol$eff, c('Dealer A' = 0.70575221238938, 'Dealer B' = 1, 'Dealer C' = 0.561224489795918, 'Dealer D' = 0.572916666666667, 'Dealer E' = 1, 'Dealer F' = 0.502949852507375))
})


test_that('Test DEA: Single output case', {
    skip_on_cran()
    sol <- try(dea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, 'dea')
    expect_equal(sol$eff, c('Dealer A' = 0.7875, 'Dealer B' = .75, 'Dealer C' = 0.30, 'Dealer D' = 0.86538462, 'Dealer E' = 1, 'Dealer F' = 0.54))
})


test_that('Test DEA: Single input and output case with names', {
    skip_on_cran()
    sol <- try(dea(input = cardealers4[, 'Employees', drop = FALSE], output = cardealers4[, 'CarsSold', drop = FALSE]))
    expect_s3_class(sol, 'dea')
    expect_equal(sol$eff, c('Dealer A' = 0.48125000000, 'Dealer B' = .625, 'Dealer C' = 0.1571428572, 'Dealer D' = 0.5729166666, 'Dealer E' = 1, 'Dealer F' = 0.36666666))
})


test_that('Test DEA: Single input and output case without names', {
    skip_on_cran()
    sol <- try(dea(input = cardealers4[, 'Employees'], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, 'dea')
    expect_equal(sol$eff, c(0.48125000000, .625, 0.1571428572, 0.5729166666, 1, 0.36666666))
})
