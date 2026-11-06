library('adea')
library('testthat')

data('cardealers4')

test_that('Test CADEA: General case (default orientation) and single bound', {
    ## skip_on_cran()
    sol <- try(cadea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test cadea', load.min = .7, load.max = 2))
    expect_s3_class(sol, 'cadea')
    expect_length(sol, 13)
    expect_equal(names(sol), c('name', 'orientation', 'load.orientation', 'inputnames', 'outputnames', 'eff', 'loads', 'ux', 'vy', 'iterations', 'vinput', 'voutput','solver'))
    expect_equal(sol$name, 'Test cadea')
    expect_equal(sol$orientation, 'input')
    expect_equal(sol$load.orientation, 'inoutput')
    expect_equal(sol$inputnames , c('Employees', 'Depreciation'))
    expect_equal(sol$outputnames , c('CarsSold', 'WorkOrders'))
    expect_equal(sol$eff, c('Dealer A' = .991592920353982, 'Dealer B' = 1, 'Dealer C' = .892857142857143, 'Dealer D' = .865384615384615, 'Dealer E' = 1, 'Dealer F' = .63664896))
    expect_length(sol$loads, 7)
    expect_equal(sol$loads$load, .7)
    expect_equal(sol$loads$input, c('Employees' = .7, 'Depreciation' = 1.3))
    expect_equal(sol$loads$iinput, c('Employees' = 1))
    expect_equal(sol$loads$output, c('CarsSold' = 1.26268132, 'WorkOrders' = .73731867))
    expect_equal(sol$loads$ioutput, c('WorkOrders' = 2))
    expect_equal(sol$loads$load.min, c(.7, .7, .7, .7))
    expect_equal(sol$loads$load.max, c(2, 2, 2, 2))
    expect_equal(sol$ux, matrix(c(0, .090909090909, 0, 0, .09090909, 0.00555555, .125, 0, .08333333333, .07692307692, 0, .045), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('Employees', 'Depreciation'))))
    expect_equal(sol$vy, matrix(c(.04712389381, .01769911504, 0, .03461538462, .025, .01804621436, .016592920354, .013274336283, .029761904762, 0, 0, .006784660767), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('CarsSold', 'WorkOrders'))))
    expect_equal(sol$iterations, 3)
    expect_equal(sol$vinput , c('Dealer A' = 8, 'Dealer B' = 11, 'Dealer C' = 12, 'Dealer D' = 13, 'Dealer E' = 11, 'Dealer F' = 19.78021978))
    expect_equal(sol$voutput , c('Dealer A' = 7.93274336, 'Dealer B' = 11, 'Dealer C' = 10.7142857, 'Dealer D' = 11.25, 'Dealer E' = 11, 'Dealer F' = 12.5930565))
    expect_equal(sol$solver, 'lpsolve')
})

test_that('Test CADEA: General case (default orientation) and multiple bounds', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test cadea', load.min = c(.75, .9, .9, .9), load.max = c(1.1, 1.3, 1.2, 1.2)))
    expect_s3_class(sol, 'cadea')
    expect_equal(sol$eff, c('Dealer A' = .991592920353982, 'Dealer B' = 1, 'Dealer C' = .892857142857143, 'Dealer D' = .865384615384615, 'Dealer E' = 1, 'Dealer F' = .61436578))
    expect_equal(sol$loads$input, c('Employees' = .75, 'Depreciation' = 1.25))
    expect_equal(sol$loads$output, c('CarsSold' = 1.1, 'WorkOrders' = .9))
})

test_that('Test CADEA: General case (default orientation) and multiple bounds with wrong length', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test cadea', load.min = c(.7, .9, .9), load.max = c(1.1, 1.3, 1.2, .9)), silent = TRUE)
    expect_s3_class(sol, 'try-error')
})

test_that('Test CADEA: Summary', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test cadea', load.min = .7, load.max = 2))
    ssol <- summary(sol)
    expect_equal(unclass(ssol),
                 list('Model name' = 'Test cadea',
                      'Orientation' = 'input',
                      'Load orientation' = 'inoutput',
                      'Model load' = .7,
                      'Input load' = c('Employees' = .7, 'Depreciation' = 1.3),
                      'Output load' = c('CarsSold' = 1.26268132, 'WorkOrders' = .73731867),
                      'Minimum for loads' = c(.7, .7, .7, .7),
                      'Maximum for loads' = c(2, 2, 2, 2),
                      'Inputs' = 'Employees Depreciation',
                      'Outputs' = 'CarsSold WorkOrders',
                      'nInputs' = 2,
                      'nOutputs' = 2,
                      'nVariables' = 4,
                      'nEfficients' = 2,
                      'Eff. Mean' = .89774727,
                      'Eff. sd' = .14068534,
                      'Eff. Min.' = .63664896,
                      'Eff. 1st Qu.'= .872252747,
                      'Eff. Median' = .94222503,
                      'Eff. 3rd Qu.' = .99789823,
                      'Eff. Max.' = 1))
})


test_that('Test CADEA: General case (output orientation)', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], orientation = 'output', load.min = .7, load.max = 2))
    expect_s3_class(sol, 'cadea')
    expect_equal(sol$eff, c('Dealer A' = 1.0084783578759480, 'Dealer B' = 1, 'Dealer C' = 1.1199999999999999, 'Dealer D' = 1.1555555555555559, 'Dealer E' = .9999999999999999, 'Dealer F' = 1.63066457))
})


test_that('Test CADEA: Single input case', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, 'Employees'], output = cardealers4[, c('CarsSold', 'WorkOrders')], load.min = .7, load.max = 2))
    expect_s3_class(sol, 'cadea')
    expect_equal(sol$eff, c('Dealer A' = .70575221238938, 'Dealer B' = 1, 'Dealer C' = .561224489795918, 'Dealer D' = .572916666666667, 'Dealer E' = 1, 'Dealer F' = .502949852507375))
})


test_that('Test CADEA: Single output case', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, 'CarsSold'], load.min = .7, load.max = 2))
    expect_s3_class(sol, 'cadea')
    expect_equal(sol$eff, c('Dealer A' = .7875, 'Dealer B' = .625, 'Dealer C' = .28571428, 'Dealer D' = .86538462, 'Dealer E' = 1, 'Dealer F' = .54))
})


test_that('Test CADEA: Single input and output case with names', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, 'Employees', drop = FALSE], output = cardealers4[, 'CarsSold', drop = FALSE], load.min = .7, load.max = 2))
    expect_s3_class(sol, 'cadea')
    expect_equal(sol$eff, c('Dealer A' = .48125000000, 'Dealer B' = .625, 'Dealer C' = .1571428572, 'Dealer D' = .5729166666, 'Dealer E' = 1, 'Dealer F' = .36666666))
})


test_that('Test CADEA: Single input and output case without names', {
    skip_on_cran()
    sol <- try(cadea(input = cardealers4[, 'Employees'], output = cardealers4[, 'CarsSold'], load.min = .7, load.max = 2))
    expect_s3_class(sol, 'cadea')
    expect_equal(sol$eff, c(.48125000000, .625, .1571428572, .5729166666, 1, .36666666))
})
