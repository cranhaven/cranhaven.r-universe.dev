library('adea')
library('testthat')

data('cardealers4')

test_that('Test fsdea: Error detections', {
    skip_on_cran()
    ## ninput incorrect value
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea', ninputs = 0), silent = TRUE)
    expect_s3_class(sol, 'try-error')
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea', ninputs = 3), silent = TRUE)
    expect_s3_class(sol, 'try-error')
    ## noutput incorrect value
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea', noutputs = 0), silent = TRUE)
    expect_s3_class(sol, 'try-error')
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea', noutputs = 3), silent = TRUE)
    expect_s3_class(sol, 'try-error')
    ## nvariables incorrect value
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea', nvariables = 0), silent = TRUE)
    expect_s3_class(sol, 'try-error')
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea', nvariables = 5), silent = TRUE)
    expect_s3_class(sol, 'try-error')
})

test_that('Test fsdea: General case', {
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea'))
    expect_s3_class(sol, 'fsdea')
    expect_length(sol, 19)
    expect_equal(names(sol), c('name', 'orientation', 'ninputs', 'noutputs', 'nvariables', 'inputnames', 'outputnames', 'eff', 'ux', 'vy', 'obj', 'iselected', 'oselected', 'niselected', 'noselected', 'nvselected', 'vinput', 'voutput', 'solver'))
    expect_equal(sol$name , 'Test fsdea')
    expect_equal(sol$orientation, 'input')
    expect_equal(sol$ninputs, 2)
    expect_equal(sol$noutputs, 2)
    expect_equal(sol$nvariables, 4)
    expect_equal(sol$inputnames, c('Employees', 'Depreciation'))
    expect_equal(sol$outputnames, c('CarsSold', 'WorkOrders'))
    expect_equal(sol$eff, c('Dealer A' = .991592920353982, 'Dealer B' = 1, 'Dealer C' = .892857142857143, 'Dealer D' = .865384615384615, 'Dealer E' = 1, 'Dealer F' = .651504424778761))
    expect_equal(sol$ux, matrix(c(0, 0, 0, 0, 0, 0, .125, .06666666666, .083333333, .07692308, .0555555555, .05), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('Employees', 'Depreciation'))))
    expect_equal(sol$vy, matrix(c(.0471238938, 0, 0, .0346153846, .0209439528, .0188495575, .01659292035, .02380952381, .02976190476, 0, .00737463127, .00663716814), ncol = 2, dimnames = list(c('Dealer A', 'Dealer B', 'Dealer C', 'Dealer D', 'Dealer E', 'Dealer F'), c('CarsSold', 'WorkOrders'))))
    expect_equal(sol$obj, 5.4013391)
    expect_equal(sol$iselected, c('Employees' = 0, 'Depreciation' = 1))
    expect_equal(sol$oselected, c('CarsSold' = 1, 'WorkOrders' = 1))
    expect_equal(sol$niselected, 1)
    expect_equal(sol$noselected, 2)
    expect_equal(sol$nvselected, 3)
    expect_equal(sol$vinput , c('Dealer A' = 8, 'Dealer B' = 15, 'Dealer C' = 12, 'Dealer D' = 13, 'Dealer E' = 18, 'Dealer F' = 20))
    expect_equal(sol$voutput , c('Dealer A' = 7.93274336, 'Dealer B' = 15, 'Dealer C' = 10.7142857, 'Dealer D' = 11.25, 'Dealer E' = 18, 'Dealer F' = 13.03008849))
    expect_equal(sol$solver, 'glpk')
})


test_that('Test fsdea: Summary', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], name = 'Test fsdea'))
    ssol <- summary(sol)
    expect_s3_class(ssol, 'summary.fsdea')
    expect_equal(unclass(ssol), list('Model name' = 'Test fsdea',
                                     'Orientation' = 'input',
                                     'nInputs' = 2,
                                     'nOutputs' = 2,
                                     'nVariables' = 4,
                                     'nEfficients' = 2,
                                     'Eff. Mean' = .90022318389575,
                                     'Eff. sd' = .135194867030839,
                                     'Eff. Min.' = .651504424778761,
                                     'Eff. 1st Qu.' = .872252747252747,
                                     'Eff. Median' = .942225031605562,
                                     'Eff. 3rd Qu.' = .997898230088496,
                                     'Eff. Max.' = 1,
                                     'iSelected' = 'Depreciation',
                                     'oSelected' = 'CarsSold, WorkOrders'))
})


test_that('Test fsdea: Single input case', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, 'Employees'], output = cardealers4[, c('CarsSold', 'WorkOrders')]))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$eff, c('Dealer A' = .70575221238938, 'Dealer B' = 1, 'Dealer C' = .561224489795918, 'Dealer D' = .572916666666667, 'Dealer E' = 1, 'Dealer F' = .502949852507375))
})


test_that('Test fsdea: Single output case', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$eff, c('Dealer A' = .7875, 'Dealer B' = .75, 'Dealer C' = .30, 'Dealer D' = .86538462, 'Dealer E' = 1, 'Dealer F' = .54))
})


test_that("Test fsdea: Single input and output case with names", {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, 'Employees', drop = FALSE], output = cardealers4[, 'CarsSold', drop = FALSE]))
    expect_s3_class(sol, "fsdea")
    expect_equal(sol$eff, c('Dealer A' = 0.48125000000, 'Dealer B' = .625, 'Dealer C' = 0.1571428572, 'Dealer D' = 0.5729166666, 'Dealer E' = 1, 'Dealer F' = 0.36666666))
})


test_that("Test fsdea: Single input and output case without names", {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, 'Employees'], output = cardealers4[, 'CarsSold']))
    expect_s3_class(sol, "fsdea")
    expect_equal(sol$eff, c(0.48125000000, .625, 0.1571428572, 0.5729166666, 1, 0.36666666))
})


test_that('Test fsdea: Select 1 input', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], ninputs = 1))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$ninputs, 1)
    expect_equal(sol$noutputs, 2)
    expect_equal(sol$eff, c('Dealer A' = .991592920353982, 'Dealer B' = 1, 'Dealer C' = .892857142857143, 'Dealer D' = .865384615384615, 'Dealer E' = 1, 'Dealer F' = .651504424778761))
    expect_equal(sol$iselected, c(Employees = 0, Depreciation = 1))
})


test_that('Test fsdea: Select 1 output', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], noutputs = 1))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$ninputs, 2)
    expect_equal(sol$noutputs, 1)
    expect_equal(sol$eff, c('Dealer A' = .7874999999999999, 'Dealer B' = .75, 'Dealer C' = .3, 'Dealer D' = .8653846153846154, 'Dealer E' = 1, 'Dealer F' = .54))
    expect_equal(sol$oselected, c(CarsSold = 1, WorkOrders = 0))
})


test_that('Test fsdea: Select 1 input and 1 output', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], ninputs = 1, noutputs = 1))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$ninputs, 1)
    expect_equal(sol$noutputs, 1)
    expect_equal(sol$eff, c('Dealer A' = .7874999999999999, 'Dealer B' = .75, 'Dealer C' = .3, 'Dealer D' = .8653846153846154, 'Dealer E' = 1, 'Dealer F' = .54))
    expect_equal(sol$oselected, c(CarsSold = 1, WorkOrders = 0))
})


test_that('Test fsdea: Select 3 variables', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], nvariables = 3))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$ninputs, 2)
    expect_equal(sol$noutputs, 2)
    expect_equal(sol$eff, c('Dealer A' = .99159292, 'Dealer B' = 1, 'Dealer C' = .89285714, 'Dealer D' = .8653846153846154, 'Dealer E' = 1, 'Dealer F' = .65150442))
    expect_equal(sol$iselected, c(Employees = 0, Depreciation = 1))
    expect_equal(sol$oselected, c(CarsSold = 1, WorkOrders = 1))
})


test_that('Test fsdea: Select 1 input and 3 variables', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], ninputs = 1, nvariables = 3))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$ninputs, 1)
    expect_equal(sol$noutputs, 2)
    expect_equal(sol$eff, c('Dealer A' = .99159292, 'Dealer B' = 1, 'Dealer C' = .89285714, 'Dealer D' = .8653846153846154, 'Dealer E' = 1, 'Dealer F' = .6515044))
    expect_equal(sol$iselected, c(Employees = 0, Depreciation = 1))
    expect_equal(sol$oselected, c(CarsSold = 1, WorkOrders = 1))
})


test_that('Test fsdea: Select 1 output and 3 variables', {
    skip_on_cran()
    sol <- try(fsdea(input = cardealers4[, c('Employees', 'Depreciation')], output = cardealers4[, c('CarsSold', 'WorkOrders')], noutputs = 1, nvariables = 3))
    expect_s3_class(sol, 'fsdea')
    expect_equal(sol$ninputs, 2)
    expect_equal(sol$noutputs, 1)
    expect_equal(sol$eff, c('Dealer A' = .7875, 'Dealer B' = .75, 'Dealer C' = .30, 'Dealer D' = .8653846153846154, 'Dealer E' = 1, 'Dealer F' = .54))
    expect_equal(sol$iselected, c(Employees = 0, Depreciation = 1))
    expect_equal(sol$oselected, c(CarsSold = 1, WorkOrders = 0))
})
