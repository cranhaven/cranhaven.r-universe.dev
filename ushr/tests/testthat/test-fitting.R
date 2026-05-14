context("tests on input")

test_that("test input arguments",{

    set.seed(1234567)
    testdata <- simulate_data(nsubjects = 20)

    expect_warning(ushr(data = testdata, initial_buffer = 3.5), fixed = TRUE,
                   "initial_buffer must be a whole number: rounding down to 3")

})
