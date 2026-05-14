context("tests on input")

test_that("test for numeric input",{

    expect_that(simulate_data(nsubjects = "twenty"),
                throws_error("The following arguments must be numeric: nsubjects, detection_threshold, censortime, max_datapoints, min_datapoints, sd_noise"))
})

test_that("test for correct parameter input",{

    expect_that(simulate_data(mean_params = c(A = 100000, delta = 0.33, B = 10000)),
                throws_error("The 'mean_params' argument must be a named vector with values for A, delta, B, and gamma."))

    expect_that(simulate_data(mean_params = c(A = "one thousand", delta = 0.33, B = 10000, gamma = 0.03)),
                throws_error("The 'mean_params' argument must have numeric values for A, delta, B, and gamma."))

})

context("tests on output")

test_that("test output is a data frame",{
    set.seed(1234567)

    expect_that(simulate_data(nsubjects = 20),is_a("data.frame"))

    # jumble parameter input
    expect_that(simulate_data(mean_params = c(A = 100000, B = 10000, delta = 0.33, gamma = 0.03)),is_a("data.frame"))
})

test_that("test output has the same number of subjects as input",{
    set.seed(1234567)

    input_subjects <- 20
    output_subjects <- simulate_data(nsubjects = input_subjects) %>% distinct(id)

    expect_that(nrow(output_subjects), equals(input_subjects))
})
