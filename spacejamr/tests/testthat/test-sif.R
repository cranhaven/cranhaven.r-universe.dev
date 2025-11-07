library(spacejamr)

test_that("we can use different spatial interaction functions", {

    # Standard power las
    expect_equal(class(standard(5, 0.9, 1, -2.8)), "numeric")

    # Attenuated power law
    expect_equal(class(attenuated(5, 0.9, 1, -2.8)), "numeric")

    # Arctangent probability law
    expect_equal(class(arctan(5, 0.9, 1, -2.8)), "numeric")

    # Exponential decay law
    expect_equal(class(decay(5, 0.9, 1, -2.8)), "numeric")

    # Logistic probability law
    expect_equal(class(logistic(5, 0.9, 1, -2.8)), "numeric")

})
