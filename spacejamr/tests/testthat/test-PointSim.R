library(spacejamr)

# Data used for testing
data("RI")

test_that("we can simulate point processes", {

    ri_points <- PointSim(points = 10, window = RI)

    # Ensure correct class
    expect_identical(class(ri_points), "PointSim")
})

test_that("we can generate a Halton Sequence", {

    ri_points <- PointSim(points = 10, window = RI, type = halton)

    # Ensure correct class
    expect_identical(class(ri_points), "PointSim")
})
