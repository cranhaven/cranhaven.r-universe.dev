library(spacejamr)

# Load data for the test
data("RI")
ri_points <- PointSim(3, RI)
pl <- NetSim(ri_points)

test_that("we can plot objects created with the spacejamr package", {

    # Plot method for spacejamr class
    expect_identical(class(plot(RI)), c("gg", "ggplot"))

    # Plot method for PointSim classes
    expect_identical(class(plot(ri_points)), c("gg", "ggplot"))

    # Plot method for NetSim classes
    expect_identical(class(plot(pl)), c("ggraph", "gg", "ggplot"))

})

test_that("print methods work for objects created by the spacejamr package", {

    # Ensures using print.spacejamr does not throw an error
    expect_error(print(RI), regexp = NA)

    # Ensures using print.PointSim does not throw an error
    expect_error(print(ri_points), regexp = NA)

    # Ensures using print.NetSim does not throw an error
    expect_error(print(pl), regexp = NA)
})

test_that("summary methods work for objects created by the spacejamr package", {

    # Ensures using print.spacejamr does not throw an error
    expect_error(summary(RI), regexp = NA)

    # Ensures using print.PointSim does not throw an error
    expect_error(summary(ri_points), regexp = NA)

    # Ensures using print.NetSim does not throw an error
    expect_error(summary(pl), regexp = NA)
})

