library(spacejamr)

# Load data for the test
data("RI")
ri_points <- PointSim(3, RI)
pl <- NetSim(ri_points)
apl <- NetSim(ri_points)

test_that("we can compare summary statistics of two networks", {

    # Ensure the output is the correct class
    expect_equal(class(suppressWarnings(compare_networks(pl, apl))),
                 c("tbl_df", "tbl", "data.frame"))

    # Ensure the returned dataframe has the correct number of dimensions
    expect_equal(dim(suppressWarnings(compare_networks(pl, apl))), c(2, 5))

})
