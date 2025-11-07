library(spacejamr)

# Data for tests
path <- system.file("extdata", "points.rds", package ="spacejamr")
points <- readRDS(path)
pl <- NetSim(points)

test_that("we can simulate networks from a PointSim object", {

    # Ensures the correct class
    expect_identical(class(pl), c("NetSim", "igraph"))

    # Ensures the correct length
    expect_equal(igraph::vcount(pl), 5)

})

