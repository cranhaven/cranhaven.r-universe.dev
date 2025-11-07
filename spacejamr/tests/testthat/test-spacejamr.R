library(spacejamr)

skip_on_os("solaris")

test_that("we can create spacejamr objects", {

    path <- system.file("shape", "ri.shp", package = "spacejamr")
    ri <- as.spacejamr(path)

    # Ensures the correct class
    expect_identical(class(ri), c("spacejamr", "owin"))

    # Ensures the correct length
    expect_equal(length(ri), 2)

    # Ensures the correct names for elements in the spacejamr class
    expect_identical(names(ri), c("window", "crs"))
})
