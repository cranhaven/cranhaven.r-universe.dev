library(testthat)
library(JAGStree)

# create tree and Zhats
data.JAGS <- create_data()

Sys.setenv("RGL_USE_NULL" = TRUE)

# Test creation of tree data type
test_that("makeJAGStree function does not return object in environment", {
    expect_type(with_mock(makeJAGStree(data.JAGS, filename = file.path(tempdir(), "JAGStest.txt"))), "NULL")
})

