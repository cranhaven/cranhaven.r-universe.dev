
context("CalculateSignatureBurdens")

dir <- system.file("extdata", package="SATS", mustWork=TRUE)

# Load the baseline object obj0
rdafile <- file.path(dir, "test_objects", "test_CalculateSignatureBurdens.rda")
load(rdafile)

# Load simulated data
data(SimData, package="SATS")

# Call the function to test
obj <- SATS::CalculateSignatureBurdens(SimData$L, SimData$TrueW_TMB, SimData$TrueH)


# Compare result to the baseline. 
test_that("CalculateSignatureBurdens",
{
  expect_equal(obj0, obj, tolerance=1e-4)
})


