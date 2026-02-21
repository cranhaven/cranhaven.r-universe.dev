
context("GeneratePanelSize")

dir <- system.file("extdata", package="SATS", mustWork=TRUE)

# Load the baseline objects obj0.COSMIC and obj0.signeR
rdafile <- file.path(dir, "test_objects", "test_GeneratePanelSize.rda")
load(rdafile)

# Load simulated data
data(SimData, package="SATS")

# Call the function to test
obj1 <- SATS::GeneratePanelSize(genomic_information=SimData$PanelEx, Class="SBS", SBS_order="COSMIC")
obj2 <- SATS::GeneratePanelSize(genomic_information=SimData$PanelEx, Class="SBS", SBS_order="signeR")

# Compare result to the baseline. 
test_that("GeneratePanelSize",
{
  expect_equal(obj0.COSMIC, obj1, tolerance=1e-8)
  expect_equal(obj0.signeR, obj2, tolerance=1e-8)
})


