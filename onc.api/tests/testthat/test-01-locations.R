skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("01. Locations Discovery Service")

# For the method "get"
expectedFields = list(
  locationCode    = "character",
  locationName    = "character",
  description     = "character",
  deployments     = "integer",
  hasDeviceData   = "logical",
  hasPropertyData = "logical",
  bbox            = "list",
  lon             = "double",
  lat             = "double",
  depth           = "double",
  dataSearchURL   = "character"
)

# For the method "getTree"
expectedTreeFields = list(
  locationCode    = "character",
  locationName    = "character",
  description     = "character",
  hasDeviceData   = "logical",
  hasPropertyData = "logical",
  children        = "list"
)


test_that("01. Get all locations", {
  result = onc$getLocations()
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 500)
})

test_that("02. Get locations hierarchy", {
  result = onc$getLocationHierarchy(list(locationCode = "SAAN"))
  row1 = result[[1]]
  expect_true(verifyFields(row1, expectedTreeFields))
  expect_equal(length(result), 1)
  expect_true("children" %in% names(row1))
  expect_gt(length(row1$children), 2)
})

test_that("03. Filter locationCode", {
  result = onc$getLocations(list(locationCode = "CQSBG"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$locationName, "Bubbly Gulch")
})

test_that("04. Filter locationName", {
  result = onc$getLocations(list(locationName = "Bubbly Gulch"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$locationCode, "CQSBG")
})

test_that("05. Filter deviceCategoryCode", {
  result = onc$getLocations(list(deviceCategoryCode = "CTD"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 50)
})

test_that("06. Filter deviceCode", {
  result = onc$getLocations(list(deviceCode = "NORTEKADCP9917"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gte(length(result), 1)
})

test_that("07. Filter propertyCode", {
  result = onc$getLocations(list(propertyCode = "co2concentration"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gte(length(result), 1)
})

test_that("08. Filter dataProductCode", {
  result = onc$getLocations(list(dataProductCode = "MP4V"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gte(length(result), 20)
})

test_that("09. Filter includeChildren", {
  result = onc$getLocations(list(includeChildren = "true", locationCode = "SAAN"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gte(length(result), 30)
})

test_that("10. ISO Date Range", {
  result = onc$getLocations(list(dateFrom = "2014-02-24T00:00:01.000Z", dateTo = "2014-03-24T00:00:01.000Z"))
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gte(length(result), 100)
})

test_that("11. Wrong locationCode", {
  result = onc$getLocations(list(locationCode = "CQS34543BG"))
  expect_equal(length(result$errors), 1)
  expect_true(isErrorResponse(result))
})

test_that("12. No locations found", {
  result = onc$getLocations(list(locationCode = "SAAN", dateTo = "1995-03-24T00:00:01.000Z"))
  expect_equal(typeof(result), "list")
  expect_equal(length(result), 0)
})

