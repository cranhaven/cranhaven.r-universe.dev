skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")

context("02. Deployments Discovery Service")

# Types that start with * can optionally be NULL
expectedFields = list(
    begin         = "character",
    depth         = "double",
    deviceCode    = "character",
    end           = "character*",
    hasDeviceData = "logical",
    heading       = "double*",
    lat           = "double",
    locationCode  = "character",
    lon           = "double",
    pitch         = "double*",
    roll          = "double*"
)

test_that("01. Get all deployments", {
  result = onc$getDeployments()
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 500)
})

test_that("02. Filter locationCode", {
    result = onc$getDeployments(list(locationCode = "CQSBG"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 2)
})

test_that("03. Filter deviceCategoryCode", {
    result = onc$getDeployments(list(deviceCategoryCode = "CTD"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 50)
})

test_that("04. Filter deviceCode", {
    result = onc$getDeployments(list(deviceCode = "NORTEKADCP9917"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("05. Filter propertyCode", {
    result = onc$getDeployments(list(propertyCode = "co2concentration"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("06. ISO Date Range", {
    result = onc$getDeployments(list(dateFrom = "2014-02-24T00:00:01.000Z", dateTo = "2014-03-24T00:00:01.000Z"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 100)
})

test_that("07. Wrong locationCode", {
    result = onc$getDeployments(list(locationCode = "XYZ123"))
    expect_equal(length(result), 1)
    expect_true(isErrorResponse(result))
})

test_that("08. No deployments found", {
    result = onc$getDeployments(list(locationCode = "SAAN", dateTo = "1995-03-24T00:00:01.000Z"))
    expect_equal(typeof(result), "list")
    expect_equal(length(result), 0)
})
