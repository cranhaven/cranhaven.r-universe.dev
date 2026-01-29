skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("04. Devices Discovery Service")

# Types that start with * can optionally be NULL
expectedFields = list(
    deviceCode    = "character",
    deviceId      = "integer",
    deviceName    = "character",
    deviceLink    = "character",
    dataRating    = "list",
    cvTerm        = "list",
    hasDeviceData = "logical"
)

test_that("01. Get all devices", {
  result = onc$getDevices()
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 300)
})

test_that("02. Filter deviceCode", {
    result = onc$getDevices(list(deviceCode = "NORTEKADCP9917"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$deviceCode, "NORTEKADCP9917")
})

test_that("03. Filter deviceName", {
    result = onc$getDevices(list(deviceName = "Nortek Aquadopp HR-Profiler 2965"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$deviceCode, "BC_POD1_AD2M")
})

test_that("04. Filter locationCode", {
    result = onc$getDevices(list(locationCode = "CQSBG"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("05. Filter deviceCategoryCode", {
    result = onc$getDevices(list(deviceCategoryCode = "CTD"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 100)
})

test_that("06. Filter propertyCode", {
    result = onc$getDevices(list(propertyCode = "co2concentration"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 2)
})

test_that("07. Filter dataProductCode", {
    result = onc$getDevices(list(dataProductCode = "MP4V"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 20)
})

test_that("08. ISO Date Range", {
    result = onc$getDevices(list(dateFrom = "2014-02-24T00:00:01.000Z", dateTo = "2014-03-24T00:00:01.000Z"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 100)
})

test_that("09. Wrong deviceCode", {
    result = onc$getDevices(list(deviceCode = "XYZ321"))
    expect_equal(length(result), 1)
    expect_true(isErrorResponse(result))
})

test_that("10. No devices found", {
    result = onc$getDevices(list(locationCode = "SAAN", dateTo = "1995-03-24T00:00:01.000Z"))
    expect_equal(typeof(result), "list")
    expect_equal(length(result), 0)
})
