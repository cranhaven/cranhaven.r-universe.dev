context("03. DeviceCategories Discovery Service")
skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")


# Types that start with * can optionally be NULL
expectedFields = list(
    deviceCategoryCode = "character",
    deviceCategoryName = "character",
    description        = "character",
    hasDeviceData      = "logical",
    longDescription    = "character",
    cvTerm             = "list"
)

test_that("01. Get all deviceCategories", {
  result = onc$getDeviceCategories()
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 100)
})

test_that("02. Filter deviceCategoryCode", {
    result = onc$getDeviceCategories(list(deviceCategoryCode = "ADCP1200KHZ"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$deviceCategoryCode, "ADCP1200KHZ")
})

test_that("03. Filter deviceCategoryName", {
    result = onc$getDeviceCategories(list(deviceCategoryName = "Current Profiler 1200"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$deviceCategoryCode, "ADCP1200KHZ")
})

test_that("04. Filter description", {
    result = onc$getDeviceCategories(list(description = "3D Camera"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$deviceCategoryCode, "CAMERA_3D")
})


test_that("05. Filter locationCode", {
    result = onc$getDeviceCategories(list(locationCode = "CQSBG"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("06. Filter propertyCode", {
    result = onc$getDeviceCategories(list(propertyCode = "co2concentration"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("07. Wrong deviceCategoryCode", {
    result = onc$getDeviceCategories(list(locationCode = "XYZ321"))
    expect_equal(length(result), 1)
    expect_true(isErrorResponse(result))
})

test_that("08. No deviceCategories found", {
    result = onc$getDeviceCategories(list(locationCode = "SAAN", propertyCode = "co2concentration"))
    expect_equal(typeof(result), "list")
    expect_equal(length(result), 0)
})
