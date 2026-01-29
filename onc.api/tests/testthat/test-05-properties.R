skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("05. Properties Discovery Service")

# Types that start with * can optionally be NULL
expectedFields = list(
    propertyCode    = "character",
    propertyName    = "character",
    description     = "character",
    hasDeviceData   = "logical",
    hasPropertyData = "logical",
    uom             = "character",
    cvTerm          = "list"
)

test_that("01. Get all properties", {
  result = onc$getProperties()
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 150)
})

test_that("02. Filter propertyCode", {
    result = onc$getProperties(list(propertyCode = "absolutehumidity"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$propertyCode, "absolutehumidity")

})

test_that("03. Filter propertyName", {
    result = onc$getProperties(list(propertyName = "Bender Electrical Resistance"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$propertyCode, "benderelectricalresistance")
})

test_that("04. Filter description", {
    result = onc$getProperties(list(description = "Kurtosis Statistical Analysis"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$propertyCode, "kurtosisstatisticalanalysis")
})

test_that("05. Filter locationCode", {
    result = onc$getProperties(list(locationCode = "ROVMP"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("06. Filter deviceCategoryCode", {
    result = onc$getProperties(list(deviceCategoryCode = "CTD"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 10)
})

test_that("07. Filter deviceCode", {
    result = onc$getProperties(list(deviceCode = "ALECACTW-CAR0014"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 3)
})

test_that("08. Wrong propertyCode", {
    result = onc$getProperties(list(propertyCode = "XYZ321"))
    expect_equal(length(result), 1)
    expect_true(isErrorResponse(result))
})

test_that("09. No properties found", {
    result = onc$getProperties(list(locationCode = "SAAN", deviceCategoryCode = "POWER_SUPPLY"))
    expect_equal(typeof(result), "list")
    expect_equal(length(result), 0)
})
