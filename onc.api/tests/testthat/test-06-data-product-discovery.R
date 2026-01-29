skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("06. DataProducts Discovery Service")

# Types that start with * can optionally be NULL
expectedFields = list(
    dataProductCode = "character",
    dataProductName = "character",
    extension       = "character",
    hasDeviceData   = "logical",
    hasPropertyData = "logical",
    helpDocument    = "character"
)

test_that("01. Get all data products", {
  result = onc$getDataProducts()
  expect_true(verifyFields(result[[1]], expectedFields))
  expect_gt(length(result), 100)
})

test_that("02. Filter dataProductCode", {
    result = onc$getDataProducts(list(dataProductCode = "CPD"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_equal(length(result), 1)
    expect_equal(result[[1]]$dataProductCode, "CPD")

})

test_that("03. Filter extension", {
    result = onc$getDataProducts(list(extension = "cor"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
    expect_equal(result[[1]]$extension, "cor")
})

test_that("04. Filter locationCode", {
    result = onc$getDataProducts(list(locationCode = "SAAN"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 1)
})

test_that("05. Filter deviceCategoryCode", {
    result = onc$getDataProducts(list(deviceCategoryCode = "CTD"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 20)
})

test_that("06. Filter deviceCode", {
    result = onc$getDataProducts(list(deviceCode = "BC_POD1_AD2M"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 5)
})

test_that("07. Filter propertyCode", {
    result = onc$getDataProducts(list(propertyCode = "oxygen"))
    expect_true(verifyFields(result[[1]], expectedFields))
    expect_gte(length(result), 10)
})

test_that("08. Wrong dataProductCode", {
    result = onc$getDataProducts(list(dataProductCode = "XYZ321"))
    expect_equal(length(result), 1)
    expect_true(isErrorResponse(result))
})

test_that("09. No data products found", {
    result = onc$getDataProducts(list(locationCode = "SAAN", deviceCategoryCode = "POWER_SUPPLY"))
    expect_equal(typeof(result), "list")
    expect_equal(length(result), 0)
})
