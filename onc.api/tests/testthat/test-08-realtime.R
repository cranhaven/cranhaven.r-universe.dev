# Settings
skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("08. Real-Time Test Suite")

expectedFields <- list(
    url  = "character",
    status = "character",
    size = "double",
    file = "character",
    index  = "character",
    downloaded = "logical",
    requestCount = "double",
    fileDownloadTime = "double"
)

# Filters

F_SCALAR1 <- list(locationCode = "CRIP.C1", deviceCategoryCode = "CTD", propertyCode = "density",
  dateFrom = "2018-03-24T00:00:00.000Z", dateTo = "2018-03-24T00:00:15.000Z")
# 3 pages of temperature, by location
F_SCALAR2 <- list(locationCode = "CRIP.C1", deviceCategoryCode = "CTD", propertyCode = "density",
  dateFrom = "2018-03-24T00:00:00.000Z", dateTo = "2018-03-24T00:00:15.000Z", rowLimit = "5")
# 3 pages of temperature
F_SCALAR3 <- list(deviceCode = "BARIX001", dateFrom = "2017-06-08T00:00:00.000Z", dateTo = "PT7M", rowLimit = "5")

F_RAW1    <- list(locationCode = "CRIP.C1", deviceCategoryCode = "CTD",
  dateFrom = "2018-03-24T00:00:00.000Z", dateTo = "2018-03-24T00:00:15.000Z")
F_RAW3    <- list(locationCode = "CRIP.C1", deviceCategoryCode = "CTD",
  dateFrom = "2018-03-24T00:00:00.000Z", dateTo = "2018-03-24T00:00:15.000Z", rowLimit = "5")
F_RAWDEV1 <- list(deviceCode = "BARIX001", dateFrom = "2017-06-08T00:00:00.000Z", dateTo = "PT5S")
F_WRONG_FILTERS <- list(locationCode = "ONION", deviceCategoryCode = "POTATO", propertyCode = "BANANA",
    dateFrom = "2018-03-24T00:00:00.000Z", dateTo = "2018-03-24T00:00:10.000Z")
F_NODATA  <- list(locationCode = "CRIP.C1", deviceCategoryCode = "CTD",
    dateFrom = "2015-03-24T00:00:00.000Z", dateTo = "2015-03-24T00:00:10.000Z")


# Test Cases

test_that("1. Get scalar data by location with 1 page", {
    response <- onc$getDirectByLocation(F_SCALAR1)
    sensorData <- response$sensorData[[1]]
    hasKey(sensorData, "data")
    hasKey(sensorData, "sensorCode", "Sensor8_Voltage")
    noNextPage(response)
})

test_that("2. Get scalar data by location with 3 pages", {
    response <- onc$getDirectByLocation(F_SCALAR2, TRUE)
    sensorData <- response$sensorData[[1]]
    hasKey(sensorData, "data")
    hasKey(sensorData, "sensorCode", "Sensor8_Voltage")
    expect_length(sensorData$data$values, 15)
    noNextPage(response)
    #Save Json To File   ${data} out_realtime_2.json
})

test_that("3. Scalar data by location not found for these filters", {
    response <- onc$getDirectByLocation(F_NODATA)
    expect_length(response$sensorData, 0)
})

test_that("4. Scalar data by location with wrong filters", {
    result <- onc$getDirectByLocation(F_WRONG_FILTERS)
    expect_true(isErrorResponse(result))
})

test_that("5. Get raw data by location with 1 page", {
    response <- onc$getDirectRawByLocation(F_RAW1)
    expect_length(response$data$readings, 15)
    noNextPage(response)
})

test_that("6. Get raw data by location with 3 pages", {
    response <- onc$getDirectRawByLocation(F_RAW3, TRUE)
    expect_length(response$data$readings, 15)
    noNextPage(response)
    #Save Json To File   ${data} out_realtime_6.json
})

test_that("7. Raw data by device with 1 page", {
    response <- onc$getDirectRawByDevice(F_RAWDEV1)
    expect_length(response$data$readings, 47)
    noNextPage(response)
})

test_that("8. Raw data not found for these filters", {
    response <- onc$getDirectRawByLocation(F_NODATA)
    expect_length(response$data$readings, 0)
})

test_that("9. Raw data with wrong filters", {
    result <- onc$getDirectRawByLocation(F_WRONG_FILTERS)
    expect_true(isErrorResponse(result))
})

test_that("10. Get scalar data by device with 6 pages", {
    response <- onc$getDirectByDevice(F_SCALAR3, TRUE)
    sensorData <- response$sensorData[[1]]
    hasKey(sensorData, "data")
    hasKey(sensorData, "sensorCode", "analog_input501")
    expect_length(sensorData$data$values, 14)
    noNextPage(response)
})
