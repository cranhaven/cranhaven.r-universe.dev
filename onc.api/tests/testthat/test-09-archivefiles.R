# Archivefiles test suite
# Downloaded files each go to their own test folder to avoid race conditions

# Settings
skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("09. Archive Files Test Suite")

# After we're done, delete the output folder
teardown({
    cleanDirectory("output")
    unlink("output", recursive = TRUE)
})


# Variables

onc0             <- prepareOnc(outPath = "output/09")
F_LOCATION1      <- list("locationCode" = "RISS", "deviceCategoryCode" = "VIDEOCAM", "dateFrom" = "2016-12-01T00:00:00.000Z", "dateTo" = "2016-12-01T01:00:00.000Z")
F_LOCATION3      <- list("locationCode" = "RISS", "deviceCategoryCode" = "VIDEOCAM", "dateFrom" = "2016-12-01T00:00:00.000Z", "dateTo" = "2016-12-01T01:00:00.000Z", "rowLimit" = 5)
F_LOCATIONFULL   <- list("locationCode" = "RISS", "deviceCategoryCode" = "VIDEOCAM", "dateFrom" = "2016-12-01T00:00:00.000Z", "dateTo" = "2016-12-01T01:00:00.000Z", "extension" = "mp4")
F_LOC_RETURN1    <- list("locationCode" = "RISS", "deviceCategoryCode" = "VIDEOCAM", "dateFrom" = "2016-12-01T00:00:00.000Z", "dateTo" = "2016-12-01T01:00:00.000Z", "rowLimit" = 5, "returnOptions" = "archiveLocation")
F_LOC_RETURN2    <- list("locationCode" = "RISS", "deviceCategoryCode" = "VIDEOCAM", "dateFrom" = "2016-12-01T00:00:00.000Z", "dateTo" = "2016-12-03T00:00:00.000Z", "rowLimit" = 50, "returnOptions" = "all", "extension" = "mp4")
F_DEVICE1        <- list("dateFrom" = "2010-01-01T00:00:00.000Z", "dateTo" = "2010-01-01T00:02:00.000Z")
F_DEVICE1EXT     <- list("deviceCode" = "NAXYS_HYD_007", "dateFrom" = "2010-01-01T00:00:00.000Z", "dateTo" = "2010-01-01T00:02:00.000Z", "extension" = "mp3")
F_GETDIRECT_DEV  <- list("dateFrom" = "2010-01-01T00:00:00.000Z", "dateTo" = "2010-01-01T00:00:30.000Z", "deviceCode" = "NAXYS_HYD_007", "returnOptions" = "all")
F_GETDIRECT_LOC  <- list("dateFrom" = "2016-12-01T00:00:00.000Z", "dateTo" = "2016-12-01T01:00:00.000Z", "locationCode" = "RISS", "deviceCategoryCode" = "VIDEOCAM", "extension" = "mp4")
F_FAKE           <- list("locationCode" = "AREA51")

# Test Cases

test_that("1. Get list by location, 1 page", {
    result <- onc0$getListByLocation(F_LOCATION1)
    expect_length(result$files, 15)
})

test_that("2. Get list by location, 3 pages", {
    result <- onc0$getListByLocation(F_LOCATION1, TRUE)
    expect_length(result$files, 15)
})

test_that("3. Get list by location, 1 page, filter by extension", {
    result <- onc0$getListByLocation(F_LOCATIONFULL)
    print(names(result))
    expect_length(result$files, 1)
})

test_that("4. Get list by location, wrong filters", {
    result <- onc0$getListByLocation(F_DEVICE1)
    expect_true(isErrorResponse(result))
})

test_that("5. Get list by device, 1 page, filter by extension", {
    result <- onc0$getListByDevice(F_DEVICE1EXT)
    expect_length(result$files, 4)
})

test_that("6. Get a file", {
    onc9 <- prepareOnc(outPath = "output/09/06")
    onc9$getFile("NAXYS_HYD_007_20091231T235919.476Z-spect-small.png")
    filesInPath(onc9$outPath, 1)
})

test_that("7. Get direct files from device, include returnOptions", {
    onc9 <- prepareOnc(outPath = "output/09/07")
    result <- onc9$getDirectFiles(F_GETDIRECT_DEV)
    filesInPath(onc9$outPath, 12)
    expect_length(result$downloadResults, 12)
})

test_that("8. Get direct files from location, try to overwrite", {
    onc9 <- prepareOnc(outPath = "output/09/08")
    result <- onc9$getDirectFiles(F_GETDIRECT_LOC)
    expect_length(result$downloadResults, 1)
    hasKey(result$downloadResults[[1]], "status", "completed")
    filesInPath(onc9$outPath, 1)
    result <- onc9$getDirectFiles(F_GETDIRECT_LOC)
    expect_length(result$downloadResults, 1)
    hasKey(result$downloadResults[[1]], "status", "skipped")
    filesInPath(onc9$outPath, 1)
})

test_that("9. Wrong getFile filename", {
    onc9 <- prepareOnc(outPath = "output/09/09")
    result <- onc9$getFile("FAKEFILE.XYZ")
    expect_true(isErrorResponse(result))
})

test_that("10. getDirectFile without any data source identifier", {
    onc9 <- prepareOnc(outPath = "output/09/10")
    expect_error(onc9$getDirectFiles(F_FAKE))
})

test_that("11. Get list by device, wrong filters", {
    result <- onc0$getListByDevice(F_FAKE)
    expect_true(isErrorResponse(result))
})

test_that("12. Get list by location, 3 pages, return archiveLocations", {
    result <- onc0$getListByLocation(F_LOC_RETURN1, TRUE)
    expect_length(result$files, 15)
    hasKey(result$files[[1]], "archiveLocation")
})

test_that("13. Get list by device, 3 pages, filter extension, all metadata", {
    result <- onc0$getListByLocation(F_LOC_RETURN2, TRUE)
    expect_length(result$files, 2)
    hasKey(result$files[[1]], "uncompressedFileSize")
})

test_that("14. Save a file to current directory (empty outpath)", {
    filename <- "NAXYS_HYD_007_20091231T235919.476Z-spect-small.png"
    onc9   <- prepareOnc(outPath = "")
    result <- onc9$getFile(filename)
    expect_true(file.exists(filename))
    removeFile(filename) # clean up
})
