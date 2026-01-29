# Settings
skip_if_not(exists("token"))
if (!exists("verifyFields", mode = "function")) source("global.R")
context("07. DataProducts Delivery Service")

expectedFields <- list(
    url        = "character",
    status     = "character",
    size       = "double",
    file       = "character",
    index      = "character",
    downloaded = "logical",
    requestCount = "double",
    fileDownloadTime = "double"
)

# Filters

F_DUMMY1 <- list(dataProductCode = "TSSD", extension = "csv", locationCode = "BACAX", deviceCategoryCode = "ADCP2MHZ",
                 dateFrom = "2016-07-27T00:00:00.000Z", dateTo = "2016-07-27T00:00:30.000Z",
                 dpo_dataGaps = "0", dpo_qualityControl = "1", dpo_resample = "none")

F_DUMMY2 <- list(dataProductCode = "TSSP", extension = "png", locationCode = "CRIP.C1", deviceCategoryCode = "CTD",
                 dateFrom = "2019-03-20T00:00:00.000Z", dateTo = "2019-03-20T00:30:00.000Z",
                 dpo_qualityControl = "1", dpo_resample = "none")

F_FAKE   <- list(dataProductCode = "FAKECODE", extension = "XYZ", locationCode = "AREA51", deviceCategoryCode = "AK47",
                 dateFrom = "2019-03-20T00:00:00.000Z", dateTo = "2019-03-20T00:30:00.000Z",
                 dpo_qualityControl = "1", dpo_resample = "none")


# Utility functions

validateRow = function(rows, index="1", status = "complete", downloaded = FALSE) {
    # grab the row at index
    row <- NULL
    for (r in rows) {
        if (r$index == index) {
            row <- r
            break
        }
    }
    expect_false(is.null(row))

    # verify properties
    expect_equal(row$status, status)
    expect_equal(row$downloaded, downloaded)
}



# Test Cases

test_that("01. Order product links only", {
    onc    <- prepareOnc(outPath = "output/07/01")
    result <- onc$orderDataProduct(F_DUMMY1, 100, TRUE, FALSE)
    rows   <- result$downloadResults
    expect_equal(length(rows), 1)
    expect_true(verifyFields(rows[[1]], expectedFields))
    validateRow(rows, index = "1", status = "complete", downloaded = FALSE)
    filesInPath(onc$outPath, 0)
})


test_that("02. Order links with metadata", {
    onc    <- prepareOnc(outPath = "output/07/02")
    result <- onc$orderDataProduct(F_DUMMY1, 100, TRUE, TRUE)
    rows   <- result$downloadResults
    expect_equal(length(rows), 2)
    expect_true(verifyFields(rows[[1]], expectedFields))
    validateRow(rows, index = "1", status = "complete", downloaded = FALSE)
    validateRow(rows, index = "meta", status = "complete", downloaded = FALSE)
    filesInPath(onc$outPath, 0)
})

test_that("03. Order and download", {
    onc    <- prepareOnc(outPath = "output/07/03")
    result <- onc$orderDataProduct(F_DUMMY1, 100, FALSE, FALSE)
    rows   <- result$downloadResults
    expect_equal(length(rows), 1)
    expect_true(verifyFields(rows[[1]], expectedFields))
    validateRow(rows, index = "1", status = "complete", downloaded = TRUE)
    filesInPath(onc$outPath, 1)
})

test_that("04. Order and download multiple", {
    onc    <- prepareOnc(outPath = "output/07/04")
    result <- onc$orderDataProduct(F_DUMMY2, 100, FALSE, FALSE)
    rows   <- result$downloadResults
    expect_equal(length(rows), 2)
    expect_true(verifyFields(rows[[1]], expectedFields))
    validateRow(rows, index = "1", status = "complete", downloaded = TRUE)
    validateRow(rows, index = "2", status = "complete", downloaded = TRUE)
    filesInPath(onc$outPath, 2)
})

test_that("05. Order and download with metadata", {
    onc    <- prepareOnc(outPath = "output/07/05")
    result <- onc$orderDataProduct(F_DUMMY1, 100, FALSE, TRUE)
    rows   <- result$downloadResults
    expect_equal(length(rows), 2)
    expect_true(verifyFields(rows[[1]], expectedFields))
    validateRow(rows, index = "1", status = "complete", downloaded = TRUE)
    validateRow(rows, index = "meta", status = "complete", downloaded = TRUE)
    filesInPath(onc$outPath, 2)
})

test_that("06. Order and download multiple with metadata", {
    onc    <- prepareOnc(outPath = "output/07/06")
    result <- onc$orderDataProduct(F_DUMMY2, 100, FALSE, TRUE)
    rows   <- result$downloadResults
    expect_equal(length(rows), 3)
    expect_true(verifyFields(rows[[1]], expectedFields))
    validateRow(rows, index = "1", status = "complete", downloaded = TRUE)
    validateRow(rows, index = "2", status = "complete", downloaded = TRUE)
    validateRow(rows, index = "meta", status = "complete", downloaded = TRUE)
    filesInPath(onc$outPath, 3)
})

test_that("07. Wrong order request argument", {
    onc    <- prepareOnc(outPath = "output/07/07")
    result <- onc$orderDataProduct(F_FAKE, 100, TRUE, FALSE)
    expect_gte(length(result$errors), 1)
    expect_true(isErrorResponse(result))
})

test_that("08. Manual request, run and download", {
    onc    <- prepareOnc(outPath = "output/07/08")
    reqId  <- onc$requestDataProduct(F_DUMMY1)[["dpRequestId"]]
    runId  <- onc$runDataProduct(reqId)[["runIds"]][[1]]
    rows   <- onc$downloadDataProduct(runId, downloadResultsOnly = FALSE, includeMetadataFile = TRUE)
    expect_equal(length(rows), 2)
    validateRow(rows, index = "1", status = "complete", downloaded = TRUE)
    validateRow(rows, index = "meta", status = "complete", downloaded = TRUE)
    filesInPath(onc$outPath, 2)
})

test_that("09. Manual request, run and download results only", {
    onc    <- prepareOnc(outPath = "output/07/09")
    reqId  <- onc$requestDataProduct(F_DUMMY1)[["dpRequestId"]]
    runId  <- onc$runDataProduct(reqId)[["runIds"]][[1]]
    rows   <- onc$downloadDataProduct(runId, downloadResultsOnly = TRUE, includeMetadataFile = TRUE)
    expect_equal(length(rows), 2)
    validateRow(rows, index = "1", status = "complete", downloaded = FALSE)
    validateRow(rows, index = "meta", status = "complete", downloaded = FALSE)
    filesInPath(onc$outPath, 0)
})

test_that("10. Manual run with wrong argument", {
    onc    <- prepareOnc(outPath = "output/07/10")
    result <- onc$runDataProduct(1234568790)
    onc$print(result)
    expect_true(isErrorResponse(result))
})

test_that("11. Manual download with wrong argument", {
    onc    <- prepareOnc(outPath = "output/07/11")
    expect_error({onc$downloadDataProduct(1234568790, downloadResultsOnly = FALSE, includeMetadataFile = TRUE)})
})
