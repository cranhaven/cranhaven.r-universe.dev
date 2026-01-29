# This file contains the functionality that wraps API data product delivery services


# Default seconds to wait between consecutive download tries of a file
# (when no estimate processing time is available)
.pollPeriod <- 2.0


#' Request, run and download a data product as described by the filters
#' @keywords internal
#'
#' @param self       Caling object
#' @param filters    (named list) Filters that describe this data product
#' @param maxRetries (numeric) Total maximum number of request calls allowed, 0 for no limit
#' @param downloadResultsOnly (logical) When TRUE, files are not downloaded
#'                   By default (FALSE) generated files are downloaded
#' @param metadata   (logical) When TRUE, a metadata file is downloaded,
#'                   otherwise it is skipped
#' @param overwrite  (logical) When TRUE downloaded files will overwrite any file
#'                   with the same filename, otherwise they will be skipped
#'
#' @return (list) A list of results (one named list for each file) with
#'         information on the operation outcome
.orderDataProduct = function(self, filters = list(), maxRetries = 0, downloadResultsOnly = FALSE,
                             metadata = TRUE, overwrite = FALSE) {
    fileList <- list()

    # Request the product
    requestData <- .requestDataProduct(self, filters)
    if (.respFailed(requestData)) return(requestData)

    runData <- .runDataProduct(self, requestData$dpRequestId)
    if (downloadResultsOnly) {
        # Only run and return links
        for (runId in runData$runIds) {
            fileList <- append(fileList, .infoForProductFiles(self, runId, runData$fileCount, metadata))
        }
    }
    else {
        # Run and download files
        for (runId in runData$runIds) {
            fileList <- append(fileList, .downloadProductFiles(self, runId, metadata, maxRetries, overwrite))
        }
    }

    cat("\n")
    .printProductOrderStats(self, fileList, runData)
    return(.formatResult(self, fileList, runData))
}


#' Request a data product generation described by the filters
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) Filters that describe this data product
#'
#' @return Parsed httr response
.requestDataProduct = function(self, filters = list()) {
    filters$method <- "request"
    filters$token  <- self$token

    url <- sprintf("%sapi/dataProductDelivery", self$baseUrl)
    response <- .doRequest(self, url, filters)
    if (.respFailed(response)) return(response)

    .estimatePollPeriod(self, response)
    .printProductRequest(self, response)

    return(response)
}

#' Run a data product generation request
#' @keywords internal
#'
#' @param self         Calling object
#' @param dpRequestId  (numeric) Request id obtained by requestDataProduct()
#' @param waitComplete (logical) When true, will keep polling the server until
#'                     the data product is ready to download
#'
#' @return (named list) information of the run process, or error object
.runDataProduct = function(self, dpRequestId = 0, waitComplete = FALSE) {
    log <- DPLogger$new()
    url <- sprintf("%sapi/dataProductDelivery", self$baseUrl)

    runResult <- list("runIds" = list(), "fileCount" = 0, "runTime" = 0, "requestCount" = 0)
    filters   <- list("method" = "run", "token" = self$token, "dpRequestId" = dpRequestId)

    # run timed run request
    tictoc::tic()
    status = 202
    while (status == 202) {
        r = .doRequest(self, url, filters, "getInfo" = TRUE)
        runResult$requestCount = runResult$requestCount + 1
        status = r$status

        # Repeat only if waitComplete
        if (waitComplete) {
            log$printResponse(r$response);
            if (r$status == 202) {
                Sys.sleep(.pollPeriod)
            }
        }
        else {
            status = 200
        }
    }
    t <- tictoc::toc(quiet = TRUE)
    
    if (.respFailed(r)) return(r)

    response = r$response;
    runResult$fileCount <- response[[1]]$fileCount
    runResult$runTime   <- round(as.numeric(t$toc - t$tic), digits = 3)

    # gather a list of runIds
    for (run in response) {
        runResult$runIds <- append(runResult$runIds, run$dpRunId)
    }

    return(runResult);
}


#' Public wrapper that lets a user download data products manually with a runId
#' Can optionally return just the download links
#' @keywords internal
#'
#' @param self       Calling object
#' @param runId      (numeric) Run ID as provided by runDataProduct()
#' @param maxRetries (numeric) Maximum number of API requests allowed, 0 for no limit
#' @param downloadResultsOnly (logical) When TRUE, files are not downloaded
#'                   By default (FALSE) generated files are downloaded
#' @param includeMetadataFile (logical) When TRUE, a metadata file is downloaded,
#'                   otherwise it is skipped
#' @param overwrite  (logical) When TRUE downloaded files will overwrite any file
#'                   with the same filename, otherwise they will be skipped
#'
#' @return (list) A list of results (one named list for each file) with
#'         information on the operation outcome
.downloadDataProduct = function(self, runId = 0, maxRetries = 0, downloadResultsOnly = FALSE,
                                includeMetadataFile = TRUE, overwrite = FALSE) {
    if (downloadResultsOnly) {
        fileData <- .infoForProductFiles(self, runId, 0, includeMetadataFile)
    }
    else {
        fileData <- .downloadProductFiles(self, runId, includeMetadataFile, maxRetries, overwrite)
    }

    return(fileData)
}

#' Download all data product files for provided run id
#' @keywords internal
#'
#' @param self        Caling object
#' @param runId       Run id returned by .runDataProduct()
#' @param getMetadata When TRUE, the metadata file will be downloaded
#' @param maxRetries  (numeric) Maximum number of API requests allowed, 0 for no limit
#' @param overwrite   (logical) When TRUE downloaded files will overwrite any file
#'                     with the same filename, otherwise they will be skipped
#' @param fileCount   The number of files to download, or 0 if unknown
#'
#' @return a list of named lists, with the results for each file
.downloadProductFiles = function(self, runId = 0, getMetadata = TRUE, maxRetries = 0, overwrite = TRUE, fileCount = 0) {
    fileList <- list()
    index    <- 1
    baseUrl  <- self$baseUrl
    token    <- self$token

    # keep increasing index until fileCount or until we get 404
    timeout <- self$timeout
    cat(sprintf("\nDownloading data product files with runId %d...\n", runId))

    dpf <- DataProductFile$new(dpRunId = runId, index = as.character(index), baseUrl = baseUrl, token = token)

    # loop thorugh file indexes
    doLoop <- TRUE
    while (doLoop) {
        # stop after too many retries
        status <- dpf$download(timeout, .pollPeriod, self$outPath, maxRetries, overwrite)

        if (status == 200 || status == 777) {
            # file was downloaded (200), or downloaded & skipped (777)
            fileList <- .appendList(fileList, dpf$getInfo())
            index <- index + 1
            dpf <- DataProductFile$new(dpRunId = runId, index = as.character(index), baseUrl = baseUrl, token = token)
        }
        else if (status != 202 || (fileCount > 0 && index >= fileCount)) {
            # no more files to download
            doLoop = FALSE
        }
    }

    # get metadata if required
    if (getMetadata) {
        dpf <- DataProductFile$new(dpRunId = runId, index = "meta", baseUrl = baseUrl, token = token)

        status <- dpf$download(timeout, .pollPeriod, self$outPath, maxRetries, overwrite)
        fileList <- .appendList(fileList, dpf$getInfo())
        if (status != 200) {
            cat(sprintf("\n   Metadata file was not saved\n"))
        }
    }

    cat("\nDownload finished.\n")
    return(fileList)
}

#' Returns a list of information lists for each file available for download
#' Returned rows will have the same structure as those returned by DataProductFile$getInfo()
#' @keywords internal
#'
#' @param self        Caling object
#' @param dpRunId     Run id returned by .runDataProduct()
#' @param fileCount   The number of files to download, or 0 if unknown
#' @param getMetadata When TRUE, the metadata file will be included
#'
#' @return a list of named lists, with the results for each file
.infoForProductFiles = function(self, dpRunId = 0, fileCount = 0, getMetadata = FALSE) {
    cat(sprintf("\nObtaining download information for data product files with runId %d...\n", dpRunId))

    # If we don"t know the fileCount, get it from the server (takes longer)
    if (fileCount <= 0) {
        fileCount <- .countFilesInProduct(self, dpRunId)
    }

    # Build a file list of data product file information
    fileList <- list()
    indexes <- seq(1, fileCount)
    if (getMetadata) {
        indexes <- append(indexes, "meta")
    }

    for (index in indexes) {
        dpf <- DataProductFile$new(dpRunId = dpRunId, index = as.character(index), baseUrl = self$baseUrl, token = self$token)
        dpf$setComplete()
        fileList <- .appendList(fileList, dpf$getInfo())
    }

    return(fileList)
}

#' Given a runId, polls the "download" method to count the number of files available
#' Uses HTTP HEAD to avoid downloading the files
#' @keywords internal
#'
#' @param self  Caling object
#' @param runId Run id returned by .runDataProduct()
#'
#' @return (numeric) Number of files available for download
.countFilesInProduct = function(self, runId = 0) {
    url <- sprintf("%sapi/dataProductDelivery", self$baseUrl)
    filters <- list("method" = "download", "token" = self$token, "dpRunId" = runId, "index" = 1)
    status  <- 200
    n <- 0

    while ((status == 200) || (status == 202)) {
        response <- httr::HEAD(url, httr::config(timeout = self$timeout), query = filters)
        status   <- response$status_code
        if (status == 200) {
            # count successful HEAD request
            filters$index <- filters$index + 1
            n <- n + 1
        }
        else if (status == 202) {
            # If the file is still running, wait
            Sys.sleep(.pollPeriod)
        }
    }

    cat(sprintf("   %d files available for download", n))
    return(n)
}

#' Sets a poll period adequate to the estimated processing time
#' Longer processing times require longer poll periods to avoid going over maxRetries
#' @keywords internal
#'
#' @param self     Calling object
#' @param response Response obtained in .requestDataProduct() for the DP request
#'
#' @return (numeric) suggested time between server polls (seconds)
.estimatePollPeriod = function(self, response) {
    # Parse estimated processing time (if the API returns it, which is
    # not the case with archived data products)
    if ("estimatedProcessingTime" %in% names(response)) {
        txtEstimated <- response$estimatedProcessingTime
        parts <- unlist(stringi::stri_split_fixed(txtEstimated, " "))
        if (length(parts) == 2) {
            unit <- parts[[2]]
            factor <- 1
            if (unit == "min") {
                factor <- 60
            }
            else if (unit == "hour") {
                factor <- 3600
            }
            total <- factor * as.numeric(parts[[1]])
            .pollPeriod <- max(0.02 * total, 1.0) # poll every 2%

            # set an upper limit to pollPeriod [sec]
            .pollPeriod = min(.pollPeriod, 1)
        }
    }
}


#' Prints a formatted representation of the total time and size downloaded
#' after the product order finishes
#' @keywords internal
#'
#' @param self     Calling object
#' @param fileList (list) As returned by .downloadProductFiles()
#' @param runInfo  (list) As returned by .runDataProduct()
.printProductOrderStats = function(self, fileList = list(), runInfo = list()) {
    downloadCount <- 0
    downloadTime  <- 0
    size <- 0

    for (file in fileList) {
        size <- size + file$size

        if (file$downloaded) {
            downloadCount <- downloadCount + 1
            downloadTime  <- downloadTime  + file$fileDownloadTime
        }
    }

    # Print run time
    runSeconds <- runInfo$runTime
    cat(sprintf("Total run time: %s\n", .formatDuration(runSeconds)))

    # Print download time
    if (downloadCount > 0) {
        if (downloadTime < 1.0) {
            txtDownTime <- sprintf("%.3f seconds", downloadTime)
        }
        else {
            txtDownTime <- .formatDuration(downloadTime)
        }
        cat(sprintf("Total download Time: %s\n", txtDownTime))

        # Print size and count of files
        cat(sprintf("%d files (%s) downloaded\n", downloadCount, .formatSize(size)))
    }
    else {
        cat("No files downloaded.\n")
    }
}


#' Prints the response after a data product request
#' The request response format might differ depending on the product origin
#' as it can be "assembled" on the fly, or reused from existing products
#' @keywords internal
#'
#' @param self     Calling object
#' @param response Parsed httr response
.printProductRequest = function(self, response) {
    isGenerated <- ("estimatedFileSize" %in% names(response))
    cat(sprintf("Request Id: %d\n", response$dpRequestId))

    if (isGenerated) {
        size <- response[["estimatedFileSize"]] # API returns it as a formatted string
        cat(sprintf("Estimated File Size: %s\n", size))

        if ("estimatedProcessingTime" %in% names(response)) {
            cat(sprintf("Estimated Processing Time: %s\n", response$estimatedProcessingTime))
        }
        else {
            size <- .formatSize(response$fileSize)
            cat(sprintf("File Size: %s\n", size))
            cat("Data product is ready for download.\n")
        }
    }
}


#' Aggregates individual download results obtained in .orderDataProduct()
#' into a list of formatted results to return, and a named list with
#' general stats of the operation
#' @keywords internal
#'
#' @param self     Calling object
#' @param fileList List of individual download results
#' @param runInfo  As returned by .runDataProduct()
#'
#' @return A named list with "downloadResults" (list of download results) and
#'         "stats" (general stats for the full operation)
.formatResult = function(self, fileList = list(), runInfo = list()) {
    size <- 0
    downloadTime <- 0
    requestCount <- runInfo$requestCount

    for (file in fileList) {
        downloadTime <- downloadTime + file$fileDownloadTime
        size         <- size + file$size
        requestCount <- requestCount + file$requestCount
    }

    result <- list(
        "downloadResults" = fileList,
        "stats" = list(
            "runTime"      = round(runInfo$runTime, digits = 3),
            "downloadTime" = round(downloadTime, digits = 3),
            "requestCount" = requestCount,
            "totalSize"    = size
        )
    )

    return(result)
}
