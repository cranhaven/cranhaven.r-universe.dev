# A helper for DataProductFile
# Keeps track of the messages printed in a single product download process


#' MultiPage
#'
#' API multiple page helper. Handles the download of data in multiple pages
#' Used by real-time and archivefile services
#' @keywords internal
#'
#' @field showInfo As provided by the Onc class
#' @field timeout  As provided by the Onc class
MultiPage <- setRefClass("MultiPage",
    fields = list(
        # Copy these from the onc object
        showInfo = "logical",
        timeout  = "numeric"
    ),
    methods = list(
        #' Class initializer
        initialize = function(showInfo, timeout) {
            .self$showInfo <- showInfo
            .self$timeout  <- timeout
        },

        #' Get all pages
        #'
        #' Obtains all data pages for a query with the filters
        #' Multiple pages will be downloaded in sequence until completed
        #' Each new page gets concatenated to the previous one (limited to RAM)
        #'
        #' @param service One of: "archivefiles", "scalardata", "rawdata"
        #' @return: Response with concatenated data for all pages obtained
        getAllPages = function(service = "", url = "", filters = list()) {

            # pop archivefiles extension
            extension <- ""
            if (service == "archivefiles") {
                if ("extension" %in% names(filters)) {
                    extension <- filters$extension
                    filters[["extension"]] <- NULL
                }
            }

            # download first page
            tic()
            r  <- .self$doPageRequest(url, filters, service, extension)
            response <- r$response
            duration <- r$duration
            rNext <- response[["next"]]

            if (!is.null(rNext)) {
                cat("Data quantity is greater than the row limit and will be downloaded in multiple pages.\n")

                pageCount <- 1
                pageEstimate <- .self$.estimatePages(response, service, duration)
                if (pageEstimate > 0) {
                    timeEstimate <- .formatDuration(pageEstimate * duration)
                    cat(sprintf("Estimated approx. %d pages\n", pageEstimate))
                    cat(sprintf("Estimated approx. %s to complete\n", timeEstimate))
                }

                # keep downloading pages until next is None
                cat("\n")
                while (!is.null(rNext)) {
                    pageCount <- pageCount + 1
                    rowCount  <- .self$.rowCount(response, service)

                    cat(sprintf("   (%d samples) Downloading page %d...\n", rowCount, pageCount))
                    r2 <- .self$doPageRequest(url, rNext$parameters, service, extension)
                    response <- .self$.catenateData(response, r2$response, service) # concatenate new data obtained

                    rNext <- r2$response[["next"]]
                }

                t <- tictoc::toc(quiet = TRUE)
                duration  <- round(as.numeric(t$toc - t$tic), digits = 3)
                rowCount  <- .self$.rowCount(response, service)
                response[["next"]] <- list()
                cat(sprintf("   (%d samples) Completed in %s.\n", rowCount, .formatDuration(duration)))
            }

            return(response)
        },

        #' Do a page request
        #'
        #' Wraps the _doRequest method
        #' Performs additional processing of the response for certain services
        #'
        #' @param extension: Only provide for archivefiles filtering by filename extension
        #' @return (list) A named list with: "response" (httr response) and "duration" (secs)
        doPageRequest = function(url = "", filters = list(), service = "", extension = "") {
            if (service == "archivefiles") {
                r <- .doRequest(.self, url = url, filters = filters, getInfo = TRUE)
                r$response <- .filterByExtension(.self, r$response, extension)
            }
            else {
                r <- .doRequest(.self, url = url, filters = filters, getInfo = TRUE)
            }

            return(r)
        },

        #' Concatenate data
        #'
        #' Concatenates the data results from nextResponse into response
        #' Compatible with the row structure of scalardata, rowdata and archivefiles
        #'
        #' @param response     (named list) Original response
        #' @param nextResponse (named list) Response with the next data page to add
        #' @param service      (character)  One of: "scalardata", "rawdata", "archivefiles"
        #' @return (named list) Modified original response
        .catenateData = function(response, nextResponse, service = "") {
            if (service == "scalardata") {
                keys <- names(response$sensorData[[1]]$data)

                response$sensorData <- lapply(response$sensorData, function(sensor) {
                    sensorCode <- sensor$sensorCode

                    # get next page for this sensor (same sensorCode)
                    nextSensor <- NA
                    for (nextSensor in nextResponse$sensorData) {
                        if (nextSensor$sensorCode == sensorCode) {
                            # append all keys and stop
                            for (key in keys) {
                                sensor$data[[key]] <- append(sensor$data[[key]], nextSensor$data[[key]])
                            }
                            break
                        }
                    }

                    return(sensor)
                })
            }
            else if (service == "rawdata") {
                for (key in names(response$data)) {
                    response$data[[key]] <- append(response$data[[key]], nextResponse$data[[key]])
                }
            }
            else if (service == "archivefiles") {
                response$files <- append(response$files, nextResponse$files)
            }

            return(response)
        },

        #' Estimate pages
        #'
        #' Estimates the number of pages this request will require to download,
        #' from the first page's response and its duration
        #'
        #' @param response      (named list) Response with the first page
        #' @param service       (character)  One of: "scalardata", "rawdata", "archivefiles"
        #' @param responseTime: (double)     Request duration in seconds
        #' @return (int) Estimated number of pages
        .estimatePages = function(response, service = "", responseTime = 0) {
            # timespan (secs) covered by the data in the response
            pageTimespan <- .self$.responseTimespan(response, service)
            if (pageTimespan == 0) {
                return(0)
            }

            # total timespan to cover
            totalBegin <- ymd_hms(response[["next"]]$parameters$dateFrom)
            totalEnd   <- ymd_hms(response[["next"]]$parameters$dateTo)
            totalSeconds <- as.numeric(totalEnd - totalBegin, units = "secs")

            # handle cases of very small timeframes
            pageSeconds  <- max(pageTimespan, 1)

            return(ceiling(totalSeconds / pageSeconds))
        },

        # Returns the number of records in the response
        .rowCount = function(response, service = "") {
            if (service == "scalardata") {
                return(length(response$sensorData[[1]]$data$sampleTimes))
            }
            else if (service == "rawdata") {
                return(length(response$data$times))
            }
            else if (service == "archivefiles") {
                return(length(response$files))
            }

            return(0)
        },

        #' Response Time Span
        #'
        #' Determines the timespan covered by the data in the response
        #'
        #' @param response (named list) Response with the first page
        #' @param service  (character)  One of: "scalardata", "rawdata", "archivefiles"
        #' @return         (int)        page duration in seconds
        .responseTimespan = function(response, service = "") {
            # grab the first and last sample times
            if (service == "scalardata" || service == "rawdata") {
                if (service == "scalardata") {
                    sampleTimes <- response$sensorData[[1]]$data$sampleTimes
                    first <- sampleTimes[[1]]
                    last  <- .lastItem(sampleTimes)
                }
                else if (service == "rawdata") {
                    first <- head(response$data$times, n = 1)[[1]]
                    last  <- .lastItem(response$data$times)
                }
            }
            else if (service == "archivefiles") {
                row0 <- response$files[[1]]
                if (typeof(row0) == "character") {
                    # extract the date from the filename
                    regExp  <- "\\d{8}T\\d{6}\\.\\d{3}Z"
                    nameFirst <- response$files[[1]]
                    nameLast  <- .lastItem(response$files)
                    first <- stri_extract_first_regex(nameFirst, regExp)
                    last  <- stri_extract_first_regex(nameLast,  regExp)
                    if (is.na(first) || is.na(last)) {
                        return(0)
                    }
                }
                else {
                    first <- response$files[[1]]$dateFrom
                    last  <- .lastItem(response$files)$dateFrom
                }
            }


            # compute the timespan, return as duration object
            dateFirst <- ymd_hms(first)
            dateLast  <- ymd_hms(last)
            return(as.numeric(dateLast - dateFirst, "secs"))
        }
    )
)
