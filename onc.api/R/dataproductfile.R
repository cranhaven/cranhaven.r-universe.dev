#' DataProductFile class
#'
#' Encapsulates the download of a single data product file, and the handling
#' of the server polling and error codes.
#' @keywords internal
#'
#' @field .filters          The list of filters to pass to the download API method
#' @field .retries          Total count of HTTP requests made by this object
#' @field .status           Last request's HTTP status code
#' @field .downloaded       TRUE if the file was downloaded from the API
#' @field .baseUrl          Same as in the Onc class
#' @field .downloadUrl      URL that downloads this file
#' @field .fileName         Filename of the file if downloaded & saved
#' @field .fileSize         Size in bytes of the file if downloaded & saved
#' @field .runningTime      Total time spent running (sec)
#' @field .downloadingTime  Total time spend downloading (sec)
DataProductFile <- setRefClass("DataProductFile",
    fields = list(
        .filters     = "list",
        .retries     = "numeric",
        .status      = "numeric",
        .downloaded  = "logical",
        .baseUrl     = "character",
        .downloadUrl = "character",
        .fileName    = "character",
        .fileSize    = "numeric",
        .runningTime = "numeric",
        .downloadingTime = "numeric",
        .showInfo    = "logical"
    ),
    methods = list(
        #' Class initializer
        #'
        #' @param dpRunId Run Id of a data product request that was run
        #' @param index   Index of this object's file
        #' @param baseUrl From the Onc class
        #' @param token   From the Onc class
        initialize = function(dpRunId = 0, index = "", baseUrl = "", token = "", showInfo = FALSE) {
            .self$.showInfo    <- showInfo
            .self$.retries     <- 0
            .self$.status      <- 202
            .self$.downloaded  <- FALSE
            .self$.baseUrl     <- sprintf("%sapi/dataProductDelivery", baseUrl)
            .self$.fileName    <- ""
            .self$.fileSize    <- 0
            .self$.runningTime <- 0
            .self$.downloadingTime <- 0

            .self$.filters <- list(
                "method"  = "download",
                "token"   = token,
                "dpRunId" = dpRunId,
                "index"   = index
            )
            # prepopulate download URL in case download() never happens
            .self$.downloadUrl <- sprintf("%s?method=download&token=%s&dpRunId=%s&index=%s", baseUrl, token, dpRunId, index)
        },

        #' Downloads this data product file
        #'
        #' Can poll, wait and retry if the file is not ready to download
        #' @param  overwrite When TRUE, existing files will be overwritten, otherwise they are skipped
        #' @param  timeout,pollPeriod,outPath,maxRetries Same as in the Onc class
        #' @return (integer) the final response's HTTP status code
        download = function(timeout = 0, pollPeriod = 0, outPath = "", maxRetries = 0, overwrite = FALSE) {
            log <- DPLogger$new()

            .self$.status <- 202
            saveResult <- 0
            while (.self$.status == 202) {
                # Run timed request
                if (.self$.showInfo) {
                    cat(sprintf("Requesting URL:\n   %s\n", .filters2url(.self$baseUrl, .self$.filters)))
                }
                tictoc::tic()
                response <- GET(.self$.baseUrl, config = list(timeout = timeout), query = .self$.filters)
                t <- tictoc::toc(quiet = TRUE)
                duration <- round(t$toc - t$tic, digits = 3)

                .self$.downloadUrl <- response$url
                .self$.status      <- response$status_code
                .self$.retries     <- .self$.retries + 1

                if (maxRetries > 0 && .self$.retries > maxRetries) {
                    log$printLine(sprintf("ERROR: Maximum number of retries (%d) exceeded", maxRetries))
                    return(408)
                }

                # Status 200: file downloaded, 202: processing, 204: no data, 400: error, 404: index out of bounds, 410: gone (file deleted from FTP)
                s <- .self$.status
                if (s == 200) {
                    # File downloaded, get filename from header and save
                    .self$.downloaded <- TRUE
                    .self$.downloadingTime <- round(duration, digits = 3)
                    filename          <- .self$extractNameFromHeader(response)
                    .self$.fileName   <- filename
                    .self$.fileSize   <- length(response$content)
                    saveResult        <- .saveAsFile(response, outPath, filename, overwrite)

                    # log status
                    if (saveResult == 0) {
                        log$printLine(sprintf("Downloaded \"%s\"", .self$.fileName))
                    }
                    else if (saveResult == -2) {
                        log$printLine(sprintf("Skipping \"%s\": File already exists", .self$.fileName))
                        .self$.status <- 777
                    }
                }
                else if (s == 202) {
                    # Still processing, wait and retry
                    log$printResponse(content(response, as = "parsed"))
                    Sys.sleep(pollPeriod)
                }
                else if (s == 204) {
                    # No data found
                    log$printLine("No data found.")
                }
                else if (s == 400) {
                    # API Error
                    .printErrorMessage(response)
                    stop(sprintf("The request failed with HTTP status %d", .self$.status))
                }
                else if (s == 404) {
                    # Index too high, no more files to download

                }
                else {
                    # Gone
                    log$printLine(sprintf("FTP Error: File not found. If this product order is recent,
                        try downloading this product using the method downloadProduct with the runId: %d\n", .self$.filters$runId))
                    .printErrorMessage(response)
                }
            }

            return(.self$.status)
        },

        #' Return the file name from the response
        #'
        #' @param response The successful (200) httr response obtained from a download request
        #' @return (string) The filename as obtained from the headers
        extractNameFromHeader = function(response) {
            head <- headers(response)
            txt <- head[["Content-Disposition"]]
            filename <- unlist(stringi::stri_split_fixed(txt, "filename="))[[2]]
            return(filename)
        },

        #' Return information on this download's outcome
        #'
        #' @return (list) A named list with information on the download result
        getInfo = function() {
            errorCodes <- list(
                "200" = "complete",
                "202" = "running",
                "204" = "no content",
                "400" = "error",
                "401" = "unauthorized",
                "404" = "not found",
                "410" = "gone",
                "500" = "server error",
                "777" = "skipped"
            )

            txtStatus <- errorCodes[[as.character(.self$.status)]]

            return(list(
                "url"              = .self$.downloadUrl,
                "status"           = txtStatus,
                "size"             = as.double(.self$.fileSize),
                "file"             = .self$.fileName,
                "index"            = .self$.filters[["index"]],
                "downloaded"       = .self$.downloaded,
                "requestCount"     = .self$.retries,
                "fileDownloadTime" = as.numeric(.self$.downloadingTime)
            ))
        },

        #' Sets this object's status to 200 (complete)
        #' Used by onc_delivery methods
        setComplete = function() {
            .self$.status <- 200
        }
    )
)
