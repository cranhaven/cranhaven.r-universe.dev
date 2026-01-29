# This file contains the functionality that wraps API archivefile services


#' Get a list of files for a given location code and device category code, and
#' filtered by others optional parameters.
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) describe the data origin
#' @param allPages When TRUE, if the data is too long to fit a single request,
#'                 multiple pages will be requested until all data is obatined
#'
#' @return (named list) file list obtained
.getListByLocation = function(self, filters = list(), allPages = FALSE) {
    return(.getList(self, filters = filters, by = "location", allPages = allPages))
}


#' Get a list of files available in Oceans 2.0 Archiving System for a given
#' device code. The list of filenames can be filtered by time range.
#' filtered by others optional parameters.
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) describe the data origin
#' @param allPages When TRUE, if the data is too long to fit a single request,
#'                 multiple pages will be requested until all data is obatined
#'
#' @return (named list) file list obtained
.getListByDevice = function(self, filters = list(), allPages = FALSE) {
    return(.getList(self, filters, by = "device", allPages = allPages))
}



#' Download the archive file with filename
#' @keywords internal
#'
#' @param self      Calling object
#' @param filename  Archive file filename
#' @param overwrite When TRUE, downloaded files will overwrite any file with the
#'                  same filename, otherwise file will be skipped
#'
#' @return (named list) Information on the download result
.getFile = function(self, filename = "", overwrite = FALSE) {
    url <- .serviceUrl(self, "archivefiles")

    filters <- list(
        "token"    = self$token,
        "method"   = "getFile",
        "filename" = filename
    )

    r <- .doRequest(self, url = url, filters = filters, getInfo = TRUE, rawResponse = TRUE)
    if (.respFailed(r)) return(r)

    response   <- r$response
    outPath    <- self$outPath
    saveStatus <- .saveAsFile(response, outPath, filename, overwrite)


    txtStatus  <- "error"
    if (r$status == 200) {
        if (saveStatus == 0)       txtStatus <- "completed"
        else if (saveStatus == -2) txtStatus <- "skipped"

        return(list(
            "url"          = .getDownloadUrl(self, filename),
            "status"       = txtStatus,
            "size"         = length(response$content),
            "downloadTime" = round(r$duration, digits = 3),
            "file"         = filename
        ))
    }

    return(list(
        "url"          = "",
        "status"       = txtStatus,
        "size"         = 0,
        "downloadTime" = 0,
        "file"         = ""
    ))
}

#' Downloads all archive files that match the filters
#' Internally will use geListByDevice or getListByLocation and getFile all files
#' @keywords internal
#'
#' @param self      Calling object
#' @param filters   (named list) describe the data origin
#' @param overwrite When TRUE, downloaded files will overwrite any file with the
#'                  same filename, otherwise file will be skipped
#' @param allPages  When TRUE, if the data is too long to fit a single request,
#'                  multiple pages will be requested until all data is obatined
#'
#' @return (named list) Information on the results of the operation, with "downloadResults"
#'         for each file downloaded and general "stats"
.getDirectFiles = function(self, filters = list(), overwrite = FALSE, allPages = FALSE) {
    filNames <- names(filters)

    # make sure we only get a simple list of files
    if ("returnOptions" %in% filNames) {
        filters[["returnOptions"]] <- NULL
    }

    # Get a list of files
    if (("locationCode" %in% filNames) && ("deviceCategoryCode" %in% filNames)) {
        dataRows <- .getListByLocation(self, filters = filters, allPages = allPages)
    }
    else if ("deviceCode" %in% filNames) {
        dataRows <- .getListByDevice(self, filters = filters, allPages = allPages)
    }
    else {
        stop("getDirectFiles filters require either a combination of (locationCode)
              and (deviceCategoryCode), or a (deviceCode) present.")
    }

    n <- length(dataRows$files)
    cat(sprintf("Obtained a list of %d files to download.\n", n))

    # Download the files obtained
    tries <- 1
    successes <- 0
    size <- 0
    time <- 0
    downInfos <- list()
    for (filename in dataRows$files) {
        # only download if file doesn"t exist (or overwrite is True)
        outPath  <- self$outPath
        filePath <- sprintf("%s/%s", outPath, filename)
        fileExists <- file.exists(filePath)

        if (!fileExists || (fileExists && overwrite)) {
            cat(sprintf("   (%d of %d) Downloading file: \"%s\"\n", tries, n, filename))
            downInfo  <- .getFile(self, filename, overwrite)

            # Skip this file if the request failed
            if (.respFailed(downInfo)) {
                cat(sprintf("   Skipping \"%s\" due to an error.\n", filename))
                tries <- tries + 1
                errorInfo <- list(
                    "url"          = .getDownloadUrl(self, filename),
                    "status"       = "error",
                    "size"         = 0,
                    "downloadTime" = 0,
                    "file"         = ""
                )
                downInfos <- .appendList(downInfos, errorInfo)
                next
            }

            size      <- size + downInfo$size
            time      <- time + downInfo$downloadTime
            tries     <- tries + 1

            if (downInfo$status == "completed") {
                successes <- successes + 1
            }
            downInfos <- .appendList(downInfos, downInfo)
        }
        else {
            cat(sprintf("   Skipping \"%s\": File already exists.\n", filename))
            downInfo <- list(
                "url"          = .getDownloadUrl(self, filename),
                "status"       = "skipped",
                "size"         = 0,
                "downloadTime" = 0,
                "file"         = filename
            )
            downInfos <- .appendList(downInfos, downInfo)
        }
    }

    cat(sprintf("%d files (%s) downloaded\n", successes, .formatSize(size)))
    cat(sprintf("Total Download Time: %s\n", .formatDuration(time)))

    return(list(
        "downloadResults" = downInfos,
        "stats" = list(
            "totalSize"    = size,
            "downloadTime" = time,
            "fileCount"    = successes
        )
    ))
}

#' Given a filename, returns an archivefile absolute download URL
#' @keywords internal
#'
#' @param self     Calling object
#' @param filename (character) archive file name
#'
#' @return (character) download URL
.getDownloadUrl = function(self, filename = "") {
    url <- .serviceUrl(self, "archivefiles")
    return(sprintf("%s?method=getFile&filename=%s&token=%s", url, filename, self$token))
}

#' A generic wrapper for getListByLocation() and getListByDevice()
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) describe the data origin
#' @param by       One of: "location", "device"
#' @param allPages When TRUE, if the data is too long to fit a single request,
#'                 multiple pages will be requested until all data is obatined
#'
#' @return (named list) Information on the list of files obtained
.getList = function(self, filters = list(), by = "location", allPages = FALSE) {
    url <- .serviceUrl(self, "archivefiles")
    filters[["token"]]  <- self$token
    filters[["method"]] <- ifelse((by == "location"), "getListByLocation", "getListByDevice")

    # parse and remove the artificial parameter extension
    extension <- ""
    if ("extension" %in% names(filters)) {
        extension <- filters$extension # Don"t remove yet
    }

    if (allPages) {
        mp <- MultiPage$new(self$showInfo, self$timeout)
        result <- mp$getAllPages("archivefiles", url, filters)
    }
    else {
        if ("extension" %in% names(filters)) {
            filters[["extension"]] <- NULL
        }
        result <- .doRequest(self, url, filters)
        result <- .filterByExtension(self, result, extension)
    }
    return(result)
}

#' Filter file list results to only those where the filename ends with the extension
#' If extension is empty no change will be made
#' @keywords internal
#'
#' @param self      Calling object
#' @param results   Results as otained by getListByLocation() or getListByDevice()
#' @param extension (character) Extension to search for (i.e. "txt")
#'
#' @return Filtered list
.filterByExtension = function(self, results = list(), extension = "") {
    if (extension == "") {
        return(results)
    }

    extension <- sprintf(".%s", extension) # match the dot to avoid matching substrings
    n <- stringi::stri_length(extension)
    filtered <- list() # appending is faster than deleting

    # determine the row structure
    rowFormat <- "filename"
    if (length(results$files) > 0) {
        if (typeof(results$files[[1]]) == "list") {
            rowFormat <- "list"
        }
    }

    # filter
    for (file in results$files) {
        if (rowFormat == "filename") {
            fileExt <- stringi::stri_sub(file, from = -n)

            if (fileExt == extension) filtered <- append(filtered, file)
        }
        else {
            fileExt <- stringi::stri_sub(file$filename, from = -n)

            if (fileExt == extension) filtered <- .appendList(filtered, file)
        }
    }

    results$files <- filtered
    return(results)
}
