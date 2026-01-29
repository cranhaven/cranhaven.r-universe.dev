# Utility methods


#' Response has failed
#'
#' Returns TRUE if the response is a 401 JSON error description provided by the API
#' @keywords internal
#'
#' @param response (named list) Parsed httr response
#' @return (logical)
.respFailed = function(response) {
    if ("errors" %in% names(response)) {
        names1 <- names(response$errors[[1]])
        return(("errorCode" %in% names1) && ("errorMessage" %in% names1))
    }
    return(FALSE)
}

#' Saves the file downloaded in the response object, in the outPath, with filename
#' @keywords internal
#'
#' @param response  An http raw response as returned by httr::GET
#' @param filePath  Path where the file will be saved
#' @param fileName  Name of the file to save
#' @param overwrite If TRUE will overwrite files with the same name
#' @return (numeric) Result code from {0: done, -1: error, -2: fileExists}
.saveAsFile = function(response, filePath = "", fileName = "", overwrite = FALSE) {
    fullPath <- fileName
    if (stringi::stri_length(filePath) > 0) {
        fullPath <- sprintf("%s/%s", filePath, fileName)
        # Create outPath directory if not exists
        if (.prepareDirectory(filePath) == FALSE) {
            cat(sprintf("   ERROR: Could not create ouput path at \"%s\". File \"%s\" was NOT saved.", filePath, fileName))
            return(-1)
        }
    }

    # Save file in outPath if it doesn"t exist yet
    if (overwrite || !file.exists(fullPath)) {
        tryCatch({
            writeBin(httr::content(response, as = "raw"), fullPath)
        },
        error = function(c) {
            return(-1)
        })
    }
    else {
        return(-2)
    }
    return(0)
}

#' Format date as ISO8601 UTC
#'
#' Helper that returns an ISO8601 string for the provided date string
#' Most date formats are supported, as explained in: https://github.com/eddelbuettel/anytime
#' A value of "now" returns the current UTC date & time
#' Depends on the local system clock
#' @keywords internal
#'
#' @param self         Calling object
#' @param dateString   A string that describes a date. Can also be "now"
#' @return (character) Date string
.formatUtc = function(self, dateString = "now") {
    if (dateString == "now") {
        dateNow <- lubridate::now("UTC")
        return(format(dateNow, format = "%Y-%m-%dT%H:%M:%S.000Z"))
    }
    else {
        dateObj <- anytime::anytime(dateString)
        return(format(dateObj, format = "%Y-%m-%dT%H:%M:%S.000Z"))
    }
}

#' Helper for printing a JSON named list to the console
#' Can alternatively print the output to a text file at filePath
#' @keywords internal
#'
#' @param self     self
#' @param obj      named list to print (usually the result of another function)
#' @param filePath (string) if present, creates the file and writes the output in it
.print = function(self, obj, filePath = "") {
    if (filePath == "") {
        .prettyPrint(obj)
        return()
    }
    else {
        # prepare output directory
        fileName <- basename(filePath)
        dirPath  <- dirname(filePath)
        if (dirPath == ".") {
            dirPath = ""
        }
        fullPath <- sprintf("%s/%s", self$outPath, dirPath)
        if (.prepareDirectory(fullPath)) {
            # print to new file
            fc <- file(file.path(fullPath, fileName))
            open(fc, "w")
            if (isOpen(fc)) {
                .prettyPrint(obj, file = fc)
                close(fc)
                return()
            }
        }
    }

    # something went wrong
    cat(sprintf("ERROR: Could not print to file \"%s\"", fullPath))
}

#' Print Error Response Message
#'
#' Prints the information from a API error response to the console
#' as a formatted error message
#' @keywords internal
#'
#' @param response Parsed response returned by httr::GET
.printErrorMessage = function(response) {
    status <- response$status_code
    if (status == 400) {
        cat(sprintf("\nERROR 400 - Bad Request:\n  %s\n", response$url))
        payload <- httr::content(response, as = "parsed")
        if ("errors" %in% names(payload)) {
            for (e in payload$errors) {
                msg = e$errorMessage
                parameters = e$parameter
                cat(sprintf("  %s (parameter: %s)\n", msg, as.character(parameters)))
            }
        }
        else if (status == 401) {
            cat(sprintf("\nERROR 401: Unauthorized - %s\n", response$url))
            cat("Please check that your Web Services API token is valid. Find your token in your registered profile at https://data.oceannetworks.ca.\n")
        }
        else if (status == 500) {
            cat(sprintf("\nERROR 500: Internal Server Error - %s\n", response$url))
            cat("The API failed to process your request. You might want to retry later in case this is a temporary issue (i.e. Maintenance).\n")
        }
        else {
            cat(sprintf("\nERROR %d: The request failed.\n", status))
        }
    }
}

#' Creates directory if it does not exist
#' Supports directory paths with or without filenames
#' @keywords internal
#'
#' @param filepath (character) Directory path
#' @return (logical) TRUE if the directory was created or already exists
.prepareDirectory = function(filepath = "") {
    if (dir.exists(filepath)) {
        return(TRUE)
    }
    else {
        return(dir.create(filepath, showWarnings = FALSE, recursive = TRUE))
    }
}

#' Pretty prints a complex hierarchy of lists
#' @keywords internal
#'
#' @param item  Hierarchy item (can be a list or an atomic element)
#' @param name  (character) Item name if its an element in a named list
#' @param level (numeric)   Depth level in the hierarchy, starting at 0
#' @param comma (logical)   If TRUE, a comma is printed after this element
#' @param file  A file connection to an open file if we should print to it,
#'              or "" (no file, print to console)
.prettyPrint = function(item, name = "", level = 0, comma = FALSE, file = "") {
    spacer <- strrep("  ", level)
    type <- typeof(item)

    # print space and name if needed
    if (level > 0) cat(spacer, file = file)
    if (name != "") cat(sprintf("%s: ", name), file = file)

    if (type == "list") {
        size <- length(item)
        c <- 1
        cat("[", file = file)

        if (is.null(names(item))) {
            # A nameless list
            if (size > 0) {
                if (is.atomic(item[[1]])) {
                    # nameless list of scalars
                    if (typeof(item[[1]]) == "character") {
                        cat(sprintf("\"%s\"", paste(item, collapse = "\", \"")), file = file)
                    }
                    else {
                        cat(paste(item, collapse = ", "), file = file)
                    }
                }
                else {
                    # nameless list of objects
                    cat("\n", file = file)
                    for (i in item) {
                        .prettyPrint(i, name = "", level = (level + 1), comma = (c < size), file = file)
                        c <- c + 1
                    }

                    if (size > 0) cat(spacer)
                }
            }
        }
        else {
            # Named list
            itemNames <- names(item)
            cat("\n", file = file)

            for (n in itemNames) {
                .prettyPrint(item[[n]], name = n, level = (level + 1), comma = (c < size), file = file)
                c <- c + 1
            }

            if (size > 0) cat(spacer, file = file)
        }

        # close list
        cat("]", file = file)
        if (comma) cat(",", file = file)
        cat("\n", file = file)
    }
    else {
        # atomic element
        txtComma <- if (comma) "," else ""
        if (is.null(item)) {
            cat(sprintf("NULL%s\n", txtComma), file = file)
        }
        else if (type == "character") {
            cat(sprintf("\"%s\"%s\n", item, txtComma), file = file)
        }
        else {
            cat(sprintf("%s%s\n", item, txtComma), file = file)
        }
    }
}

#' Returns a formatted time duration string representation of a duration in seconds
#' @keywords internal
#'
#' @param secs (double) Number of seconds
#' @return (character) A readable string for this time duration
.formatDuration = function(secs = 0) {
    if (secs <= 1.0) {
        txtDownTime <- sprintf('%.3f seconds', secs)
    }
    else {
        txtDownTime <- sprintf("%s", lubridate::dseconds(secs))
    }
    return(txtDownTime)
}

#' Returns a formatted file size string representation
#' @keywords internal
#'
#' @param size (float) Size in bytes
#' @return (character) a readable string for this file size
.formatSize = function(size = 0) {
    return(humanize::natural_size(size))
}

#' Prints message to console only when showInfo is true
#' @keywords internal
#'
#' @param self Calling object
#' @param msg  (character) The message to print
.log = function(self, msg = "") {
    if (self$showInfo) {
        cat(msg)
        cat("\n")
    }
}

#' Helper that appends a list item to a list of lists
#' @keywords internal
#'
#' @param li The container list
#' @param item The list to append to li as a new element
#' @return The modified list
.appendList = function(li = list(), item = list()) {
    li[[length(li) + 1]] <- item
    return(li)
}

#' R helper: Returns the last element of a list
#' @keywords internal
#'
#' @param collection The list
#' @return The last element of the list
.lastItem <- function(collection) {
    if (typeof(collection) != "list") {
        return(collection)
    }
    else if (length(collection) == 1) {
        return(collection[[1]])
    }

    el <- utils::tail(collection, n = 1)[[1]]
    return(el)
}

#' Returns a string URL for a base url and a named list of parameters
#' @keywords internal
#'
#' @param url Base url string
#' @param params Named list of parameters
#' @return Full URL
.filters2url <- function(url, params) {
    query <- sprintf('%s?', url)
    for (name in names(params)) {
        query <- sprintf("%s%s=%s&", query, name, params[[name]])
    }
    return(stringi::stri_sub(query, 1, -2))
}
