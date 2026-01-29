# Main package class

#' Onc Class
#'
#' Provides convenient & easy access to Ocean Networks Canada's data.
#' For detailed information and usage examples, visit our
#' \href{https://wiki.oceannetworks.ca/display/O2A/Oceans+2.0+API+Home}{official Documentation}.
#'
#' @field token character. User token
#' @field showInfo logical. Print verbose debug comments
#' @field timeout numeric. Number of seconds before a request to the API is canceled
#' @field baseUrl character. Base URL for API requests
#' @field outPath character. Output path for downloaded files
#'
Onc <- setRefClass("Onc",
    fields = list(
        token    = "character",
        showInfo = "logical",
        timeout  = "numeric",
        baseUrl  = "character",
        outPath  = "character"
    ),

    methods = list(
        #' Class initializer
        #'
        #' @param token        User token
        #' @param production   whether the ONC Production server URL is used for service requests
        #' @param showInfo     Whether verbose debug messages are displayed
        #' @param outPath      Output path for downloaded files
        #' @param timeout      Number of seconds before a request to the API is canceled
        initialize = function(token = "", production = TRUE, showInfo = FALSE, outPath = "output", timeout = 60) {
            # basic parameter validation
            stopifnot(is.character(token), (stringi::stri_length(token) > 32))
            stopifnot(is.character(outPath))
            stopifnot(is.logical(production))
            stopifnot(is.logical(showInfo))
            stopifnot(is.numeric(timeout), (timeout >= 0))

            .self$token    <- stri_trim_both(token)
            .self$showInfo <- showInfo
            .self$timeout  <- timeout
            .self$baseUrl  <- "https://data.oceannetworks.ca/"

            # sanitize & store outPath
            path <- stri_trim_both(outPath)
            if (!stri_isempty(path)) {
                path <- stri_replace_all_fixed(path, "\\", "/")
            }
            if (stringi::stri_sub(path,-1) == "/") {
                path <- stringi::stri_sub(path, 1,-2)
            }
            .self$outPath = path

            # switch to qa if needed
            if (!production) {
                .self$baseUrl = "https://qa.oceannetworks.ca/"
            }
        }
    )
)


# Discovery methods
Onc$methods(
    getLocations = function(filters = list()) {
        "Returns a filtered list of locations"
        return(.getLocations(.self, filters))
    },
    getLocationHierarchy = function(filters = list()) {
        "Returns a filtered locations tree"
        return(.getLocationHierarchy(.self, filters))
    },
    getDeployments = function(filters = list()) {
        "Returns a filtered list of deployments"
        return(.getDeployments(.self, filters))
    },
    getDevices = function(filters = list()) {
        "Returns a filtered list of devices"
        return(.getDevices(.self, filters))
    },
    getDeviceCategories = function(filters = list()) {
        "Returns a filtered list of device categories"
        return(.getDeviceCategories(.self, filters))
    },
    getProperties = function(filters = list()) {
        "Returns a filtered list of properties"
        return(.getProperties(.self, filters))
    },
    getDataProducts = function(filters = list()) {
        "Returns a filtered list of data products"
        return(.getDataProducts(.self, filters))
    }
)

# Delivery methods
Onc$methods(
    orderDataProduct = function(filters = list(), maxRetries = 0, downloadResultsOnly = FALSE, includeMetadataFile = TRUE, overwrite = FALSE) {
        "Request, run and download a data product"
        return(.orderDataProduct(.self, filters, maxRetries, downloadResultsOnly, includeMetadataFile, overwrite))
    },
    requestDataProduct = function(filters = list()) {
        "Manually Request a data product"
        return(.requestDataProduct(.self, filters))
    },
    runDataProduct = function(dpRequestId = 0, waitComplete = FALSE) {
        "Manually run a data product request"
        return(.runDataProduct(.self, dpRequestId, waitComplete))
    },
    downloadDataProduct = function(runId = 0, maxRetries = 0, downloadResultsOnly = FALSE, includeMetadataFile = TRUE, overwrite = FALSE) {
        "Manually download a data product after it was requested and run"
        return(.downloadDataProduct(.self, runId, maxRetries, downloadResultsOnly, includeMetadataFile, overwrite))
    }
)

# Real-time methods
Onc$methods(
    getDirectByLocation = function(filters = list(), allPages = FALSE) {
        "Obtain scalar data readings from a device category in a location"
        return(.getDirectByLocation(.self, filters, allPages))
    },
    getDirectByDevice = function(filters = list(), allPages = FALSE) {
        "Obtain scalar data readings from a device"
        return(.getDirectByDevice(.self, filters, allPages))
    },
    getDirectRawByLocation = function(filters = list(), allPages = FALSE) {
        "Obtain raw data readings from a device category in a location"
        return(.getDirectRawByLocation(.self, filters, allPages))
    },
    getDirectRawByDevice = function(filters = list(), allPages = FALSE) {
        "Obtain raw data readings from a device"
        return(.getDirectRawByDevice(.self, filters, allPages))
    },
    getDirectScalar = function(filters = list(), allPages = FALSE) {
        # Alias for legacy method getDirectByLocation (to be eventually removed)
        return(.getDirectByLocation(.self, filters, allPages))
    }
)

# Archive file download methods
Onc$methods(
    getListByLocation = function(filters = list(), allPages = FALSE) {
        "Get a list of archived files for a device category in a location"
        return(.getListByLocation(.self, filters, allPages))
    },
    getListByDevice = function(filters = list(), allPages = FALSE) {
        "Get a list of archived files for a device"
        return(.getListByDevice(.self, filters, allPages))
    },
    getFile = function(filename = "", overwrite = FALSE) {
        "Download a file with the given filename"
        return(.getFile(.self, filename, overwrite))
    },
    getDirectFiles = function(filters = list(), overwrite = FALSE, allPages = FALSE) {
        "Download a list of archived files that match the filters provided"
        return(.getDirectFiles(.self, filters, overwrite, allPages))
    }
)

# Utility methods
Onc$methods(
    print = function(data, filename = "") {
        "Prints a named list in a format easier to read"
        .print(.self, data, filename)
        invisible(.self)
    },
    formatUtc = function(dateString = "now") {
        "Formats the provided date string to meet ISO8601"
        return(.formatUtc(.self, dateString))
    }
)
