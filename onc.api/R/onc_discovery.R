# This file contains the functionality that wraps API discovery services


#' Discovery Request
#' encapsulates a request to a discovery service
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters Named list of filters
#' @param service One of {"locations", "deployments", "deviceCategories", "devices",
#'                "properties", "dataProducts"}
#' @param method  One of {"get", "getTree"}
#'
#' @return (named list) Parsed response obtained
.discoveryRequest = function(self, filters = list(), service = "", method = 'get') {
    url <- .serviceUrl(self, service)
    filters <- c(filters, 'method' = method)
    filters <- c(filters, 'token' = self$token)

    result <- .doRequest(self, url, filters)
    result <- .sanitizeBooleans(self, result)
    return(result)
}

#' For all rows in data, enforce that fields expected to have bool values
#' are logical values (fixes API issues with booleans returned as strings)
#' @keywords internal
#'
#' @param self Calling object
#' @param data (list) Parsed response
#'
#' @return     (list) The modified data
.sanitizeBooleans = function(self, data = list()) {
    if (typeof(data) != "list" || length(data) == 0) return(data)

    fixHasDeviceData   <- FALSE
    fixHasPropertyData <- FALSE

    # check hasDeviceData only if present and of the wrong type
    # for now we only check the first row
    names1 = names(data[[1]])
    if ("hasDeviceData" %in% names1) {
        if (typeof(data[[1]][["hasDeviceData"]]) != "boolean") fixHasDeviceData <- TRUE
    }

    # same for hasPropertyData
    if ("hasPropertyData" %in% names1) {
        if (typeof(data[[1]][["hasPropertyData"]]) != "boolean") fixHasPropertyData <- TRUE
    }

    if (fixHasDeviceData || fixHasPropertyData) {
        data <- lapply(data, function(row) {
            if (fixHasDeviceData) {
                row[["hasDeviceData"]]   <- as.logical(row[["hasDeviceData"]] == "true")
            }
            if (fixHasPropertyData) {
                row[["hasPropertyData"]] <- as.logical(row[["hasPropertyData"]] == "true")
            }

            # Sanitize children, if any
            if (!is.null(row$children)) {
                if (length(row$children) > 0) {
                    row$children <- .sanitizeBooleans(self, row$children)
                }
            }

            return(row)
        })
    }

    return(data)
}

#' Request a list of locations that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of locations found, or an error description if any
.getLocations = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'locations')
    return(result)
}

#' Request a hierarchical tree list of locations that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of locations found, or an error description if any
#'                Locations include lists of children locations recursively
.getLocationHierarchy = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'locations', method = 'getTree')
    return(result)
}

#' Request a list of deployments that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of deployments found, or an error description if any
.getDeployments = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'deployments')
    return(result)
}

#' Request a list of devices that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of devices found, or an error description if any
.getDevices = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'devices')
    return(result)
}

#' Request a list of device categories that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of device categories found, or an error description if any
.getDeviceCategories = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'deviceCategories')
    return(result)
}

#' Request a list of properties that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of properties found, or an error description if any
.getProperties = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'properties')
    return(result)
}

#' Request a list of data products that matches the filters provided
#' @keywords internal
#'
#' @param self    Calling object
#' @param filters (named list) search filters
#'
#' @return        (named list) A list of data products found, or an error description if any
.getDataProducts = function(self, filters = list()) {
    result = .discoveryRequest(self, filters = filters, service = 'dataProducts')
    return(result)
}
