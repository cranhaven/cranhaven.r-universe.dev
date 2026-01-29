# This file contains the functionality that wraps API real-time service methods


#' Obtains scalar data from a location, from the source described by the filters
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) Filters that describe the data origin
#' @param allPages When TRUE, if the data requested is too large to fit a single
#'                 API resquest, keep downloading data pages until we gather all data
#'
#' @return (named list) Scalar data obtained for all sensors found
.getDirectByLocation = function(self, filters = list(), allPages = FALSE) {
    return(.getDirectAllPages(self, filters = filters, service = "scalardata", method = "getByLocation", allPages = allPages))
}

#' Obtains scalar data from a device, as described by the filters
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) Filters that describe the data origin
#' @param allPages When TRUE, if the data requested is too large to fit a single
#'                 API resquest, keep downloading data pages until we gather all data
#'
#' @return (named list) Scalar data obtained for all sensors found
.getDirectByDevice = function(self, filters = list(), allPages = FALSE) {
    return(.getDirectAllPages(self, filters = filters, service = "scalardata", method = "getByDevice", allPages = allPages))
}

#' Obtains raw data from a location, from the source described by the filters
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) Filters that describe the data origin
#' @param allPages When TRUE, if the data requested is too large to fit a single
#'                 API resquest, keep downloading data pages until we gather all data
#'
#' @return (named list) Raw data obtained for all sensors found
.getDirectRawByLocation = function(self, filters = list(), allPages = FALSE) {
    return(.getDirectAllPages(self, filters = filters, service = "rawdata",    method = "getByLocation", allPages = allPages))
}

#' Obtains raw data from a device, as described by the filters
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) Filters that describe the data origin
#' @param allPages When TRUE, if the data requested is too large to fit a single
#'                 API resquest, keep downloading data pages until we gather all data
#'
#' @return (named list) Raw data obtained for all sensors found
.getDirectRawByDevice = function(self, filters = list(), allPages = FALSE) {
    return(.getDirectAllPages(self, filters = filters, service = "rawdata",    method = "getByDevice", allPages = allPages))
}

#' Generic method to download and concatenate all pages of data
#' Keeps downloading all scalar or raw data pages until finished
#' Automatically translates sensorCategoryCodes to a string if a list is provided
#' @keywords internal
#'
#' @param self     Calling object
#' @param filters  (named list) Filters that describe the data origin
#' @param service  One of: "scalardata", "rawdata"
#' @param method   One of: "getByDevice", "getByLocation"
#' @param allPages When TRUE, if the data requested is too large to fit a single
#'                 API resquest, keep downloading data pages until we gather all data
#'
#' @return A single response in the expected format, with all data pages concatenated
.getDirectAllPages = function(self, filters = list(), service = "", method = "", allPages = FALSE) {
    # prepare filters for first page request
    url <- .serviceUrl(self, service)
    filters[["method"]] <- method
    filters[["token"]]  <- self$token

    # if sensorCategoryCodes is a list, join it into a comma-separated string
    if ("sensorCategoryCodes" %in% names(filters) && typeof(filters$sensorCategoryCodes == "list")) {
        filters$sensorCategoryCodes <- paste(filters$sensorCategoryCodes, sep = ",")
    }

    if (allPages) {
        mp <- MultiPage$new(self$showInfo, self$timeout)
        result <- mp$getAllPages(service, url, filters)
    }
    else {
        result <- .doRequest(self, url, filters)
    }

    return(result)
}
