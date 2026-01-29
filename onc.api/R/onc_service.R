#' Generic request wrapper for making simple web service requests
#' Will return parsed results, even if they are an error description sent by the API
#' Will stop() if the request fails into an error different from 400
#' @keywords internal
#'
#' @param self    Calling object
#' @param url     Full URL to make the request, without GET parameters
#' @param filters GET parameters
#' @param getInfo When TRUE, will return a list with ("response", "duration", "status"), where
#'                "duration" is the time the request took and "status" is the HTTP response status
#' @param rawResponse When TRUE, the response returned is the unparsed object returned
#'                    by httr::GET, otherwise a parsed named list is returned
#'
#' @return (list) The parsed response
.doRequest = function(self, url = "", filters = list(), getInfo = FALSE, rawResponse = FALSE) {
    # Print request URL if required
    if (self$showInfo) {
        cat(sprintf("Requesting URL:\n   %s\n", .filters2url(self$baseUrl, filters)))
    }

    # Do request
    tictoc::tic("doRequest")
    tryCatch({
        response <- httr::GET(url, config = list(timeout = self$timeout), query = filters)
        status <- response$status_code
    }, error = function(err) {
        stop(err$message)
    })
    t <- tictoc::toc(quiet = TRUE)
    duration <- round(as.numeric(t$toc - t$tic), digits = 3)
    .log(self, sprintf("   Web Service response time: %s", .formatDuration(duration)))

    if (httr::http_error(response)) {
        if (status == 400) {
            .printErrorMessage(response)
            return(httr::content(response, as = "parsed"))
        }
        else if (status == 401) {
            stop("ERROR 401: Unauthorized. Please verify your user token")
        }
        else if (status == 503) {
            stop(paste("ERROR 503: Service unavailable. We could be down for maintenance;",
                       "visit data.oceannetworks.ca for more information."))
        }
        else {
            stop(as.character(httr::http_status(response)$message))
        }
    }

    if (rawResponse) r <- response
    else r <- httr::content(response, as = "parsed")

    if (getInfo) return(list("response" = r, "duration" = duration, "status" = status))
    else return(r)
}

#' Returns the absolute url for a given ONC API service
#' @keywords internal
#'
#' @param self    Calling object
#' @param service One of ONC's API services
#'
#' @return (character) Service's URL
.serviceUrl = function(self, service = "") {
    services <- c("locations", "deployments", "devices", "deviceCategories",
                  "properties", "dataProducts", "archivefiles", "scalardata", "rawdata")
    if (service %in% services) {
        return(sprintf("%sapi/%s", self$baseUrl, service))
    }
    return("")
}
