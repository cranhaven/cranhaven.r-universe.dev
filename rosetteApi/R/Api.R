# Copyright 2016-2019 Basis Technology Corporation.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Rosette API R binding

library(httr)
library(jsonlite)

#' api wrapper function that checks for a user_key and determines the
#' Rosette API endpoint to be utilized
#'
#' @param user_key - Rosette API authentication key
#' @param endpoint - Rosette API endpoint to be utilized
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @param custom_headers - custom headers for Rosette Api
#' @param url_parameters - query parameters
#' @param url - url for Rosette Api
#' @return Returns a list(content, header)
#' @examples
#' \dontrun{
#' parameters <- list()
#' parameters[[ "content" ]] <- "Bill Murray will appear in new Ghostbusters
#' film."
#' response <- api(01234567890, "entities", parameters)
#' # The call above returns response$content and response$header
#' }
#' @export
api <- function(user_key, endpoint, parameters=FALSE, custom_headers=NULL,
                url_parameters=NULL, url="https://api.rosette.com/rest/v1/") {
  if (is.null(user_key)) {
    stop("API key param empty")
  } else {
    if (!is.null(custom_headers)) {
      if (grepl("^X-RosetteAPI-", names(custom_headers)) == FALSE) {
        stop("Custom headers must start with \"X-RosetteAPI-\"")
      }
    }
    if (!endsWith(url, "/")) {
      url <- paste(url, "/", sep = "")
    }



    response <- switch(endpoint,
      "address-similarity" = error_check(
        post_endpoint(user_key, check_address_parameters(parameters),
                      "address-similarity", url, custom_headers, url_parameters)
      ),
      "categories" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "categories", url, custom_headers, url_parameters)
      ),
      "entities" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "entities", url, custom_headers, url_parameters)
      ),
      "info" = error_check(
        get_endpoint(user_key, "info", url, custom_headers, url_parameters)
      ),
      "language" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "language", url, custom_headers, url_parameters)
      ),
      "morphology" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      check_morphology(parameters, "morphology"), url,
                      custom_headers, url_parameters)
      ),
      "name-deduplication" = error_check(
        post_endpoint(user_key, check_deduplication(parameters,
                                                    "name-deduplication"),
                      "name-deduplication", url, custom_headers,
                      url_parameters)
      ),
      "name-similarity" = error_check(
        post_endpoint(user_key, check_names(parameters, "name-similarity"),
                      "name-similarity", url, custom_headers, url_parameters)
      ),
      "name-translation" = error_check(
        post_endpoint(user_key, check_names(parameters, "name-translation"),
                      "name-translation", url, custom_headers, url_parameters)
      ),
      "ping" = error_check(
        get_endpoint(user_key, "ping", url, custom_headers, url_parameters)
      ),
      "relationships" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "relationships", url, custom_headers, url_parameters)
      ),
      "semantics/similar" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "semantics/similar", url, custom_headers, url_parameters)
      ),
      "semantics/vector" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "semantics/vector", url, custom_headers, url_parameters)
      ),
      "sentences" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "sentences", url, custom_headers, url_parameters)
      ),
      "sentiment" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "sentiment", url, custom_headers, url_parameters)
      ),
      "syntax/dependencies" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "syntax/dependencies", url, custom_headers,
                      url_parameters)
      ),
      "text-embedding" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "text-embedding", url, custom_headers, url_parameters)
      ),
      "tokens" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "tokens", url, custom_headers, url_parameters)
      ),
      "topics" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "topics", url, custom_headers, url_parameters)
      ),
      "transliteration" = error_check(
        post_endpoint(user_key, check_content_parameters(parameters),
                      "transliteration", url, custom_headers, url_parameters)
      ),
      stop("Specify a valid Rosette API endpoint")
    )

    return(list("content" = httr::content(response),
                "header" = httr::headers(response)))
  }
}

#' Provides the binding Version
#'
#' @return current binding version
get_binding_version <- function() {
  binding_version <- "1.14.4"
  return(binding_version)
}

#' preemptive check for address-similarity request parameter errors
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @return Returns list of verified parameters to be sent to Rosette API
check_address_parameters <- function(parameters) {
  if (!("address1" %in% names(parameters)) ||
      !("address2" %in% names(parameters))) {
    stop("Must specify both address1 and address2 parameters")
  } else {
    return(parameters)
  }
}

#' preemptive check for content/contentUri request parameter errors
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @return Returns list of verified parameters to be sent to Rosette API
check_content_parameters <- function(parameters) {
  if ("content" %in% names(parameters) && "contentUri" %in% names(parameters)) {
    stop("Cannot specify both content and contentUri parameters")
  } else if (!("content" %in% names(parameters)) &&
             !("contentUri" %in% names(parameters))) {
    stop("Must specify either content or contentUri parameters")
  } else {
    return(parameters)
  }
}

#' determine which morphology endpoint to use
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @param endpoint - Rosette API endpoint to be utilized
#' @return Returns the specified morphology function
check_morphology <- function(parameters, endpoint) {
  if ("morphology" %in% names(parameters)) {
    endpoint <- paste("morphology/", parameters["morphology"], sep = "")
    return(endpoint)
  } else {
    return(endpoint)
  }
}

#' check if the required request parameters for either names endpoint are
#' correct
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @param endpoint - Rosette API endpoint to be utilized
#' @return Returns list of verified parameters to be sent to Rosette API
check_names <- function(parameters, endpoint) {
  params <- parameters
  if (endpoint == "name-translation") {
    if (!("name" %in% names(params)) ||
        !("targetLanguage" %in% names(params))) {
      stop(paste("Must supply name and targetLanguage parameters for ",
                 "name-translation endpoint", sep = ""))
    } else {
      return(parameters)
    }
  } else if (endpoint == "name-similarity") {
    if (!("name1" %in% names(params)) || !("name2" %in% names(params))) {
      stop(paste("Must supply both name1 and name2 parameters for ",
                 "name-similarity endpoint", sep = ""))
    } else {
      return(parameters)
    }
  }
}

#' check if the required request parameters for name deduplication are correct
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @param endpoint - Rosette API endpoint to be utilized
#' @return Returns list of verified parameters to be sent to Rosette API
check_deduplication <- function(parameters, endpoint) {
  params <- parameters
  if (!("names" %in% names(params))) {
    stop("Must supply a list of names to deduplicate")
  } else {
    return(parameters)
  }
}

#' create a multipart
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @return Returns a multipart
create_multipart <- function(parameters) {
  content <- parameters["content"]
  parameters["content"] <- NULL
  parameters <- serialize_parameters(parameters)

  boundary <- "--89dszpjalrbmlsor"
  crlf <- "\r\n"
  multi <- paste(boundary, crlf, sep = "")
  multi <- paste(multi, "Content-Type: application/json", sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, 'Content-Disposition: mixed; name="request"', sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, crlf, sep = "")
  if (length(jsonlite::fromJSON(parameters)) != 0) {
    multi <- paste(multi, parameters, sep = "")
  } else {
    multi <- paste(multi, "{}", sep = "")
  }
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, boundary, sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, "Content-Type: text/plain", sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, 'Content-Disposition: mixed; name="content";', sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, content, sep = "")
  multi <- paste(multi, crlf, sep = "")
  multi <- paste(multi, boundary, sep = "")
  multi <- paste(multi, "--", sep = "")
  return(multi)
}

#' check if Rosette API response includes and error message
#'
#' @param response - response from Rosette API
#' @return Returns an error if one exists or the Rosette API response
error_check <- function(response) {
  if ("message" %in% names(response)) {
    stop(response["message"])
  } else {
    return(response)
  }
}

#' Helper to combine custom headers with the default ones
#'
#' @param user_key - Rosette API authentication key
#' @param content_type - Header Content-Type
#' @param custom_headers - custom headers for the Rosette API
#' @return Returns the combined h
get_headers <- function(user_key, content_type="application/json",
                        custom_headers=NULL) {
  headers <- c("X-RosetteAPI-Key" = user_key, "Content-Type" = content_type,
               "X-RosetteAPI-Binding" = "R",
               "X-RosetteAPI-Binding-Version" = get_binding_version(),
               "user-agent" = get_user_agent())
  if (!is.null(custom_headers)) {
    return(c(headers, custom_headers))
  }
  return(headers)
}

get_user_agent <- function() {
  version <- paste(R.Version()$platform, R.Version()$major, R.Version()$minor,
                   sep = ".")
  return(paste("R", get_binding_version(), version, sep = "/"))
}

#' serialize Rosette API parameters
#'
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @return Returns the serialized parameters for the Rosette API
serialize_parameters <- function(parameters) {
  serialized_params <- list()
  for (param in names(parameters)) {
    if (param == "genre" || param == "profileId" || param == "language" ||
        param == "content" || param == "options" || param == "contentUri" ||
        param == "content_type") {
      serialized_params[[param]] <- parameters[[param]]
    }
  }
  return(jsonlite::toJSON(serialized_params, auto_unbox = TRUE))
}

#' serialize address-similarity parameters
#'
#' @param parameters - parameters list to be passed to address-similarity
#' @return Returns the serialized parameters for the Rosette API
serialize_address_params <- function(parameters) {
  serialized_params <- list()
  for (param in names(parameters)) {
    if (param == "address1" || param == "address2") {
      serialized_params[[param]] <- parameters[[param]]
    }
  }
  return(jsonlite::toJSON(serialized_params, auto_unbox = TRUE))
}

#' serialize Rosette API parameters
#'
#' @param parameters - parameters list to be passed to either name-translation
#'   or name-similarity
#' @return Returns the serialized parameters for the Rosette API
serialize_name_parameters <- function(parameters) {
  serialized_params <- list()
  for (param in names(parameters)) {
    if (param == "name" || param == "targetLanguage" ||
        param == "targetScript" || param == "entityType" ||
        param == "sourceScript" || param == "sourceLanguageOfOrigin" ||
        param == "sourceLanguageOfUse" || param == "targetScheme" ||
        param == "name1" || param == "name2") {
      serialized_params[[param]] <- parameters[[param]]
    }
  }
  return(jsonlite::toJSON(serialized_params, auto_unbox = TRUE))
}

#' serialize Rosette API parameters
#'
#' @param parameters - parameters list to be passed to name-deduplication
#' @return Returns the serialized parameters for the Rosette API
serialize_name_deduplication_parameters <- function(parameters) {
  serialized_params <- list()
  for (param in names(parameters)) {
    if (param == "names" || param == "threshold") {
      serialized_params[[param]] <- parameters[[param]]
    }
  }
  return(jsonlite::toJSON(serialized_params, auto_unbox = TRUE))
}

#' Helper to check for file submission
#'
#' @param parameters - JSON parameters
#' @return true if multipart
check_for_multipart <- function(parameters) {
  return("documentFile" %in% names(parameters))
}

#' httr::POST request to specified Rosette API endpoint
#'
#' @param user_key - Rosette API authentication key
#' @param endpoint - Rosette API endpoint to be utilized
#' @param parameters - parameters list to be passed to specified Rosette API
#'   endpoint
#' @param url - url for Rosette Api
#' @param custom_headers - custom headers for Rosette Api
#' @param url_parameters - query parameters
#' @return Returns the Rosette API response
post_endpoint <- function(user_key, parameters, endpoint, url,
                          custom_headers=NULL, url_parameters=NULL) {
  if (check_for_multipart(parameters)) {
    request_body <- create_multipart(parameters)
    encoding <- "multipart"
    content_type <- "multipart/mixed"
  } else {
    encoding <- "json"
    content_type <- "application/json"
    if (endpoint == "name-translation" || endpoint == "name-similarity") {
      request_body <- serialize_name_parameters(parameters)
    } else if (endpoint == "name-deduplication") {
      request_body <- serialize_name_deduplication_parameters(parameters)
    } else if (endpoint == "address-similarity") {
      request_body <- serialize_address_params(parameters)
    } else {
      request_body <- serialize_parameters(parameters)
    }
  }

  if (is.null(url_parameters)) {
    response <- httr::POST(
      paste(url, endpoint, sep = ""),
      encode = encoding,
      httr::add_headers(get_headers(user_key, content_type = content_type,
                                    custom_headers = custom_headers)),
      body = request_body
    )
  } else {
    response <- httr::POST(
      paste(url, endpoint, sep = ""),
      encode = encoding,
      httr::add_headers(get_headers(user_key, content_type = content_type,
                                    custom_headers = custom_headers)),
      body = request_body,
      query = url_parameters
    )
  }
  return(response)
}

#' httr::GET request to specified Rosette API endpoint
#'
#' @param user_key - Rosette API authentication key
#' @param endpoint - Rosette API endpoint to be utilized
#' @param url - url for Rosette Api
#' @param custom_headers - custom headers for Rosette Api
#' @param url_parameters - url query parameters
#' @return Returns the Rosette API response
get_endpoint <- function(user_key, endpoint, url, custom_headers=NULL,
                         url_parameters=NULL) {
  if (is.null(url_parameters)) {
    response <- httr::GET(
      paste(url, endpoint, sep = ""),
      httr::add_headers(get_headers(user_key, custom_headers))
    )
  } else {
    response <- httr::GET(
      paste(url, endpoint, sep = ""),
      httr::add_headers(get_headers(user_key, custom_headers)),
      query = url_parameters
    )
  }

  return(response)
}
