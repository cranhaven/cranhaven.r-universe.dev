# utility functions

.get_env <- function(the_var) {
  # get value of environment variable, throw error if not found.
  result <- Sys.getenv(the_var)
  if (result == "") {
    stop(paste0(c("env var '", the_var, "' not found."), collapse = ""))
  }
  return(result)
}

.get_cumulocity_base_url <- function() {
  .get_env("CUMULOCITY_base_url")
}

.get_cumulocity_usr <- function() {
  .get_env("CUMULOCITY_usr")
}

.get_cumulocity_pwd <- function() {
  .get_env("CUMULOCITY_pwd")
}

.get_cumulocity_device_id <- function() {
  .get_env("CUMULOCITY_device_id")
}


.get_with_query <- function(url, query) {
  response <- httr::GET(
    url = url,
    query = query,
    httr::authenticate(
      .get_cumulocity_usr(),
      .get_cumulocity_pwd()
    )
  )
}


.check_response_for_error <- function(response, cont_parsed = NULL) {
  # Check for http error; if TRUE, print an error message.

  if (is.null(cont_parsed)) {
    cont <- httr::content(response, "text")
    cont_parsed <- jsonlite::fromJSON(cont)
  }

  error_resp <- httr::http_error(response)
  if (error_resp) {
    if (!is.null(cont_parsed$message)) {
      message_2 <- cont_parsed$message
    } else if (!is.null(cont_parsed$error)) {
      message_2 <- cont_parsed$error
    } else {
      message_2 <- ""
    }

    error_message <- sprintf(
      "Cumulocity API request failed with status code %s.\n\n%s\n%s\n%s\n%s",
      httr::status_code(response),
      paste("Category:         ", httr::http_status(response)$category, sep = ""),
      paste("Reason:           ", httr::http_status(response)$reason, sep = ""),
      paste("Status message:   ", httr::http_status(response)$message, sep = ""),
      paste("Response message: ", message_2, sep = "")
    )
    stop(error_message, call. = FALSE)
  }
}


.check_date <- function(the_date) {
  # TODO: Check that the input is of class POSIXlt or can be converted to POSIXlt.

  # if (inherits(the_date, "POSIXlt")){
  #   the_date <- as.character(the_date)
  # } else {
  #   stop("Dates must be of class POSIXlt.")
  # }

  # the_date <- as.character(the_date)

  if (!is.null(the_date) & !is.character(the_date)) {
    stop("Dates must be of class character.")
  }
}




# .parse_datetime <- function(the_time) {
#   # Parse datetime from char to POSIXct.
#   # strptime(the_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "Z") #POSIXlt
#   as.POSIXct(the_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "Z")
# }


.issue_em_warning <- function(cur_page, type) {
  if (type == "meas") {
    warning(paste("No measurements found on page ",
      toString(cur_page), ".",
      sep = ""
    ))
  } else if (type == "event") {
    warning(paste("No events found on page ",
      toString(cur_page), ".",
      sep = ""
    ))
  }
}

# .get_em_from_response <- function(response, cur_page, type) {
#   # Get events or measurements from response
#   # parse content, check response for error, and issue warning if empty
#
#   cont <- httr::content(response, "text")
#   cont_parsed <- jsonlite::fromJSON(cont)
#
#   .check_response_for_error(response, cont_parsed)
#
#   if (type == "meas") {
#     dat <- cont_parsed$measurements
#     if (!length(dat)) {
#       .issue_em_warning(cur_page, "meas")
#     }
#   } else if (type == "event") {
#     dat <- cont_parsed$events
#     if (!length(dat)) {
#       .issue_em_warning(cur_page, "event")
#     }
#   }
#
#
#
#   return(dat)
# }


.get_content_from_response <- function(response) {
  # Check repsponse for error, get content without parsing, and issue warning if empty

  .check_response_for_error(response = response)

  cont <- httr::content(response, "text")

  # if ((type == "meas") & grepl("measurements\\\":\\[]", cont)) {
  #   .issue_em_warning(cur_page, type)
  # }
  #
  # if ((type == "event") & grepl("events\\\":\\[]", cont)) {
  #   .issue_em_warning(cur_page, type)
  # }

  return(cont)
}


.create_page_sizes <- function(num_rows, pages_per_query) {
  # Create vector of page sizes. Each entry except the last should be 2000.
  if (pages_per_query == 1) {
    page_sizes <- c(num_rows)
  } else {
    page_sizes <- rep(2000, pages_per_query - 1)
    page_sizes[pages_per_query] <- num_rows %% 2000
  }
  return(page_sizes)
}


.check_if_logical <- function(x) {
  if (!is.logical(x)) {
    stop(paste(x, "must be a logical."))
  }
}


.get_m_response <- function(device_id, date_from, date_to, num_rows,
                            parse_json) {
  # Used by get_measurements to get the measurements

  url <- paste0(.get_cumulocity_base_url(),
    "/measurement/measurements",
    collapse = ""
  )

  # Get data
  df_list <- list()
  page_size <- 2000

  if (is.null(num_rows)) {
    cur_page <- 1
    while (TRUE) {
      query <- list(
        source = device_id, pageSize = page_size,
        currentPage = cur_page, dateFrom = date_from,
        dateTo = date_to
      )

      response <- .get_with_query(url, query)

      cont <- .get_content_from_response(response)

      if (grepl("measurements\\\":\\[]", cont)) {
        break # If there are no measurements, exit the loop.
      } else {
        df_list[[cur_page]] <- cont
        cur_page <- cur_page + 1
      }
    }
  } else {
    pages_per_query <- ceiling(num_rows / page_size)
    page_sizes <- .create_page_sizes(num_rows, pages_per_query)

    for (cur_page in c(1:pages_per_query)) {
      query <- list(
        source = device_id, pageSize = page_sizes[cur_page],
        currentPage = cur_page, dateFrom = date_from,
        dateTo = date_to
      )

      response <- .get_with_query(url, query)

      cont <- .get_content_from_response(response)

      if (grepl("measurements\\\":\\[]", cont)) {
        break # If there are no measurements, exit the loop.
      } else {
        df_list[[cur_page]] <- cont
        cur_page <- cur_page + 1
      }
    }
  }
  return(df_list)
}

.get_e_response <- function(device_id, date_from, date_to, num_rows,
                            parse_json) {
  # Used by get_events to get the events

  url <- paste0(.get_cumulocity_base_url(),
    "/event/events",
    collapse = ""
  )

  # Get data
  df_list <- list()
  page_size <- 2000

  if (is.null(num_rows)) {
    cur_page <- 1
    while (TRUE) {
      query <- list(
        source = device_id, pageSize = page_size,
        currentPage = cur_page, dateFrom = date_from,
        dateTo = date_to
      )

      response <- .get_with_query(url, query)

      cont <- .get_content_from_response(response)

      if (grepl("events\\\":\\[]", cont)) {
        break # If there are no events, exit the loop.
      } else {
        df_list[[cur_page]] <- cont
        cur_page <- cur_page + 1
      }
    }
  } else {
    pages_per_query <- ceiling(num_rows / page_size)
    page_sizes <- .create_page_sizes(num_rows, pages_per_query)

    for (cur_page in c(1:pages_per_query)) {
      query <- list(
        source = device_id, pageSize = page_sizes[cur_page],
        currentPage = cur_page, dateFrom = date_from,
        dateTo = date_to
      )

      response <- .get_with_query(url, query)

      cont <- .get_content_from_response(response)

      if (grepl("events\\\":\\[]", cont)) {
        break # If there are no events, exit the loop.
      } else {
        df_list[[cur_page]] <- cont
        cur_page <- cur_page + 1
      }
    }
  }
  return(df_list)
}


.get_dev_response <- function(num_rows, parse_json) {
  # Used by get_devices to get the devices

  url <- paste0(.get_cumulocity_base_url(),
    "/inventory/managedObjects?fragmentType=c8y_IsDevice",
    collapse = ""
  )

  # Get data
  df_list <- list()
  page_size <- 2000

  if (is.null(num_rows)) {
    cur_page <- 1
    while (TRUE) {
      query <- list(
        pageSize = page_size,
        currentPage = cur_page
      )

      response <- .get_with_query(url, query)

      cont <- .get_content_from_response(response)

      if (grepl("managedObjects\\\":\\[]", cont)) {
        break # If there are no managedObjects, exit the loop.
      } else {
        df_list[[cur_page]] <- cont
        cur_page <- cur_page + 1
      }
    }
  } else {
    pages_per_query <- ceiling(num_rows / page_size)
    page_sizes <- .create_page_sizes(num_rows, pages_per_query)

    for (cur_page in c(1:pages_per_query)) {
      query <- list(
        pageSize = page_sizes[cur_page],
        currentPage = cur_page
      )

      response <- .get_with_query(url, query)

      cont <- .get_content_from_response(response)

      if (grepl("managedObjects\\\":\\[]", cont)) {
        break # If there are no managedObjects, exit the loop.
      } else {
        df_list[[cur_page]] <- cont
        cur_page <- cur_page + 1
      }
    }
  }
  return(df_list)
}

# .get_devices <- function(page_size) {
#   # Get object with devices.
#   url <- paste0(.get_cumulocity_base_url(),
#                 "/inventory/managedObjects?fragmentType=c8y_IsDevice",
#                 collapse = ""
#   )
#
#   query <- list(pageSize = page_size)
#   response <- .get_with_query(url, query)
#
#   return(response)
# }
