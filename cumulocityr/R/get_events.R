# CUMULOCITYR
#
# Copyright (c) 2019, Software AG, Darmstadt, Germany and/or Software AG
# USA Inc., Reston, VA, USA, and/or its subsidiaries and/or its affiliates
# and/or their licensors.
#
# This file is part of the CUMULOCITYR package for R.
#
# The CUMULOCITYR package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# The CUMULOCITYR package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
# #############################################################################

#' Get the events for a device.
#'
#'
#' @inheritParams get_measurements
#'
#' @return A \code{data.frame} if \code{parse_json = TRUE},
#' and a character string otherwise.
#'
#' @details
#'
#' The datetime fields \code{date_from} and \code{date_from} are expected to be
#' strings in the format "YYYY-MM-DDTHH:MM:SSZ".
#'
#' If \code{date_to} is null, it is set to the current time.
#'
#' If num_rows and both dates are specified, the lesser of the two ranges is returned.
#'
#' If \code{parse_json} is TRUE, the JSON object is parsed using \code{jsonlite::fromJSON}
#' before being returned. The data is converted to a single flattened data frame.
#' If a page does not contain any events, it does not get added to the data frame.
#'
#' If \code{parse_json} is FALSE, the JSON object is returned as a JSON string.
#' For queries with multiple pages, a list of such objects is returned. Each
#' element in this list contains up to 2000 records.
#'
#' @details
#' Get the events for a device for a time period.
#'
#' @author Dmitriy Bolotov
#'
#' @references
#' \href{https://cumulocity.com/guides/reference/events/}{Cumulocity Events API}
#'
#'
#' @examples
#' \donttest{
#' get_events(device_id, date_from = "2019-09-30T20:00:00Z")
#' }
#' @export
get_events <- function(device_id,
                       date_from,
                       date_to = NULL,
                       num_rows = NULL,
                       parse_json = TRUE) {
  .check_date(date_from)
  .check_date(date_to)

  .check_if_logical(parse_json)


  if (is.null(date_to)) {
    date_to <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%OSZ")
  }

  df_list <- .get_e_response(
    device_id, date_from,
    date_to, num_rows,
    parse_json
  )

  # If no events, return empty list
  if (length(df_list) == 0) {
    return(df_list)
  }

  # Parse data or not
  if (parse_json == FALSE) {
    return(df_list)
  } else {
    for (x in c(1:length(df_list))) {
      df_list[[x]] <- jsonlite::fromJSON(df_list[[x]], flatten = TRUE)$events
    }

    the_data <- do.call("rbind", df_list)

    return(the_data)
  }
}
