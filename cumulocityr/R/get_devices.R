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

#' Get the devices or for a tenant.
#'
#'
#' @param num_rows The number of records to return.
#' @param parse_json If TRUE, parse the JSON object into a data frame.
#'
#' @return A \code{data.frame} if \code{parse_json = TRUE},
#' and a character string otherwise.
#'
#' @details
#' Get the devices for a tenant.
#'
#' If \code{num_rows = NULL} (default), all devices are returned.
#'
#' If \code{parse_json} is TRUE, the JSON object is parsed using \code{jsonlite::fromJSON}
#' before being returned. The data is converted to a single flattened data frame.
#'
#' If \code{parse_json} is FALSE, the JSON object is returned as a JSON string.
#'
#' @author Dmitriy Bolotov
#'
#' @references
#' \href{https://cumulocity.com/guides/reference/inventory/}{Cumulocity Inventory API}
#'
#'
#' @examples
#' \donttest{
#' get_devices()
#' }
#'
#' @export
get_devices <- function(num_rows = NULL,
                        parse_json = TRUE) {
  .check_if_logical(parse_json)


  df_list <- .get_dev_response(
    num_rows,
    parse_json
  )


  # If no devices, return empty list
  if (length(df_list) == 0) {
    return(df_list)
  }


  # Parse data or not
  if (parse_json == FALSE) {
    return(df_list)
  } else {
    for (x in c(1:length(df_list))) {
      df_list[[x]] <- jsonlite::fromJSON(df_list[[x]], flatten = TRUE)$managedObjects
    }

    the_data <- do.call("rbind", df_list)

    # if (parse_datetime) {
    #   the_data$creationTime <- .parse_datetime(the_data$creationTime)
    #   the_data$lastUpdated <- .parse_datetime(the_data$lastUpdated)
    #   the_data$c8y_Availability$lastMessage <- .parse_datetime(the_data$c8y_Availability.lastMessage)
    # }

    return(the_data)
  }
}
