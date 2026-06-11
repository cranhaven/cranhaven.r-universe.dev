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

#' Client for the Cumulocity API
#'
#' Access the 'Cumulocity' API and retrieve data on devices, measurements, and events.
#'
#' @section Functions:
#' \itemize{
#' \item \code{\link{get_devices}} returns available devices for a tenant.
#' \item \code{\link{get_measurements}} returns measurements for a device.
#' \item \code{\link{get_events}} returns events for a device.
#' }
#'
#' @section Authentication:
#'
#' The package expects a local .Renviron file with credentials defined as follows:
#'
#' \preformatted{
#' CUMULOCITY_base_url = "[tenant url]"
#' CUMULOCITY_usr = "[username]"
#' CUMULOCITY_pwd = "[password]"
#' CUMULOCITY_device_id = "[an example device id]"
#' }
#'
#' The tenant url should be of the form \code{"https://tenant_name.cumulocity.com"}.
#' \code{CUMULOCITY_device_id} is not required by any of the main functions.
#'
#' \code{.Renviron} can be edited with \code{usethis::edit_r_environ()}.
#'
#' @section Other considerations:
#'
#' Time should be in the following format: "YYYY-MM-DDTHH:MM:SSZ"
#'
#'
#' @section References:
#' \itemize{
#' \item \href{https://cumulocity.com/guides/reference/inventory/}{Cumulocity Inventory API}
#' \item \href{https://cumulocity.com/guides/reference/events/}{Cumulocity Events API}
#' \item \href{https://cumulocity.com/guides/reference/measurements/}{Cumulocity Measurements API}
#' }
#'
#' @docType package
#' @aliases cumulocityr-package
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
