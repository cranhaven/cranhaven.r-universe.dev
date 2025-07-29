#' getVersion Get the version number of nprcgenekeepr
#'
## Copyright(c) 2017-2021 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @return Current Version
#' @param date A logical value when TRUE (default) a date in YYYYMMDD format
#' within parentheses is appended.
#' @importFrom utils packageVersion
#' @importFrom sessioninfo package_info
#' @export
#' @examples
#' library(nprcgenekeepr)
#' getVersion()
getVersion <- function(date = TRUE) {
  version <- packageVersion("nprcgenekeepr")
  if (date) {
    pkg_date <- sessioninfo::package_info("nprcgenekeepr")
    pkg_date <- pkg_date[["date"]][pkg_date[["package"]] == "nprcgenekeepr"]
    paste0(version, " (", pkg_date, ")")
  } else {
    paste0(version)
  }
}
