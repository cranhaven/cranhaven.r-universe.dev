#' getConfigFileName returns the configuration file name appropriate for
#' the system.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Character vector with expected configuration file
#'
#' @param sysInfo object returned by Sys.info()
#' @importFrom stringi stri_detect_fixed
#' @export
#' @examples
#' library(nprcgenekeepr)
#' sysInfo <- Sys.info()
#' config <- getConfigFileName(sysInfo)
getConfigFileName <- function(sysInfo) {
  homeDir <- file.path(Sys.getenv("HOME"))
  if (stri_detect_fixed(toupper(sysInfo[["sysname"]]), "WIND")) {
    configFile <- file.path(homeDir, "_nprcgenekeepr_config")
  } else {
    configFile <- file.path(homeDir, ".nprcgenekeepr_config")
  }
  c(homeDir = homeDir, configFile = configFile)
}
