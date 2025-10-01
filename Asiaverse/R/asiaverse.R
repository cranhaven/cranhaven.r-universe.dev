# Asiaverse - A Metapackage for Asian Countries RESTful APIs and Curated Datasets
# Version 0.1.0
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

#' @import ChinAPIs
#' @import JapanAPIs
#' @import SouthKoreAPIs
#' @import IndiAPIs
#' @import IndonesiAPIs
NULL

#' Print summary of the Asiaverse metapackage
#'
#' This function displays a formatted list of the API packages included
#' in the Asiaverse metapackage and their respective versions.
#'
#' @return Invisibly returns the names of the loaded packages.
#' @importFrom cli rule symbol
#' @importFrom utils packageVersion
#' @export
Asiaverse <- function() {
  pkgs <- c(
    "ChinAPIs",
    "JapanAPIs",
    "SouthKoreAPIs",
    "IndiAPIs",
    "IndonesiAPIs"
  )

  cat(cli::rule(center = "Welcome to Asiaverse", line = 2), "\n")
  cat("A Metapackage for Asian Countries RESTful APIs and Curated Datasets.\n\n")

  versions <- vapply(pkgs, function(pkg) {
    as.character(utils::packageVersion(pkg))
  }, character(1))

  pkg_info <- paste0(cli::symbol$tick, " ", format(pkgs, width = 18), " v", versions)
  cat(paste(pkg_info, collapse = "\n"), "\n")

  invisible(pkgs)
}

#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  pkgs <- c(
    "ChinAPIs",
    "JapanAPIs",
    "SouthKoreAPIs",
    "IndiAPIs",
    "IndonesiAPIs"
  )

  versions <- vapply(pkgs, function(pkg) {
    as.character(utils::packageVersion(pkg))
  }, character(1))

  packageStartupMessage(cli::rule(center = "Welcome to Asiaverse", line = 2))
  packageStartupMessage("A Metapackage for Asian Countries RESTful APIs and Curated Datasets.\n")

  pkg_info <- paste0(cli::symbol$tick, " ", format(pkgs, width = 18), " v", versions)
  lapply(pkg_info, packageStartupMessage)

  invisible(NULL)
}
