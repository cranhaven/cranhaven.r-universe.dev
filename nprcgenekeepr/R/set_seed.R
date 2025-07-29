#' Work around for unit tests using sample() among various versions of R
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' The change in how `set.seed` works in R 3.6 prompted the creation of this
#' R version agnostic replacement to get unit test code to work on multiple
#' versions of R in a CICD test build.
#'
#' It seems \code{RNGkind(sample.kind="Rounding")} does not work prior to
#' version 3.6 so I resorted to using version dependent construction of the
#' argument list to set.seed() in do.call().#'
#' @return NULL, invisibly.
#'
#' @param seed argument to \code{set.seed}
#' @export
#' @examples
#' set_seed(1)
#' rnorm(5)
set_seed <- function(seed = 1L) {
  version <- as.integer(R_version()$major) +
    (as.numeric(R_version()$minor) / 10.0)
  if (version >= 3.6) {
    arguments <- list(seed, sample.kind = "Rounding")
  } else {
    arguments <- list(seed)
  }
  suppressMessages(suppressWarnings(do.call(set.seed, arguments)))
}
#' Wrapper for R.Version
#'
#' @returns R.Version() output
#' @noRd
R_version <- function() { # nolint: object_name_linter.
  R.Version()
}
