#' Base R OS specific alias
#'
#' Data from the R source code of OS specific man help pages.
#' This is to complement `tools::base_aliases_db()` which only provides links for Unix.
#' @format ## `os_alias`
#' A matrix with 33 rows and 5 columns:
#' \describe{
#'   \item{Package}{Package name}
#'   \item{os}{OS system where this applies}
#'   \item{file}{Name of the Rd file.}
#'   \item{Source}{Path to the file.}
#'   \item{Target}{Name of the Target}
#' }
"os_alias"
