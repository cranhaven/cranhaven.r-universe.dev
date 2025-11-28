#' API 5L. Values of SMYS and UTS
#'
#' Data represents specified minimum yield strength (SMYS) and ultimate
#' tensile strength (UTS) both achieved when producing line pipes
#' according to
#' \href{https://law.resource.org/pub/us/cfr/ibr/002/api.5l.2004.pdf}{API SPECIFICATION 5L}.
#'
#' @format A data frame with 11 rows and 3 variables:
#'
#' \describe{
#'   \item{grade}{designation of standard grade of manufactured pipe. Type: \code{\link{assert_character}}.}
#'   \item{smys}{SMYS - specified minimum yield strength, [\emph{psi}]. Type: \code{\link{assert_double}}.}
#'   \item{uts}{UTS - ultimate tensile strength, [\emph{psi}]. Type: \code{\link{assert_double}}.}
#'  }
#' @source \url{https://law.resource.org/pub/us/cfr/ibr/002/api.5l.2004.pdf}
"api5l3t"
