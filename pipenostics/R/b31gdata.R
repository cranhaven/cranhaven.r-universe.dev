#' ASME B31G. Corrosion state of 12 pipes
#'
#' Data represents examples used for verification of computer program
#' \emph{CRVL.BAS} listed in \emph{Appendix A} of
#' \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}{ASME B31G-1991}.
#'
#' @format A data frame with 12 rows and 15 variables:
#' \describe{
#'   \item{maop}{maximum allowable operating pressure - \emph{MAOP}, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{d}{nominal outside diameter of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{wth}{nominal wall thickness of pipe, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{smys}{specified minimum yield of stress (\emph{SMYS}) as a
#'               characteristics of steel strength, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{def}{appropriate (combined) design factor from
#'              \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.4.2002.pdf}{ASME B31.4},
#'              \href{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31.8.2003.pdf}{ASME B31.8},
#'              or \href{https://www.asme.org/codes-standards/find-codes-standards/b31-11-slurry-transportation-piping-systems}{ASME B31.11},
#'              []. Type: \code{\link{assert_double}}.}
#'   \item{depth}{measured maximum depth of the corroded area, [\emph{inch}].
#'         Type: \code{\link{assert_double}}.}
#'   \item{l}{measured maximum longitudinal length of corroded area, [\emph{inch}].
#'    Type: \code{\link{assert_double}}.}
#'   \item{status}{Operational status of pipe:
#'                 \emph{1} - excellent,
#'                 \emph{2} - monitoring is recommended,
#'                 \emph{3} - alert! replace the pipe immediately!
#'                 Type: \code{\link{assert_numeric}}.
#'                 }
#'   \item{design_pressure}{design pressure of pipe, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{safe_pressure}{safe maximum pressure for the corroded area, [\emph{PSI}]. Type: \code{\link{assert_double}}.}
#'   \item{pressure_exceeding}{whether operator's action is required to reduce
#'                             \emph{MOAP} lower than the maximum safe pressure
#'                             of the corroded area. . Type: \code{\link{assert_logical}}.}
#'   \item{allowed_corrosion_depth}{allowable depth of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{A}{intermediate factor related to the geometry of the corroded area, []. Type: \code{\link{assert_double}}.}
#'   \item{allowed_corrosion_length}{allowable length of the corroded area, [\emph{inch}]. Type: \code{\link{assert_double}}.}
#'   \item{AP}{another intermediate factor related to the geometry of the corroded area, []. Type: \code{\link{assert_double}}.}
#'  }
#' @source \url{https://law.resource.org/pub/us/cfr/ibr/002/asme.b31g.1991.pdf}
"b31gdata"
