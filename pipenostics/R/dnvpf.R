#' @title
#'  DNV-RP-F101. Failure pressure of the corroded pipe
#'
#' @family DNV-RP-F101 functions
#'
#' @description
#'  Calculate failure pressure of the corroded pipe
#'  according to \emph{Section 8.2} of
#'  in \href{https://www.dnv.com/oilgas/download/dnv-rp-f101-corroded-pipelines/}{DNV-RP-F101}.
#'  The estimation is valid for single isolated metal loss defects of
#'  the corrosion/erosion type and when only internal pressure loading
#'  is considered.
#'
#'  The next assumption of the corrosion shape is adopted by
#'  \href{https://www.dnv.com/oilgas/download/dnv-rp-f101-corroded-pipelines/}{DNV-RP-F101}:
#'
#'  \figure{dnvpf.png}
#'
#'  There d\emph{cor} represents argument \code{depth}.
#'
#' @details
#'   In contrast to
#'   \href{https://www.asme.org/codes-standards/find-codes-standards/b31g-manual-determining-remaining-strength-corroded-pipelines}{ASME B31G-2012}
#'   property of pipe metal is characterized by specified minimum tensile
#'   strength - \emph{SMTS}, [\eqn{N/mm^2}], and \href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}
#'   is default unit system. \emph{SMTS} is given in the linepipe steel
#'   material specifications (e.g. \href{https://www.api.org/products-and-services/standards/important-standards-announcements/standard-5l}{API 5L})
#'   for each material grade.
#'
#'   At the same time \emph{Timashev et al.} used ultimate tensile strength
#'   - \href{https://en.wikipedia.org/wiki/Ultimate_tensile_strength}{UTS}
#'   in place of \emph{SMTS}. So, for the case those quantities may be used in
#'   interchangeable way.
#'
#'   Numeric \code{NA}s may appear in case prescribed conditions of
#'   use are offended.
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param uts
#'  ultimate tensile strength (\emph{UTS}) or
#'  specified minimum tensile strength (\emph{SMTS}) as a
#'  characteristic of steel strength, [\emph{MPa}]. Type: \code{\link{assert_double}}.
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of corroded area, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @return
#'  Estimated failure pressure of the corroded pipe, [\emph{MPa}].
#'  Type: \code{\link{assert_double}}.
#'
#' @references
#'  \enumerate{
#'  \item Recommended practice \href{https://www.dnv.com/oilgas/download/dnv-rp-f101-corroded-pipelines/}{DNV-RP-F101}.
#'    Corroded pipelines. \strong{DET NORSKE VERITAS}, October 2010.
#'  \item \href{https://store.accuristech.com:443/standards/asme-b31g-2012-r2017?product_id=1842873}{ASME B31G-2012}.
#'    Manual for determining the remaining strength of corroded pipelines:
#'    supplement to \emph{B31 Code} for pressure piping.
#'  \item  S. Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
#'    of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
#'    \strong{DOI 10.1007/978-3-319-25307-7}.
#'  }
#'
#' @seealso
#'   Other fail pressure functions: \code{\link{b31gpf}}, \code{\link{b31gmodpf}},
#'   \code{\link{shell92pf}}, \code{\link{pcorrcpf}}
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#' d     <- c(812.8, 219.0)  # [mm]
#' wth   <- c( 19.1,  14.5)  # [mm]
#' uts   <- c(530.9, 455.1)  # [N/mm^2]
#' l     <- c(203.2, 200.0)  # [mm]
#' depth <- c( 13.4,   9.0)  # [mm]
#'
#' dnvpf(d, wth, uts, depth, l)
#' # [1] 15.86626 34.01183
#'
dnvpf <- function(d, wth, uts, depth, l){
  checkmate::assert_double(
    d, lower = 1, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    wth, lower = 0, upper = 5e2, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    uts, lower = 5, upper = 2e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_double(
    depth, lower = 0, upper = 1e3, finite = TRUE, any.missing = FALSE,
    min.len = 1L
  )
  checkmate::assert_double(
    l, lower = 0, upper = 5e3, finite = TRUE, any.missing = FALSE, min.len = 1L
  )
  checkmate::assert_true(commensurable(c(
    length(d), length(wth), length(uts), length(depth), length(l)
  )))

  Q <- sqrt(1.0 + .31*l^2/d/wth)
  Pf <- 2.0*wth*uts*(1 - depth/wth)/(d - wth)/(1.0 - depth/wth/Q)
  Pf[depth >= .85*wth] <- NA_real_
  Pf
}

