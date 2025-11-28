#' @title
#'  PCORRC. Failure pressure of the corroded pipe
#'
#' @family PCORRC
#'
#' @description
#'  Calculate failure pressure of the corroded pipe according to \emph{PCORRC}
#'  model.
#'
#'  \emph{PCORRC} methodology was developed on the basis of studying the
#'  mechanism of destruction of pipes, material of which has improved or high
#'  fracture toughness, and on the high-precision modeling of the finite element
#'  pipe models performed at the \emph{Battelle Institute}. According to field
#'  test results of a large number of actual pipe segments, the destruction
#'  mechanism for defective pipeline segment depends on the pipe material
#'  fracture toughness. These tests also showed that only pipes made out of
#'  steel with improved or high fracture toughness fail a result of plastic
#'  fracture. In determining the \emph{Folias} factor the effect of increased
#'  stress concentration and steel hardening in the plastic deformation zone at
#'  the start of the defect failure process was taken into account.
#'
#'  This code should be applied only to
#'  \itemize{
#'    \item a single cross section of the pipeline containing a longitudinally
#'          oriented, flat bottom surface defect of corrosion/erosion type;
#'    \item pipelines, which operate at temperatures exceeding the temperature
#'          of pipe material ductile–brittle transition, and for pipematerial
#'          with the impact energy of Charpy 61 [\emph{J}] and above.
#'  }
#'
#' @param d
#'  nominal outside diameter of pipe, [\emph{mm}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param wth
#'  nominal wall thickness of pipe, [\emph{mm}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param uts
#'  ultimate tensile strength (\emph{UTS}) or
#'  specified minimum tensile strength (\emph{SMTS}) as a
#'  characteristic of steel strength, [\eqn{MPa}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param depth
#'  measured maximum depth of the corroded area, [\emph{mm}].
#'  Type: \code{\link{assert_double}}.
#'
#' @param l
#'  measured maximum longitudinal length of corroded area, [\emph{mm}].
#'  Type: \code{\link{assert_double}}.
#'
#' @return
#'  Estimated failure pressure of the corroded pipe, [\eqn{MPa}].
#'  Type: \code{\link{assert_double}}.
#'
#' @references
#'  \enumerate{
#'    \item S. Timashev and A. Bushinskaya, \emph{Diagnostics and Reliability
#'          of Pipeline Systems}, Topics in Safety, Risk, Reliability and Quality 30,
#'          \strong{DOI 10.1007/978-3-319-25307-7}
#'
#'    \item A.C.Reddy, \emph{Safety Failure Criteria of Fluorocarbon Plastic
#'          Pipes for Dry Chlorine Transport using Finite Element Analysis}
#'          Materials today: proceedings, Vol. \strong{4}(8), 2017,
#'          pp. \strong{7498}-\strong{7506}.
#'          \strong{DOI 10.1016/j.matpr.2017.07.081}
#'  }
#'
#' @seealso
#'   Other fail pressure functions: \code{\link{b31gpf}}, \code{\link{b31gmodpf}},
#'   \code{\link{dnvpf}}, \code{\link{shell92pf}}
#'
#' @export
#'
#' @examples
#'  library(pipenostics)
#'
#'  d     <- c(812.8, 219.0)  # [mm]
#'  wth   <- c( 19.1,  14.5)  # [mm]
#'  uts   <- c(530.9, 455.1)   # [N/mm^2]
#'  l     <- c(203.2, 200.0)  # [mm]
#'  depth <- c( 13.4,   9.0)   # [mm]
#'
#'  pcorrcpf(d, wth, uts, depth, l)
#'  # [1] 16.35449 33.01288
#'
pcorrcpf <- function(d, wth, uts, depth, l){
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

  Q <- 1 - exp(-.16*l/sqrt(.5*d*(wth - depth)))
  2*wth*uts/d*(1 - depth/wth*Q)
}

