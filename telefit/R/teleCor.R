#' Pointwise correlations for an exploratory teleconnection analysis
#'
#' Computes empirical correlations between rows of \code{Y} and \code{Z}, 
#' for use as exploratory analysis of teleconnection patterns between locations 
#' indexed by \code{coords.s} and \code{coords.r}.  Optionally, an \code{stData}
#' object containing \code{Y} and \code{Z} can be passed instead.
#'
#' @export
#'
#' @importFrom stats cor
#' 
#' @param Y [ny x nt] a matrix composed of \eqn{ny} row vectors, each of which 
#'   contains \eqn{nt} observations fom a different spatial location.  Spatial 
#'   locations for \code{Y} are indexed by \code{coords.s}.
#' @param Z [nz x nt] a matrix composed of \eqn{nz} row vectors each of which
#'   contains \eqn{nt} observations from a different spatial location.  Spatial
#'   locations for \code{Z} are indexed by \code{coords.r}.
#' @param coords.s coordinates of locations in Y
#' @param coords.r coordinates of locations in Z
#' @param stData stData object containing data to analyze
#' 
#' @return list with a matrix 'cor' containing correlations.  The columns index
#'   remote coordinates, while the rows index the local coordinates.  The 
#'   returned list also includes the coordinates.
#'  
#' @examples
#' 
#' data("coprecip")
#' 
#' cors = teleCor(coprecip)
#' 

teleCor = function( stData = NULL, Y = stData$Y, Z = stData$Z, 
                    coords.s = stData$coords.s, coords.r = stData$coords.r) {
  
  # coerce to a more compact form
  res = list(
    cor = cor(t(Y), t(Z)),
    coords.r = coords.r,
    coords.s = coords.s
  )
  
  class(res) = 'teleCor'
  
  res
}
