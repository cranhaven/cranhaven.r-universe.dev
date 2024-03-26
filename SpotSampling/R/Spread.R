#' @title Spreading measure of several spatial samples
#'
#'
#' @description Measure the spread of several spatial samples depending on inclusion probabilities and spatial coordinates.
#' Two spreading criteria are available: one based on the space partition of Voronoi polygons and one based on Moran's I index (see references).
#'
#'
#' @param S a matrix that contains samples in columns. Matrix rows correspond to the units.
#' It could be matrix of temporal samples returned by function \code{\link{Spot}}.
#' @param pik a matrix of temporal inclusion probabilities.
#' Columns of \code{pik} correspond to samples, and rows correspond to the units.
#' @param coord a matrix that contains spatial coordinates in columns. Matrix rows correspond to the units.
#' @param criteria it specifies the criteria used to measure samples spreading. \code{criteria = "IB"}: the criteria based on Moran's I index is used (see \code{\link[WaveSampling:IB]{IB}}), \code{criteria = "sb"}: the criteria based on Moran's I index is used (see \code{\link[BalancedSampling:sb]{sb}}).
#'
#'
#' @return a vector that contains values of the spreading measure of the samples in columns of \code{S}.
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}
#'
#'
#' @references Grafstrom, A., Lundstrom, N. L. P., and Schelin, L. (2012). Spatially balanced sampling through the pivotal method. Biometrics, 68(2):514-520.
#'
#' Jauslin, R. and Tille, Y. (2019). Spatial spread sampling using weakly associated vectors. Statistical Office, University of Neuchatel.
#'
#'
#' @seealso \code{\link[WaveSampling:IB]{IB}}, \code{\link[BalancedSampling:sb]{sb}}.
#'
#'
#' @examples
#' set.seed(1)
#' ## Coordinates in two dimensions of 10 units ##
#' coord <- matrix(stats::runif(10*2), ncol=2)
#' ## Temporal inclusion probabilities with 3 waves and 4 units ##
#' pik <- matrix(rep(0.2,10*3), ncol = 3, byrow = TRUE)
#' ## Spot method to obtain temporal samples ##
#' S <- Spot(pik, coord)
#' ## Compute IB criteria ##
#' Spread(S, pik, coord, criteria = 'IB')
#'
#' @export
Spread <- function(S, pik, coord, criteria)
{
  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------
  EPS <- 1e-8
  if((criteria == 'IB') & (sum(pik > 1-EPS) > 0)){ stop('Inclusion probabilities must be smaller than 1.')}
  N       <- nrow(pik)
  t       <- ncol(pik)
  measure <- rep(0,t)


  ##----------------------------------------------------------------
  ##                      Compute the criteria                     -
  ##----------------------------------------------------------------
  for(i in 1:t){
    if(criteria == 'IB'){
      m.strat    <- WaveSampling::wpik(X = coord, pik = pik[,i])
      m.strat2   <- m.strat-diag(diag(as.matrix(m.strat)), nrow = nrow(m.strat), ncol = ncol(m.strat))
      measure[i] <- WaveSampling::IB(W = m.strat2, s = S[,i])
    }else{
      if(criteria == 'sb'){
        measure[i] <- BalancedSampling::sb(pik[,i], coord, (1:N)[S[,i]==1])
      }
    }
  }

  return(measure)
}
