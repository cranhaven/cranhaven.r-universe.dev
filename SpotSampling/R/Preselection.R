#' @title Selection of an initial spread set
#'
#' @description Select an initial spread set using the flight phase of the local cube method. Some inclusion probabilities are set to 0.
#' The others probabilities are also modified so as not to change the sum of the inclusion probabilities.
#'
#'
#' @param pik a matrix of temporal inclusion probabilities.
#' Columns of \code{pik} correspond to the times, and rows correspond to the units.
#' Inclusion probabilities can be totally unequal.
#' @param coord a matrix that contains spatial coordinates in columns. The number of columns can be more than two.
#' Matrix rows correspond to the units.
#' @param L a parameter to achieve good spatial balanced (see details). Default value is 1.
#' @param EPS a tolerance parameter. Default value is 1e-9.
#'
#'
#' @details \code{L} is used to achieve good spatial balance. It must be equal to or larger than one.
#'
#'
#' @return a matrix with the same size as \code{pik} that contains new temporal inclusion probabilities.
#' Some inclusion probabilities are updated to 0.
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}
#'
#' @references
#' Grafstrom, A., Lundstrom, N. L. P., and Schelin, L. (2012). Spatially balanced sampling through the pivotal method. Biometrics, 68(2):514-520.
#'
#'
#' @seealso \code{\link[sampling:fastflightcube]{fastflightcube}}
#'
#' @examples
#' ## Coordinates in two dimensions of 4 units ##
#' coord <- matrix(c(0.5,0.6,0.2,0.3,0.8,0.9,0.4,0.7), ncol=2)
#' ## Temporal inclusion probabilities with 3 waves and 4 units ##
#' pik <- matrix(c(0.6,0.3,0.3,
#'                 0.2,0.4,0.9,
#'                 0.3,0.2,0.5,
#'                 0.9,0.1,0.3), ncol = 3, byrow = TRUE)
#' ## Selection of an initial spread set ##
#' Preselection(pik, coord)
#'
#' @export

Preselection <- function(pik, coord, L = 1, EPS = 1e-9)
{
  a    <- pmin(L*rowSums(pik),1)
  aa   <- a
  TEST <- (a < 1-EPS)

  if(sum(TEST) > 2){
    pik_a   <- pik[TEST,]
    LOGIC   <- rep(TRUE, ncol(pik_a))
    for(i in 2:ncol(pik_a)){
      LOGIC[i] <- sum(abs(stats::residuals(stats::lm(pik_a[,i]~pik_a[,1:(i-1)]-1))))>EPS
    }
    res       <- pik_a[,LOGIC]

    aa[TEST]  <- BalancedSampling::lcubeflightphase(a[TEST],coord[TEST,]/100,as.matrix(res))
    pik_b     <- aa/a*pik
  }else{
    pik_b   <- pik
  }

  return(pik_b)
}
