#' @title ORSP method
#'
#' @description
#' Select spatio-temporal samples with random size and well spread out in space at each wave.
#' The pivotal method is used to obtain spread samples. It provides optimal time rotation of the selected sample using the systematic sampling method.
#'
#'
#' @param pik a matrix of temporal inclusion probabilities.
#' Columns of \code{pik} correspond to the waves, and rows correspond to the units.
#' Inclusion probabilities can be totally unequal.
#' @param coord a matrix that contains spatial coordinates in columns. The number of columns can be more than two.
#' Matrix rows correspond to the units.
#' @param EPS a tolerance parameter. Default value is 1e-6.
#' @param comment a comment is written during the execution if \code{comment} is TRUE (default value).
#'
#'
#'
#' @return a matrix that contains spatio-temporal samples.
#' This is the update of \code{pik} and contains only 0s and 1s that indicates if a unit is selected or not at each wave.
#'
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}
#'
#'
#' @references
#' Quenouille, M. H. (1949). Approximate Tests of Correlation in time-Series. Royal Statistical Society, Series B Vol. 11, No. 1 (1949), pp. 68-84.
#'
#' Grafstrom, A., Lundstrom, N. L. P., and Schelin, L. (2012). Spatially balanced sampling through the pivotal method. Biometrics, 68(2):514-520.
#'
#'
#' @seealso \code{\link{SystematicDesign}}, \code{\link[BalancedSampling:lpm1]{lpm1}}
#'
#'
#' @examples
#' ## Coordinates in two dimensions of 4 units ##
#' coord <- matrix(c(0.5,0.6,0.2,0.3,0.8,0.9,0.4,0.7), ncol=2)
#' ## Temporal inclusion probabilities with 3 waves and 4 units ##
#' pik <- matrix(c(0.6,0.3,0.3,
#'                 0.2,0.4,0.9,
#'                 0.3,0.2,0.5,
#'                 0.9,0.1,0.3), ncol = 3, byrow = TRUE)
#' ## ORSP method ##
#' Orsp(pik, coord, EPS = 1e-6)
#'
#' @export
Orsp <- function(pik, coord, EPS = 1e-6, comment = TRUE)
{

  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------

  N <- nrow(pik)
  t <- ncol(pik)


  ##---------------------------------------------------------------
  ##                          Orsp method                         -
  ##---------------------------------------------------------------

  if(comment){ cat("\n\nBeginning of the ORSP method.\n--------------------------------")}
  for(j in 1:(t-1))
  {
    if(comment){
      cat("\n- Time number",j)
      start_time <- Sys.time()
    }
    s      <- rep(0,N)
    lpm    <- BalancedSampling::lpm1(pik[,j], coord)
    s[lpm] <- 1

    for(k in 1:N){
      choice     <- s[k]
      pik1       <- pik[k,j:t]
      A          <- SystematicDesign(pik1)
      S          <- A$samples
      p          <- A$probas
      S1         <- matrix(S[S[,1]== choice, ], ncol = ncol(S))
      p1         <- p[S[,1] == choice]/sum(p[S[,1] == choice])
      pik[k,j:t] <- c(t(S1)%*%p1)
    }
    if(comment){ cat("\nSample selection time:",Sys.time()-start_time) }
  }
  if(comment){
    cat("\n- Time number",t)
    start_time <- Sys.time()
  }
  s       <- rep(0,N)
  lpm     <- BalancedSampling::lpm1(pik[,t],coord)
  s[lpm]  <- 1
  pik[,t] <- s
  if(comment){ cat("\nSample selection time:",Sys.time()-start_time) }

  ## rounding of probabilities in pik
  pik[pik > 1-EPS] <- 1
  pik[pik < EPS]   <- 0

  return(pik)
}
