#' @title ORFS method
#'
#' @description
#' Select temporal samples with fixed size at each wave using the cube method.
#' It provides optimal time rotation of the selected samples using the systematic sampling method.
#'
#'
#' @param pik a matrix of temporal inclusion probabilities.
#' Columns of \code{pik} correspond to the waves, and rows correspond to the units.
#' Inclusion probabilities can be totally unequal.
#' @param EPS a tolerance parameter. Default value is 1e-8.
#' @param comment a comment is written during the execution if \code{comment} is TRUE (default value).
#'
#'
#' @return a matrix that contains temporal samples.
#' This is the update of \code{pik} and contains only 0s and 1s that indicates if a unit is selected or not at each wave.
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}, Raphael Jauslin \email{raphael.jauslin@@unine.ch}
#'
#'
#' @references
#' Quenouille, M. H. (1949). Approximate Tests of Correlation in time-Series. Royal Statistical Society, Series B Vol. 11, No. 1 (1949), pp. 68-84.
#'
#'
#' @seealso \code{\link{SystematicDesign}}, \code{\link{ReducedSamplecube}}
#'
#'
#' @examples
#' ## Temporal inclusion probabilities with 3 waves and 4 units ##
#' pik <- matrix(c(0.6,0.3,0.3,
#'                 0.2,0.4,0.9,
#'                 0.3,0.2,0.5,
#'                 0.9,0.1,0.3), ncol = 3, byrow = TRUE)
#' ## ORFS method ##
#' Orfs(pik)
#'
#' @export
Orfs <- function(pik, EPS = 1e-8, comment = TRUE)
{
  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------

  N <- nrow(pik)
  t <- ncol(pik)

  ## Systematic sampling
  res    <- SystematicDesign(pik[1,])
  S      <- as.matrix(res$samples)
  P      <- res$probas
  R      <- rep(1, each = length(res$probas))
  for(i in 2:N){
    res  <- SystematicDesign(pik[i,])
    S    <- rbind(S,res$samples)
    P    <- c(P, res$probas)
    R    <- c(R, rep(i, each = length(res$probas)))
  }
  P      <- as.vector(P)

  if(comment){ cat("\n\nBeginning of the ORFS method.\n--------------------------------")}

  ##  Matrix of constraints
  RR <- unique(R)
  Z  <- matrix(rep(0,nrow(S)*length(RR)), nrow = nrow(S))

  for(i in 1:length(RR)){
    Z[R == RR[i],i] <- 1
  }
  M           <- cbind(Z,S)
  colnames(M) <- NULL


  ##---------------------------------------------------------------
  ##                          Orfs method                         -
  ##---------------------------------------------------------------

  if(comment){
    cat("\nBeginning of the cube method.\n")
    start_time <- Sys.time()
  }

  PP  <- ReducedSamplecube(X = P*M, pik = P,redux = TRUE,t = t)

  if(comment){ cat("Execution time:",Sys.time()-start_time,".\n")}


  return(S[PP > (1-EPS),])
}
