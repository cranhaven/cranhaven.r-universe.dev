#' @title SPOT method
#'
#' @description
#' Select spatio-temporal samples of fixed size and well spread out in space at each wave.
#' The pivotal method is used to obtain spread samples. It provides optimal time rotation of the selected sample using the systematic sampling method.
#'
#'
#' @param pik a matrix of temporal inclusion probabilities.
#' Columns of \code{pik} correspond to the waves, and rows correspond to the units.
#' Inclusion probabilities can be totally unequal.
#' @param coord a matrix that contains spatial coordinates in columns. The number of columns can be more than two.
#' Matrix rows correspond to the units.
#' @param EPS a tolerance parameter. Default value is 1e-8.
#' @param comment a comment is written during the execution if \code{comment} is TRUE (default value).
#'
#'
#'
#' @return a matrix that contains temporal samples.
#' This is the update of \code{pik} and contains only 0s and 1s that indicates if a unit is selected or not at each wave.
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
#' @seealso \code{\link{SystematicDesign}}, \code{\link{ReducedSamplecube}}.
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
#' ## SPOT method ##
#' Spot(pik, coord, EPS = 1e-6)
#'
#' @export
Spot <- function(pik, coord, EPS = 1e-8, comment = TRUE)
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

  ## Create and sort pairs
  coord <- as.matrix(coord)
  res   <- cbind(t(utils::combn(1:N, 2)), rep(0, N*(N-1)/2))
  for(i in 1:nrow(res)){
    res[i,3] <- stats::dist(rbind(coord[res[i,1],], coord[res[i,2],]))
  }
  res   <- res[order(res[,3]),]
  pairs <- res[,1:2]

  ##----------------------------------------------------------------
  ##                          Spot method                          -
  ##----------------------------------------------------------------

  if(comment){ cat("\n\nBeginning of the SPOT method.\n--------------------------------")}

  ## flight phase
  d0       <- rep(0, t)
  for( t1 in 1:t ){
    if(comment){
      cat("\n- Wave number",t1)
      start_time <- Sys.time()
    }
    index_pairs <- 1:nrow(pairs)
    d           <- d0
    d[t1]       <- 1
    r           <- 0
    if(t1 > 1){
      for(u in 1:N){
        if((t(P[R == u])%*%S[R==u,t1])%%1 == 0){
          with_u                   <- which((pairs[,1]==u)|(pairs[,2]==u))
          index_pairs[index_pairs %in% with_u] <- 0
        }
      }
    }
    while((sum(index_pairs)!=0) && (r<length(index_pairs))){
      r <- r+1
      if(index_pairs[r] != 0){
        u1  <- pairs[index_pairs[r],1]
        u2  <- pairs[index_pairs[r],2]
        design1 <- list(samples = S[R == u1,], probas = P[R == u1])
        design2 <- list(samples = S[R == u2,], probas = P[R == u2])

        DD       <- TemporalPivot(design1, design2, d, EPS = EPS)
        P[R==u1] <- DD$p1_new
        P[R==u2] <- DD$p2_new
        if((t(P[R == u1])%*%S[R == u1,t1])%%1 == 0){
          with_u                               <- which((pairs[,1]==u1)|(pairs[,2]==u1))
          index_pairs[index_pairs %in% with_u] <- 0
        }
        if((t(P[R == u2])%*%S[R == u2,t1])%%1 == 0){
          with_u                               <- which((pairs[,1]==u2)|(pairs[,2]==u2))
          index_pairs[index_pairs %in% with_u] <- 0
        }
      }

      if((sum(index_pairs) == 0)|(length(index_pairs)==1)|(length(index_pairs)==r)){ break }

      ## remove pairs with 0 at each (2N) steps
      if(r > 2*N){
        index_pairs <- index_pairs[(r+1):length(index_pairs)][index_pairs[(r+1):length(index_pairs)]!=0]
        r           <- 0
      }
    }
    if(comment){ cat("\nSample selection time:",Sys.time()-start_time) }
  }

  ## Landing phase
  TEST <- (P < (1-EPS))&(P > EPS)
  if(sum(TEST) >= 1){
    RR        <- R[TEST]
    unique_RR <- unique(R[TEST])
    if(comment){ cat("\n\nLanding phase required for",length(unique_RR),"units.\n")}
    PP        <- P[TEST]
    SS        <- S[TEST, ]
    Z         <- matrix(rep(0, nrow(SS)*length(unique_RR)), nrow = nrow(SS))

    for(i in 1:length(unique_RR)){
      Z[RR == unique_RR[i],i] <- 1
    }

    M           <- cbind(Z,SS)
    colnames(M) <- NULL
    PLand                  <- ReducedSamplecube(PP*M, PP, redux = TRUE, t = ncol(M))
    PLand[PLand < EPS]     <- 0
    PLand[PLand > (1-EPS)] <- 1
    P[TEST]                <- PLand
  }else{
    if(comment){ cat("\n\nNo landing phase required.\n")}
  }

  return(S[P > (1-EPS),])
}
