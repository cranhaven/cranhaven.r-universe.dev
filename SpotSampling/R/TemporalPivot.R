#' @title Adaptation of the local pivotal method on temporal samples
#'
#' @description
#' This function considers longitudinal systematic sampling designs of two different units that result from function \code{\link{SystematicDesign}}.
#' It allows to decide if one of these 2 units is selected at a specific time by putting at least one of the samples selection probabilities to 0.
#' It is based on the local pivotal method.
#'
#'
#' @param design1 a longitudinal systematic sampling design of a first unit. The length of the longitudinal samples is \eqn{T}.
#' It results from function \code{\link[SpotSampling:SystematicDesign]{SystematicDesign}}.
#' @param design2 a longitudinal systematic sampling design of a second unit. The length of the longitudinal samples is \eqn{T}.
#' It results from function \code{\link[SpotSampling:SystematicDesign]{SystematicDesign}}.
#' @param d a vector of size T that specify for which time \eqn{t} a decision must be taken, with \eqn{1 \le t \le T}.
#' \code{d} is such that the \eqn{t}-th element is equal to 1, and the others to 0.
#' @param EPS a tolerance parameter. Default value is 1e-6.
#'
#'
#'
#' @return Returns a list including:
#' @return \code{p1_new} the updated probabilities of the longitudinal systematic sampling design of the first unit.
#' @return \code{p2_new} the updated probabilities of the longitudinal systematic sampling design of the second unit.
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}
#'
#'
#' @references
#' Quenouille, M. H. (1949). Approximate Tests of Correlation in time-Series. Royal Statistical Society, Series B Vol. 11, No. 1 (1949), pp. 68-84.
#'
#' Tille, Y. (2020). Sampling and Estimation from Finite Populations. John Wiley & Sons, 91(4), page 89.
#'
#' Grafstrom, A., Lundstrom, N. L. P., and Schelin, L. (2012). Spatially balanced sampling through the pivotal method. Biometrics, 68(2):514-520.
#'
#'
#' @seealso \code{\link{SystematicDesign}}
#'
#'
#' @examples
#' ## Vectors of temporal inclusion probabilities with 3 waves ##
#' pik1   <- c(0.2,0.3,0.5) # of a first unit
#' pik2   <- c(0.1,0.4,0.5) # of a second unit
#' ## Find the systematic sampling designs of pik1 and pik2 ##
#' design1 <- SystematicDesign(pik1, EPS = 1e-6)
#' design2 <- SystematicDesign(pik2, EPS = 1e-6)
#' ## The time we want to take a decision ##
#' t    <- 2
#' d    <- rep(0,3)
#' d[t] <- 1
#' ## Update probabilities to take a decision at wave t=2 ##
#' TemporalPivot(design1, design2, d)
#'
#' @export
TemporalPivot <- function(design1, design2, d, EPS = 1e-6)
{

  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------

  S1 <- design1$samples
  p1 <- design1$probas
  S2 <- design2$samples
  p2 <- design2$probas
  t <- ncol(S1)
  TT  <- T


  ##----------------------------------------------------------------
  ##                            Method                             -
  ##----------------------------------------------------------------

  while(TT){
    TEST1 <- p1>EPS
    TEST2 <- p2>EPS
    if(((sum(TEST1)) <= 1)|((sum(TEST2)) <= 1)){ break }

    SS1   <- S1[TEST1, ]
    SS2   <- S2[TEST2, ]
    pp1   <- p1[TEST1]
    pp2   <- p2[TEST2]

    sum1  <- rowSums(SS1)
    sum2  <- rowSums(SS2)

    if((length(unique(sum1))!=1)|(length(unique(sum2))!=1)){
      w             <- rep(0,nrow(SS1))
      c             <- which(sum1 != max(sum1))
      w[c]          <- max(sum1)-sum1[c]
      SS1           <- cbind(SS1, w)
      colnames(SS1) <- NULL

      w             <- rep(0,nrow(SS2))
      c             <- which(sum2 != max(sum2))
      w[c]          <- max(sum2)-sum2[c]
      SS2           <- cbind(SS2, w)
      colnames(SS2) <- NULL

      dd            <- c(d,0)
    }else{ dd <- d }

    M   <- rbind(cbind(t(SS1), -t(SS2)), c(rep(1,nrow(SS1)),rep(0,nrow(SS2))), c(rep(0,nrow(SS1)),rep(1,nrow(SS2))))
    ker <- pracma::nullspace(M)

    if(is.null(ker)){ break }
    if(all( ker^2 < EPS )){ break }

    u   <- t(SS1)%*%ker[1:nrow(SS1),]
    uProj <- c(pracma::linearproj(u, matrix(dd))$Q)
    if(all( uProj^2 < EPS )){ break }

    b1   <- c(solve(SS1%*%t(SS1))%*%SS1%*%uProj)
    l1.1 <- min(-pp1[b1<(-EPS)]/b1[b1<(-EPS)])
    l1.2 <- min(pp1[b1>EPS]/b1[b1>EPS])

    b2   <- c(solve(SS2%*%t(SS2))%*%SS2%*%uProj)
    l2.1 <- min(-pp2[b2<(-EPS)]/b2[b2<(-EPS)])
    l2.2 <- min(pp2[b2>EPS]/b2[b2>EPS])

    l1   <- min(l1.1,l2.2)
    l2   <- min(l2.1,l1.2)

    q    <- l2/(l1+l2)

    if(stats::runif(1) < q) {
      p1[TEST1] <- pp1+l1*b1
      p2[TEST2] <- pp2-l1*b2
    }else{
      p1[TEST1] <- pp1-l2*b1
      p2[TEST2] <- pp2+l2*b2
    }

    if((max(p1)>(1-EPS*100))|(max(p2)>(1-EPS*100))){
      TT <- F
    }
  }

  return(list(p1_new = p1, p2_new = p2))
}
