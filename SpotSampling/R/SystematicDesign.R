#' @title Systematic sampling design
#'
#' @description
#' Find the systematic sampling design of an inclusion probabilities vector.
#' It gives all possible samples and their selection probabilities using the systematic sampling.
#'
#'
#' @param pik a vector of inclusion probabilities.
#' @param EPS a tolerance parameter. Default value is 1e-6.
#'
#'
#' @return Returns a list including:
#' @return \code{samples} a matrix that contains the systematic samples in rows.
#' The samples have the same length as vector \code{pik} and contains 0s and 1s that specify whether a unit is selected or not in the sample.
#' @return \code{probas} a vector that contains the selection probabilities of samples in \code{samples}.
#'
#'
#' @references
#' Quenouille, M. H. (1949). Approximate Tests of Correlation in time-Series. Royal Statistical Society, Series B Vol. 11, No. 1 (1949), pp. 68-84.
#'
#' Tille, Y. (2020). Sampling and Estimation from Finite Populations. John Wiley & Sons, 91(4), page 89.
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}
#'
#'
#' @examples
#' ## Vector of inclusion probabilities ##
#' pik   <- c(0.2,0.3,0.6)
#' ## Find the systematic sampling design of pik ##
#' SystematicDesign(pik)
#'
#'
#' @export
SystematicDesign <- function(pik, EPS=1e-6)
{
  ## index of 0 and 1
  zeros <- which(pik < EPS)
  ones  <- which(pik > (1-EPS))

  ## to sum to an integer
  pik1 <- c(pik,ceiling(sum(pik))-sum(pik))
  N    <- length(pik1)

  ## Vk and r
  Vk   <- cumsum(pik1)
  Vk1  <- Vk-floor(Vk)
  Vk1[Vk1>(1-EPS)] <- 0
  r    <- c(sort(Vk1), 1)

  ## interval centers and p
  cent <- (r[1:N] + r[2:(N + 1)])/2
  p    <- r[2:(N + 1)] - r[1:N]

  ## add 0 to Vk
  A  <- t(matrix(c(0, Vk), nrow = N + 1, ncol = N)) - matrix(cent, nrow = N, ncol = N + 1)
  A  <-  A-floor(A)
  M  <- matrix(as.integer(A[ ,1:N] > A[ ,2:(N + 1)]), N, N)
  M1 <- M[,1:(N-1)]

  TEST <- p>EPS
  if(any(!TEST)){
    M1   <- M1[TEST,]
    p    <- p[TEST]
  }
  if(length(p) == 1){ M1 <- t(as.matrix(M1)) }

  if(any(pik < EPS)){ M1[,which(pik < EPS)] <- 0 }
  if(any(pik > 1-EPS)){ M1[,which(pik > 1-EPS)] <- 1 }

  return(list(samples = M1, probas = p))
}

