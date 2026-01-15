#' Evaluation of zonal polynomials
#'
#' Evaluates a zonal polynomial.
#'
#' @param x numeric or complex vector or \link[gmp]{bigq} vector
#' @param lambda an integer partition, given as a vector of decreasing
#' integers
#' @param algorithm the algorithm used, either \code{"DK"} (Demmel-Koev)
#' or \code{"naive"}
#'
#' @return A numeric or complex scalar or a \code{bigq} rational number.
#' @export
#'
#' @seealso \code{\link{ZonalPolR}}
#'
#' @references \itemize{
#' \item Robb Muirhead. \emph{Aspects of multivariate statistical theory}.
#' Wiley series in probability and mathematical statistics.
#' Probability and mathematical statistics.
#' John Wiley & Sons, New York, 1982.
#' \item Akimichi Takemura. \emph{Zonal Polynomials},
#' volume 4 of Institute of Mathematical Statistics Lecture Notes –
#' Monograph Series.
#' Institute of Mathematical Statistics, Hayward, CA, 1984.
#' \item Lin Jiu & Christoph Koutschan.
#' \emph{Calculation and Properties of Zonal Polynomials}.
#' \url{http://koutschan.de/data/zonal/}
#' }
#'
#' @examples lambda <- c(2,2)
#' ZonalR(c(1,1), lambda)
#' ZonalR(c(gmp::as.bigq(1),gmp::as.bigq(1)), lambda)
#' ##
#' x <- c(3,1)
#' ZonalR(x, c(1,1)) + ZonalR(x, 2) # sum(x)^2
#' ZonalR(x, 3) + ZonalR(x, c(2,1)) + ZonalR(x, c(1,1,1)) # sum(x)^3
ZonalR <- function(x, lambda, algorithm = "DK"){
  algorithm <- match.arg(algorithm, c("DK", "naive"))
  stopifnot(
    is.vector(x) || is.bigq(x),
    is.numeric(x) || is.complex(x) || is.bigq(x)
  )
  stopifnot(isPartition(lambda))
  lambda <- lambda[lambda != 0]
  if(algorithm == "DK"){
    ZonalEval(x, lambda)
  }else{
    ZonalEvalNaive(x, lambda)
  }
}
