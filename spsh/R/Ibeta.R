#' Incomplete Beta Function
#' @description Calculation of the incomplete beta function used in\link[spsh]{sncFun.01110}
#' @param z Vector of \code{n} model parameters.
#' @param a Vector of \code{n} model parameters.
#' @param b Vector of \code{n} model parameters.
#' @details If \code{a=1} or \code{a=1/n} and \code{b=0}, this implementation cannot evaluate values for \code{z < 1.014}.
#' @return Returns a vector of numerical values
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @export
#'
#' @examples
#' x = seq(.5, 4.2 , length = 20)
#' alf = 0.1
#' n = 2
#' y = 1 + (alf * 10^x)^n
#' 
#' result <- Ibeta(z = y, a = 1, b = 0 )
#' 
#' @export
#' @importFrom hypergeo hypergeo
#' 
Ibeta <- function(z, a, b){
      stopifnot(all(is.numeric(c(z,a,b))))
return(Re(hypergeo(a+b, 1, a+1, z)) * 1/a * z^a * (1-z)^b)
      }
