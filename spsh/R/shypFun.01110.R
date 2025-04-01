#' @title van Genuchten-Mualem Soil Hydraulic Proptery Model
#' 
#' @description van Genuchten-Mualem functions for the retention and hydraulic conductivity curves \insertCite{vanGenuchten.1980}{spsh}.
#' @param p vector of the 6 van Genuchten-Mualem model parameters, the order is sensitve and has to be given as:
#' \tabular{lll}{
#'       \code{thr}\tab{residual water water content [cm cm-3]}\cr
#'       \code{ths}\tab{saturated water water content [cm cm-3]}\cr
#'       \code{alf1}\tab{van Genuchten alpha [cm-3]}\cr
#'       \code{n1}\tab{van Genuchten n [-]}\cr
#'       \code{Ks}\tab{saturated conductivity [cm d-1]}\cr
#'       \code{tau}\tab{exponent of \code{Se} in the capillary conductivity model, sometimes denoted in the literature as \code{l} [-]}
#' }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function solves analytically the spec. water capacity function and integral to the capillary bundle model.
#' It can be extended by the Brunswick model to account for non-capillary storage and conductivity \insertCite{Weber.2019}{spsh}.
#' @return
#'  returns a \code{list} with calculations at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{cap}{specific water capacity function}
#' \item{psd}{pore size distribution}
#' \item{Kh}{Hydraulic conductivity values}
#'  
#' @references
#' \insertRef{vanGenuchten.1980}{spsh}
#' \insertRef{Weber.2019}{spsh}\cr
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @examples
#' p      <- c(0.1, 0.4, 0.01, 2, 100, .5)
#' h      <- 10^seq(-2, 6.8, length = 197)
#' shyp.L <- shypFun.01110(p, h)
#' @export shypFun.01110
#' 

shypFun.01110 <- function(p, h){
      
      # RETC: constrained unimodal van Genuchten (1980): constrained refers to substituting the shape parameter 'm' with '1-1/n'
      # COND: Mualems Capillary Bundle Model (1976)
      # Number of Parameters: 6
          h <- abs(h)
          thr <- p[1]
          ths <- p[2]
          alf1 <- p[3]
          n1 <- p[4]
          Ks <- p[5]
          tau <- p[6]
          
          m1 <- 1-1/n1
          Se <- (1+(alf1*h)^n1)^-m1        
          theta <- thr + (ths-thr) * Se  
          
          cap = -(ths-thr) * (n1*m1*alf1*(alf1*h)^(n1-1)*(1+(alf1*h)^(n1))^(-m1-1))
          
          poredis = log(10) * h * -cap
          Kh <-  Ks*Se^tau*(1-(1-Se^(1/m1))^m1)^2 
          
          return(list("theta" = theta, "Se" = Se, "cap" = cap, "psd" = poredis, "Kh" = Kh))
}

