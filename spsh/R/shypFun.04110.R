#' Unimodal Brooks-Corey Model 
#' @description Calculates the soil hydraulic property function values based on given pressure heads \insertCite{Kosugi.1996}{spsh}.
#' @param p vector of the 6 Brooks-Corey model parameters, order is sensitve and has to be given as:
#'    \tabular{lll}{
#'          \code{thr}\tab{residual water water content [cm cm-3]}\cr
#'          \code{ths}\tab{saturated water water content [cm cm-3]}\cr
#'          \code{alf}\tab{air entry pressure head [cm^-1]}\cr
#'          \code{bet}\tab{effective model parameter [ - ]}\cr
#'          \code{Ks}\tab{saturated conductivity [cm d-1]}\cr
#'          \code{tau}\tab{exponent of \code{Se} in the capillary conductivity model, sometimes denoted in the literature as \code{l} [-]}\cr
#'             }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function solves analytically the spec. water capacity function and integral to the capillary bundle model.
#' @note The Muealm integral is solved numerically
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{cap}{returns NA; not supported}
#' \item{psd}{returns NA; not supported}
#' \item{Kh}{Hydraulic conductivity values}
#' @references 
#' \insertRef{Brooks.1964}{spsh}
#' @examples
#' p      <- c(0.1, 0.4, .01, .3, 100, .5)
#' h      <- 10^seq(-2, 6.8, length = 197)
#' shyp.L <- shypFun.04110(p, h)
#' @export
#' @importFrom pracma erfc
#' 
shypFun.04110 <- function(p, h){
      # RETC: 6 parametric Brooks Corey function (Brooks-Corey, 1964)
      # COND: Mualems Capillary Bundle Model (1976)
      # Number of Parameters: 6     
          h   <- abs(h)
          thr <- p[1];
          ths <- p[2];
          alf <- p[3];           # cm^-1
          bet <- p[4];
          Ks  <- p[5];
          tau <- p[6];

          ah  <- alf * h
          
          Se   <-  ifelse(ah >= 1, ah^(-1/bet), 1)
          theta<- thr + (ths-thr) * Se  
          
          cap     <- NA
          poredis <- NA # log(10) * h * -cap
          Kh      <- Ks * Se^tau * numMualem(h, Se)
          
          return(list("theta" = theta, "Se" = Se, "cap" = cap, "psd" = poredis, "Kh" = Kh))
}
