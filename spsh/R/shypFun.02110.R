#' Unimodal Kosugi-Mualem Model (2 Parameter Model)
#' @description Calculates the soil hydraulic property function values based on given pressure heads \insertCite{Kosugi.1996}{spsh}.
#' @param p vector of the 6 Kosugi-Mualem model parameters, order is sensitve and has to be given as:
#'    \tabular{lll}{
#'          \code{thr}\tab{residual water water content [cm cm-3]}\cr
#'          \code{ths}\tab{saturated water water content [cm cm-3]}\cr
#'          \code{hm}\tab{air entry pressure head [cm]}\cr
#'          \code{sigma}\tab{width of pore size distribution [ - ]}\cr
#'          \code{Ks}\tab{saturated conductivity [cm d-1]}\cr
#'          \code{tau}\tab{exponent of \code{Se} in the capillary conductivity model, sometimes denoted in the literature as \code{l} [-]}\cr
#'             }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function solves analytically the spec. water capacity function and integral to the capillary bundle model.
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{cap}{specific water capacity function}
#' \item{psd}{pore size distribution}
#' \item{Kh}{Hydraulic conductivity values}
#' @references 
#' \insertRef{Kosugi.1996}{spsh}
#' @examples
#' p      <- c(0.1, 0.4, 100, 2, 100, .5)
#' h      <- 10^seq(-2, 6.8, length = 197)
#' shyp.L <- shypFun.02110(p, h)
#' @export
#' @importFrom pracma erfc
#' 
shypFun.02110 <- function(p, h){
      # RETC: 2 parametric Kosugi function (Kosugi, 1996)
      # COND: Mualems Capillary Bundle Model (1976)
      # Number of Parameters: 6     
          h <- abs(h)
          thr <- p[1];
          ths <- p[2];
          hm <- p[3];           # cm^-1
          sigma <- p[4];
          Ks <- p[5]
          tau <- p[6]

          Se <- 0.5 * erfc(log(h/hm)/(sqrt(2)*sigma))
          
          theta <- thr + (ths-thr) * Se  
          
          cap <- (ths-thr)/(sqrt(2*pi)*sigma*h)*exp(log(h/hm)^2/(2*sigma^2))
          
          poredis <- log(10) * h * -cap
          Kh <-   Ks * Se^tau * (0.5 * erfc(log(h/hm)/(sqrt(2)*sigma) + sigma/sqrt(2) )  )^2
          
          return(list("theta" = theta, "Se" = Se, "cap" = cap, "psd" = poredis, "Kh" = Kh))
}
