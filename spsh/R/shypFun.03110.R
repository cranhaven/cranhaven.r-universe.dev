#' Unimodal Fredlund-Xing - Mualem Model
#' @description Calculates the soil hydraulic property function values based on given pressure heads. The function calculates the base function of Fredlund and Xing \insertCite{Fredlund.1994}{spsh}.
#' @param p vector of the 6 van Genuchten-Mualem model parameters, order is sensitve and has to be given as:
#' \tabular{lll}{
#'       \code{thr}\tab{residual water water content [cm cm-3]}\cr
#'       \code{ths}\tab{saturated water water content [cm cm-3]}\cr
#'       \code{alf1}\tab{inverse of the air entry pressure head [cm]}\cr
#'       \code{n1}\tab{width of pore size distribution [ - ]}\cr
#'       \code{Ks}\tab{saturated conductivity [cm d-1]}\cr
#'       \code{tau}\tab{exponent of \code{Se} in the capillary conductivity model, sometimes denoted in the literature as \code{l} [ - ]}\cr
#' }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function numerically solves the spec. water capacity function and integral to Mualem's conductivity model.
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{cap}{specific water capacity function}
#' \item{psd}{pore size distribution}
#' \item{Kh}{Hydraulic conductivity values}
#' @references
#' \insertRef{Fredlund.1994}{spsh}
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @examples
#' p      <- c(0.1, 0.4, 0.01, 2, 100, .5)
#' h      <- 10^seq(-2, 6.8, length = 197)
#' shyp.L <- shypFun.03110(p, h)
#' @export

shypFun.03110 <- function(p, h){
      # RETC: Fredlund D.G., and A. Xing. 1994. Equations for the soil-water characteristic curve. Can. Geotech. J. 31:521-532.
      # COND: Mualems Capillary Bundle Model (1976)
      # Number of Parameters: 6
      
          h <- abs(h)
          thr <- p[1];
          ths <- p[2];
          alf1 <- p[3];           # cm^-1
          n1 <- p[4];
          Ks <- p[5]
          tau <- p[6]
          
          m1 <- 1-1/n1       # constrained case

          Se <- (log(exp(1)+(alf1*h)^n1))^-m1
          theta <- thr + (ths-thr) * Se  
          
          cap_num = diff(theta)/diff(h)
          cap <- c(cap_num[1],cap_num)
          
          poredis <- log(10) * h * -cap

          Kh <- Ks * Se^tau * numMualem(h, scap = Se)
          
          return(list("theta" = theta, "Se" = Se, "cap" = cap, "psd" = poredis,
                      "Kh" = Kh))
}
