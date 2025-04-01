#' van Genuchten-Mualem bimodal Soil Hydraulic Propterty Model
#' @description bimodal van Genuchten-Mualem functions (Durner model) for the retention and hydraulic conductivity curves \insertCite{Durner.1994}{spsh}.
#' @param p vector of the 9 van Genuchten-Mualem model parameters, order is sensitve and has to be given as:
#' \tabular{lll}{
#'       \code{thr}\tab{residual water water content [cm cm-3]}\cr
#'       \code{ths}\tab{saturated water water content [cm cm-3]}\cr
#'       \code{alf1}\tab{van Genuchten alpha [cm-3]}\cr
#'       \code{n1}\tab{van Genuchten n [-]}\cr
#'       \code{w1}\tab{fraction of the first modality [-], \code{w2} is internally computed as \code{w2 = 1-w1}}\cr
#'       \code{alf2}\tab{van Genuchten alpha of the second modality[cm-3]}\cr
#'       \code{n2}\tab{van Genuchten n of the second modality [-]}\cr
#'       \code{Ks}\tab{saturated conductivity [cm d-1]}\cr
#'       \code{tau}\tab{exponent of \code{Se} in the capillary conductivity model, sometimes denoted in the literature as \code{l} [-]}
#' }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function solves analytically the spec. water capacity function and integral to the capillary bundle model.
#' @return
#'  returns a \code{list} with calculations at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{cap}{specific water capacity function}
#' \item{psd}{pore size distribution}
#' \item{Kh}{Hydraulic conductivity values}
#' 
#' @references
#' \insertRef{Durner.1994}{spsh}
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @examples
#' p <- c("thr" = 0.1, "ths" = 0.4, alf1 = 0.5, "n1" = 3,
#'        "w1" = .6, "alf2" = 0.01, "n2" = 1.6, 
#'        "Ks" = 100, "tau" = .5)
#' h <- 10^seq(-2, 6.8, length = 197)
#' shyp.L <- shypFun.01210(p, h)
#'
#' @export
#' 
shypFun.01210 <- function(p, h){
      # RETC: constrained bimodal van Genuchten (1980), Durner (1994): constrained refers to substituting the shape parameter 'm' with '1-1/n'
      # COND: Mualems Capillary Bundle Model (1976)
      # Number of Parameters: 9          
          h <- abs(h)
          thr <- p[1]
          ths <- p[2]
          # first modality
          alf1 <- p[3]
          n1 <- p[4]
          w1 <- p[5]
          # second modality
          alf2 <- p[6]
          n2 <- p[7]
          # conductivity
          Ks <- p[8]
          tau <- p[9]
          
          m1 <- 1-1/n1
          m2 <- 1-1/n2
          w2 <- 1-w1
          
          Se1 = (1+(alf1*h)^n1)^-m1   
          Se2 = (1+(alf2*h)^n2)^-m2
          
          Se = w1*Se1 + w2*Se2  
          theta = thr + (ths-thr)*Se
          
          cap1 =      w1*alf1*m1*n1*(alf1*h)^(n1-1)*(1+(alf1*h)^(n1))^(-m1-1)
          cap2 =      w2*alf2*m2*n1*(alf2*h)^(n2-1)*(1+(alf2*h)^(n2))^(-m2-1)
          cap = (ths - thr) * (cap1+cap2)
          
          poredis = log(10)*h*cap
          
          Kh <- Ks * Se^tau *
                    ((w1 * alf1 * (1-(1-Se^(1/m1))^m1) +
                                w2 * alf2 * (1-(1-Se^(1/m2))^m2))/(w1*alf1+w2*alf2))^2 
          
          return(list("theta" = theta, "Se" = Se, "cap" = cap, "psd" = poredis, "Kh" = Kh))
}

