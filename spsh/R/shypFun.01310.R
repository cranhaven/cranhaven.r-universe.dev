#' van Genuchten-Mualem trimodal Soil Hydraulic Propterty Model
#' @description trimodal van Genuchten-Mualem functions for the retention and hydraulic conductivity curves \insertCite{Durner.1994}{spsh}.
#' @param p vector of the 9 van Genuchten-Mualem model parameters, order is sensitve and has to be given as:
#' \tabular{lll}{
#'       \code{thr}\tab{residual water water content [cm cm-3]}\cr
#'       \code{ths}\tab{saturated water water content [cm cm-3]}\cr
#'       \code{alf1}\tab{van Genuchten alpha [cm-3]}\cr
#'       \code{n1}\tab{van Genuchten n [-]}\cr
#'       \code{w1}\tab{fraction of the first modality [-], \code{w2} is internally computed as \code{w2 = 1-w1}}\cr
#'       \code{alf2}\tab{van Genuchten alpha of the second modality[cm-3]}\cr
#'       \code{n2}\tab{van Genuchten n of the second modality [-]}\cr
#'       \code{w2}\tab{fraction of the second modality [-], \code{w3} is internally computed as \code{w3 = 1-w1-w2}, in \code{resFun} ensures \code{w3 >=0} }\cr
#'       \code{alf3}\tab{van Genuchten alpha of the third modality[cm-3]}\cr
#'       \code{n3}\tab{van Genuchten n of the third modality [-]}\cr
#'       \code{Ks}\tab{saturated conductivity [cm d-1]}\cr
#'       \code{tau}\tab{exponent of \code{Se} in the capillary conductivity model, sometimes denoted in the literature as \code{l} [-]}
#' }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function solves analytically the spec. water capacity function and integral to the capillary bundle model.
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{theta}{calculated volumetric moisture content}
#' \item{Se}{calculated saturation}
#' \item{cap}{specific water capacity function}
#' \item{psd}{pore size distribution}
#' \item{Kh}{Hydraulic conductivity values}
#' 
#' 
#' @references
#' \insertRef{Durner.1994}{spsh}\cr
#' @seealso
#' \insertRef{Weber.2019}{spsh}\cr
#' \insertRef{Weber.2017a}{spsh}\cr
#' \insertRef{Weber.2017b}{spsh}\cr
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @examples
#' p <- c("thr" = 0.1, "ths" = 0.4, alf1 = .5, "n1" = 3,
#'        "w1" = .5, "alf2" = 0.01, "n2" = 2, 
#'        "w2" = .3, "alf3" = 0.01, "n3" = 1.6, 
#'        "Ks" = 100, "tau" = .5)
#' h <- 10^seq(-2, 6.8, length = 197)
#' shyp.L <- shypFun.01310(p, h)
#'
#' @export
#' 

shypFun.01310 <- function(p, h){
      # RETC: constrained trimodal van Genuchten (1980), (Durner 1994) (c.f. Priesack and Durner for analytical solution): constrained refers to substituting the shape parameter 'm' with '1-1/n'
      # COND: Mualems Capillary Bundle Model (1976)
      # Number of Parameters: 12        
          h <- abs(h)
          thr <- p[1]
          ths <- p[2]
          
          alf1 <- p[3]
          n1 <- p[4]
          w1 <- p[5]
          
          alf2 <- p[6]
          n2 <- p[7]
          w2 <- p[8]
          
          alf3 <- p[9]
          n3 <- p[10]
          
          Ks <- p[11]
          tau <- p[12]
          q <- 1
          r <- 2

          # computed parameters
          ah1 <- alf1*h
          ah2 <- alf2*h
          ah3 <- alf3*h
          
          m1 <- 1-1/n1
          m2 <- 1-1/n2
          m3 <- 1-1/n3
          
          w3 <- 1-w1-w2
          
          # capillary storage
          u1 = (1+ah1^n1)^-m1   
          u2 = (1+ah2^n2)^-m2
          u3 = (1+ah3^n3)^-m3
          scap = w1*u1 + w2*u2  + w3*u3
          # water content
          theta = thr + (ths-thr)*scap
          
          # capacity function
          cap1 =      w1*alf1*m1*n1*ah1^(n1-1)*(1+ah1^n1)^(-m1-1)
          cap2 =      w2*alf2*m2*n2*ah2^(n2-1)*(1+ah2^n2)^(-m2-1)
          cap3 =      w3*alf3*m3*n3*ah3^(n3-1)*(1+ah3^n3)^(-m3-1)
          cap = (ths - thr) * (cap1+cap2+cap3)
          
          # pore distribution
          poredis = log(10)*h*cap
          
          # conductivity
          fs <- w1 * alf1 * (1-u1^(1/m1))^m1 + 
                    w2 * alf2 * (1-u2^(1/m2))^m2 + 
                    w3 * alf3 * (1-u3^(1/m3))^m3
          
          f0 <- w1*alf1 + w2*alf2 + w3*alf3
          
          mua <- 1-fs^-q/f0^-q
          Kh <- Ks * scap^tau * mua^r

          return(list("theta"=theta, "Se" = scap, "cap" = cap, "psd" = poredis, "Kh" = Kh))
}