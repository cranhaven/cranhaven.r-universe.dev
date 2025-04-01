#' Function to Numerically Compute the Mualem Integral
#' @description This function will calculate numerically Mualems Integral \insertCite{Mualem.1976}{spsh} and return Hydraulic Conductivity Values.
#' @param h vector of length \code{l} pressure head values.
#' @param scap Capillary saturation [cm3 cm-3].
#' @param pcon vector of soil hydraulic conductivity model parameters, the first argument is \code{q} and the second \code{r}.
#'
#' @details The numerical solution of Mualems integral relies on the trapezoidal rul of integration.
#' @return returns a vector of length \code{l} of calulcated conductivity values at \code{h}.
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @references \insertRef{Mualem.1976}{spsh}
#' @examples
#' h <- 10^seq(-3, 6.8, length = 501)
#' p = c(.05, .5, .01, 1.8, 100, .5)
#' shyp.L <- shypFun.01110(p, h)
#' 
#' Ks <- p[5]
#' tau <- p[6]
#' Se <- shyp.L[['Se']]
#' Khrnum <- numMualem(h, pcon = tau, scap = Se) 
#' 
#' Khnum <- Ks * Se^tau * Khrnum
#' 
#' plot(log10(h), log10(shyp.L[['Kh']]), ylim = c(-10, 2.3), 
#'      xlim = c(-1,6), ylab = "log10 Kunsat [ cm/d ]", xlab = "pF [ - ]", type = "l", lwd = 8)
#' lines(log10(h), log10(Khnum), col = "red", lwd = 2)
#' @export
#' @importFrom pracma cumtrapz
#' 
numMualem <- function(h, scap, pcon = NA) {

          #
          ####  PURPOSE
          #
          #     Function to calculate the capillary bundle model, analytically
          #

          #
          #### ARGUMENTS 
          #
          #       h       vector of length l with pressure heads [cm]
          #       pcon    vector of conductivity model parameters, second entry has to be the parameter tau
          #       scap    vector of length l with non (!) rescaled saturation values for the capillary part
          
          #
          #### RETURNS vector of length l
          #
          #       mua_num     calculated capillary conductivity model 

          # general capacity conductivity parameters (Hoffmann-Riem et al., 1999)
      if(!is.na(pcon)){
            q <- pcon[1]
            r <- pcon[2]
      }else{
            q <- 1
            r <- 2
      }


          int1 = pracma::cumtrapz(rev(scap), rev(h^-q))
          int2 = max(int1)[1];
          # mua_num = rev((int1/int2)^r)
          
          # int1 = cumsum(rev(h^-q) * c(diff(rev(scap)[1:2])/2, diff(rev(scap))))
          # int2 = int1[length(int1)]
          mua_num = rev((int1/int2)^r)
          
          return(mua_num)
}


