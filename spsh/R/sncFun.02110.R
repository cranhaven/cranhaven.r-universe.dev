#' Unimodal Kosugi Non-Capillary Saturation Model 
#' @description Analytical implementation of the non-capillary saturation function for the \insertCite{Kosugi.1996}{spsh}.
#' @param p_snc vector of the 2 Kosugi saturation model parameters, and h0 sensitve and has to be given as:
#'    \tabular{lll}{
#'          \code{hm}\tab{air entry pressure head [cm]}\cr
#'          \code{sigma}\tab{width of pore size distribution [ - ]}\cr
#'          \code{h0}\tab{pressure head representing oven dryness given in pF, i.e. log[10](|pressure head| [cm])}\cr
#'             }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function is Eq. Table 1-B in insertRef{Streck.2020}{spsh}
#' The analytical solution presented in \code{sncFun.02110} only requires the Kosugi specific model parameters and \code{h0}
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{snc}{non-capillary saturation}
#' @references 
#' \insertRef{Kosugi.1996}{spsh}
#' \insertRef{Streck.2020}{spsh}
#' @examples
#' p      <- c(0.1, 0.4, 100, 2, 100, .5)
#' # add h0
#' p_snc  <- c(p[3:4], 6.8)
#' h      <- 10^seq(-2, 6.8, length = 197)
#' Se     <- shypFun.02110(p, h)$Se
#' snc    <- sncFun.02110(p_snc, h)
#' @export
#' @importFrom utils tail
#' @importFrom pracma erfc
#' 
sncFun.02110 <- function(p_snc, h){
      # analytical expression of the non-capillary saturation function
      # Equation numbers as in Streck and Weber (2020), VZJ
      h   <- abs(h)
      hm  <- p_snc[1];    # cm^-1
      sig <- p_snc[2];
      x0  <- tail(p_snc,1);

      xm <- log10(hm)
      x  <- c(log10(h), x0)
      
      {
      term1 <- (x-xm)/2 
      term2 <- pracma::erfc((log(10)*(x-xm))/(sqrt(2)*sig))
      term3 <- (sig/(log(10)*sqrt(2*pi))) 
      term4 <- exp(-(log(10)^2*(x-xm)^2)/(2*sig^2))
      term5 <- (x-xm)

      gnc  <-  term1 * term2 - term3 * term4 - term5 
      gnc0 <-  tail(gnc, n = 1)
      }
      gnc <- tail(gnc, -1)
      snc <- 1 - gnc/gnc0 
      
      return(list("snc" = snc))
}
