#' Unimodal van Genuchten Non-Capillary Saturation Model 
#' @description Analytical implementation of the non-capillary saturation function \insertCite{vanGenuchten.1980}{spsh}.
#' @param p_snc vector of the 2 van Genuchten Mualem model and h0, the order is sensitve and has to be given as:
#'    \tabular{lll}{
#'       \code{alf1}\tab{van Genuchten alpha [cm-3]}\cr
#'       \code{n1}\tab{van Genuchten n [-]}\cr
#'       \code{h0}\tab{pressure head representing oven dryness given in pF, i.e. log[10](|pressure head| [cm])}\cr
#'             }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function is Eq. Table 1-C1 in insertRef{Streck.2020}{spsh} using eq 21 which can be used
#' to accelerate the convergence of the sum.
#' The analytical solution presented in \code{sncFun.01110} only requires the Brooks-Corey model parameters
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{snc}{non-capillary saturation}
#' @references 
#' \insertRef{vanGenuchten.1980}{spsh}
#' \insertRef{Weber.2019}{spsh}
#' \insertRef{Streck.2020}{spsh}
#' @examples
#' p      <- c(0.1, 0.4, .01, 2, 100, .5)
#' # add h0
#' p_snc  <- c(p[3:4], 6.8)
#' h      <- 10^seq(-2, 6.8, length = 197)
#' Se     <- shypFun.01110(p, h)$Se
#' snc    <- sncFun.01110(p_snc, h)
#' @export
#' @importFrom utils tail
#' 
sncFun.01110 <- function(p_snc, h){
      # analytical expression of the non-capillary saturation function
      # Equation numbers as in Streck and Weber (2020), VZJ
      h  <- abs(h)
      x  <- log10(h)
      
      alf<- p_snc[1]
      n1 <- p_snc[2]
      x0 <- tail(p_snc,1)
      
      k_end  <- 100
      ycrit  <- 1 + 0.014 # empirical value
       
      ah     <- alf*10^x
      ah0    <- alf*10^x0
 
      y      <- 1 + ah^n1
      y0     <- 1 + ah0^n1

      ssel   <- ifelse(y >= ycrit, TRUE, FALSE)
      yshort <- y[ssel]
      
      Bterm1 <- Ibeta(yshort, a = 1, b = 0)
      Bterm2 <- Ibeta(yshort, a = 1/n1, b = 0)
      
      Bterm1_h0 <- Ibeta(y0, a = 1, b = 0)
      Bterm2_h0 <- Ibeta(y0, a = 1/n1, b = 0)
      
      k     <- 1:k_end
      kint  <-  ((k * (k + 1)) * (n1 * k + 1))^-1
      SUM_k <- (n1+1) - sum(kint)
      
      # Solution C1 for all y
      gnc   <- 1/(n1*log(10))*((Bterm1 - Bterm2) + (1-1/n1) * SUM_k)
      # Solution C1 for y0
      gnc0  <- 1/(n1*log(10))*((Bterm1_h0 - Bterm2_h0) + (1-1/n1) * SUM_k)
      
      snc_short<- 1 - gnc/gnc0
      snc      <- c(rep(1, sum(!ssel)), snc_short)

      return(list("snc" = snc))
}
