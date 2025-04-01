#' Unimodal Brooks-Corey Non-Capillary Saturation Model 
#' @description Analytical implementation of the non-capillary saturation function from the Brooks-Corey function \insertCite{Brooks.1964}{spsh}.
#' @param p_snc vector of the 2 Brooks-Corey model parameters, order is sensitve and has to be given as:
#'    \tabular{lll}{
#'          \code{alf}\tab{air entry pressure head [cm^-1]}\cr
#'          \code{bet}\tab{effective model parameter [ - ]}\cr
#'          \code{h0}\tab{pressure head representing oven dryness given in pF, i.e. log[10](|pressure head| [cm])}\cr
#'             }
#' @param h pressure heads [cm] for which the corresponding retention and conductivity values are calculated.
#' @details The function is Eq. Table 1-A  in insertRef{Streck.2020}{spsh}
#' The analytical solution presented in \code{sncFun.04110} only requires the Brooks-Corey model specific parameters and \code{h0}.
#' @return
#' returns a \code{list} with calculations at specified \code{h}:
#' \item{snc}{non-capillary saturation}
#' @references 
#' \insertRef{Brooks.1964}{spsh}
#' \insertRef{Streck.2020}{spsh}
#' @examples
#' p     <- c(0.1, 0.4, .02, 2, 100, .5)
#' p_snc <- c(p[3:4], 6.8)
#' h     <- 10^seq(-2, 6.8, length = 197)
#' Se    <- shypFun.04110(p, h)$Se
#' snc   <- sncFun.04110(p_snc, h)
#' @export
#' @importFrom utils tail
#' 
sncFun.04110 <- function(p_snc, h){
 # analytical expression of the non-capillary saturation function
 # Equation numbers as in Streck and Weber (2020), VZJ
        
        h   <- abs(h)
        alf <- p_snc[1];    # cm^-1
        bet <- p_snc[2];
        h0  <- tail(p_snc, 1);
        
        ah <- alf*h
        ah0 <- alf*10^h0
        
        # gc  <- ifelse(ah >= 1, ah^(-1/bet), 1)

        gnc  <- ifelse(ah  >= 1, (bet+log(1/alf))/log(10) - bet/log(10)*ah^(-1/bet) - log10(h) , 0)
        gnc0 <- ifelse(ah0 >= 1, (bet+log(1/alf))/log(10) - bet/log(10)*ah0^(-1/bet)- h0, 0)
         
        snc <- 1 - gnc/gnc0
 
 return(list("snc" = snc))
}


