#' Non-capillary Saturation Function to Extend Other Functions
#' @description The general purpose method to calculate numerically the effective non-capillary saturation is directly obtained from any arbritrary expression for the 
#' rescaled capillary saturation function as described by \insertCite{Weber.2019}{spsh}. Examples of capillary saturation functions are the well known \insertCite{vanGenuchten.1980}{spsh}, \insertCite{Kosugi.1996}{spsh}, \insertCite{Fredlund.1994}{spsh} functions.
#' @param h A vector of \code{n} pressure head values for which \code{scap} was calculated
#' @param scap vector of \code{n} monotonically decreasing capillary saturation function values calculated by \code{shypFun}, rescaled between 0 and 1.
#' @return A vector of \code{n} elements with calculated saturation content of the non-capillary part.
#' @references 
#' \insertRef{vanGenuchten.1980}{spsh}\cr
#' \insertRef{Kosugi.1996}{spsh}\cr
#' \insertRef{Fredlund.1994}{spsh}\cr
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @export
#'
#' @examples
#' # set variables
#' p <- c(0.1, 0.4, 0.01, 2, 100, .5)
#' h <- 10^seq(-2, 6.8, length = 197)
#' 
#' # Calculate the capillary and non-capillary saturation function.
#' Se <- shypFun(p, h, shpmodel = "01110")$Se     
#' Snc <- sncFun(Se)
#' 
sncFun <- function(h, scap){
          
          #
          ####  PURPOSE
          #
          #     Function to calculate the non-capillary saturation
          #
          
          #
          #### ARGUMENTS 
          #
          #       scap    vector of length l with non (!) rescaled saturation values for the capillary part
          
          #
          #### RETURNS vector of length l
          #
          #       snc     calculated non-capillary saturation values
          
          
          nh <- length(h)
          
          # solution to the integral
          # Snc_star_int =  pracma::cumtrapz(h,(1-scap)/h);
          # snc = 1-(Snc_star_int/Snc_star_int[nh]);
          # 
          result = tryCatch({
                Snc_star_int =  drop(pracma::cumtrapz(h,(1-scap)/h));
                1-(Snc_star_int/Snc_star_int[nh]);
          }, warning = function(w) {
                rep(1, nh)
          }, error = function(e) {
                rep(1, nh)
          }
          )
          # is.na(result) { result <- }
          return(result)
}
