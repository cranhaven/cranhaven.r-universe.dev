#' Parameter Transformation and Back-transformation
#' @description Enables the transformation and backtransformation of parameters. 
#' This is widely considered advantageous during parameter estimation as the parameter space in the transformed is well-behaved, e.g. with normally distributed posteriors.
#' @param par.vec Vector of \code{n} model parameters.
#' @param trans.L list of \code{n} transformation/backtransformation operators, transformation and backtransformatio rules have to be antonyms and position in vector has to coincide with that in \code{par.vec}.
#' @details Transformation rules are:\deqn{log10 \alpha_i,log10 n_i-1,log10 Ks,log10 \omega,log10 Ksc, and log10 Ksnc}.
#' @return Returns transformed parameters as specificef by trans.L.
#' @note The function is used to transform the parameter space and enabling optimisation or MCMC sampling to be more efficient.
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @export
#'
#' @examples
#' # van Genuchten-Mualem Model parameters
#' parL <- list("p" = c("thr"= 0.05, "ths" = 0.45, "alf1" = 0.01, "n" = 2, "Ks" = 100, "tau" = .5),
#'              "psel" = c(1, 1, 0, 1, 1, 1),
#'              "plo" = c(0.001 , 0.2, 0.001, 1.1, 1, -2),
#'              "pup" = c(0.3, 0.95, 1, 10, 1e4, 10)
#' )
#' # Two lists, one with function to transform, the other to back-transform model parameters
#' ptransfit <- c(function(x)x, function(x)x,log10,function(x)log10(x-1),log10, function(x)x)
#' pretransfit <- c(function(x)x, function(x)x,function(x)10^x, 
#'                  function(x)10^x+1,function(x)10^x,function(x)x)
#' # Transform
#' p_trans <- transFun(parL$p, ptransfit)
#' 
transFun <- function(par.vec, trans.L) {
          
          #
          ####  PURPOSE
          #
          #     Function to transform and back-transform the parameters (of e.g.  soil hydraulic property functions),
          #         this enables more robust estimation by reducing the correlation structure of the parameters
          
          #
          #### ARGUMENTS 
          #
          #       par.vec     num       vector of length l of parameters to be transformed
          #       trans.L     function  list of transformation functions of same length par.vec
          
          #
          #### RETURNS vector of length l
          #
          #       p.transform num       vector of length l with transformed parameters.
          
          p.transform <- mapply(function(f, x) f(x), trans.L, par.vec)
          
          return(p.transform)
          
}