#' Generates an Initial Population of Transformed Soil Hydraulic Property Model Parameters
#' @description Draws a Latin Hypercube Sample from a set of uniform distributions in the transformed parameter space,in creating a Latin Hypercube Design. This function uses the Columnwise Pairwise (CP) algorithm to generate an optimal design with respect to the S optimality criterion, as implemented in \link[=lhs]{lhs}-package.
#' @param p vector of model parameters 
#' @param psel \code{vector} of selectors
#' @param plo   \code{vector} of lower boundary values of non-transformed parameters
#' @param pup \code{vector} of upper boundary values of non-transformed parameters
#' @param trans.L \code{list} of transformation/backtransformation operators with same length as \code{p}, \code{psel}, \code{plo}, and \code{pup}.
#' @param Npop \code{integer} of initial population size
#' @details Produces and optimum latin hypercube sample from a bounded uniform distribution.
#' @return \code{n} draws of \code{k} parameters in an \code{n x k} Latin Hypercube Sample matrix with values uniformly distributed on user specified bounds.
#' @seealso \code{\link[lhs]{optimumLHS}}
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' 
#' @examples
#' 
#' # Example based on soil hydraulic property model parameters of shpmodel = "01110" parameters
#' parL <- list("p" = c("thr"= 0.05, "ths" = 0.45, "alf1" = 0.01, "n" = 2, "Ks" = 100, "tau" = .5),
#'              "psel" = c(1, 1, 0, 1, 1, 1),
#'              "plo" = c(0.001 , 0.2, 0.001, 1.1, 1, -2),
#'              "pup" = c(0.3, 0.95, 1, 10, 1e4, 10)
#' )
#' # rules for the parameter transformation
#' ptransfit<- c(function(x)x, function(x)x,log10,
#'               function(x)log10(x-1),log10 , function(x)x)
#' # get latin hypercube sample. 
#' test.inipop <- inipopFun(parL$p, parL$psel,
#'                          parL$plo, parL$pup, ptransfit, Npop = 20)
#' # plot the latin hypercube 
#' pairs(test.inipop)
#' 
#' 
#' @importFrom lhs optimumLHS
#' @export
#' 
inipopFun <- function(p, psel, plo, pup, trans.L, Npop = NA) {
      # Function to generate an inital population on a latin hypercube
      
          # transform
          plotrans <- transFun(plo, trans.L)
          puptrans <- transFun(pup, trans.L)   
          
          # prepare "upscaling" from uniform distribution [0,1] to transformed parameter bounds [lower,upper]
          ran   <- puptrans - plotrans
          
          # Set population for lhs
          Npop  <- ifelse(is.na(Npop), sum(psel)*10, ceiling(Npop))
          
          # create population
          inipop0 <- lhs::optimumLHS(n = Npop, k = length(p), maxSweeps = 20, eps = .1, verbose = FALSE)
          
          # upscale to parameters | This works, since plo was set to the fixed parameter in p
          inipop  <- t(apply(inipop0 , 1, function(x) x*ran + plotrans))
          
          # return
          return(inipop)
}
