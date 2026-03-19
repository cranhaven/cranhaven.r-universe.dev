# This file is part of econetwork

# econetwork is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# econetwork is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with econetwork.  If not, see <http://www.gnu.org/licenses/>


######################################################################################################################################################
#                                                                                                                                                    #
#     Alpha, beta, gamma decomposition with parametrization of the dominance (q) effect                                                              #
#                                                                                                                                                    #    
#     Refs : Chalmandrier, L., Muenkemueller, T., Lavergne, S., & Thuiller, W. (2014). Ecology, 96(1), 2015, pp. 143-153                               #
#            Chao, A., Chiu, C. H., & Jost, L. (2010). Philosophical Transactions of the Royal Society B: Biological Sciences, 365(1558), 3599-3609. #
#            Leinster, T., & Cobbold, C. A. (2012). Ecology, 93(3), 477-489.                                                                         #
#                                                                                                                                                    #
######################################################################################################################################################

# Functions
## divLeinster calculates the diversity of each site of a site by species matrix according to the q parameter according to Leinster & Cobbold 2012.
## abgDecompQ performs a alpha, beta, gamma multiplicative decomposition using Leinster's diversity indices. 

## BetaDisQ calculates the pairwise beta-diversity (minus 1) between sites of a site by species matrix according to the q parameter using the afformentionned functions
##Allows a parametrization of the dominance effect

## chaoObjects is a data preparation function. It returns adequate arguments for abgDecompQ, BetaDisQ and divLeinster to perform a diversity analysis using Chao's diversity index.
### Warning: this formula only works with an ultrametric tree!

# Arguments
## spxp : sites (row) by species (cols) matrix with or without rownames and colnames.
## Z : similarity matrix used into all functions.
## check : arguments specifying if the arguments should be checked.

divLeinster <- function(spxp, Z=NULL, q=2, check = TRUE){
  #Calcul the diversity of each site of sites by species matrix. 
  #spxp columns and Z rows and columns are assumed to be in the same order.
  if (is.null(Z)) Z <- diag(ncol(spxp))
  if (check){
    if (!inherits(spxp, "matrix")) {
      stop("object \"spxp\" is not of class \"matrix\"")}  
    if (!inherits(Z, "matrix")) {
      stop("object \"Z\" is not of class \"matrix\"")}  
    if (!all(c(ncol(Z), nrow(Z)) == ncol(spxp))){
      stop("object \"Z\" and object \"spxp\" does not have matching dimensions")}
  }  
  spxp <- sweep(spxp, 1, rowSums(spxp), "/")
  Zp <- Z %*% t(spxp)
  
  if (q != 1 & q != Inf){
    mat <- t(spxp) * (Zp)^(q-1)
    mat[is.na(mat)] <- 0
    D <- colSums(mat) ^ (1/(1-q))
  }
  if (q==Inf)  {
    D <- 1/ apply(Zp, 2, max)
  }  
  if (q == 1){
    D <- apply(Zp^t(spxp), 2, function(x) 1/prod(x))
  }
  return(D)
}

abgDecompQ <- function(spxp, Z=NULL, q=2, check=TRUE) {
  #Calcul the diversity of each site of sites by species matrix. 
  #spxp columns and Z rows/cols are assumed to be in the same order.
  if (is.null(Z)) Z <- diag(ncol(spxp))
  if (check){
    if (!inherits(spxp, "matrix")) {
      stop("object \"spxp\" is not of class \"matrix\"")}  
    if (!inherits(Z, "matrix")) {
      stop("object \"Z\" is not of class \"matrix\"")}  
    if (!all(c(ncol(Z), nrow(Z)) == ncol(spxp))){
      stop("object \"Z\" and object \"spxp\" does not have matching dimensions")}
  }
  
  site.weight <- rep(1/nrow(spxp), nrow(spxp))
  spxp <- sweep(spxp, 1, rowSums(spxp), "/")
  
  gamma.ab <- colSums(sweep(spxp, 1, site.weight, "*"))
  
  Gamma <- divLeinster(t(as.matrix(gamma.ab)), Z=Z , q=q, check = FALSE)
  Alphas <- divLeinster(spxp, Z=Z , q=q, check = FALSE) 
  
  if (q != 1 & q != Inf) {
    mAlpha <- (sum(site.weight * (Alphas ^ (1 - q))))^(1 / (1 - q))
  }
  if (q==1){
    mAlpha <- exp(sum(site.weight * log(Alphas)))
  }
  if (q==Inf){
    mAlpha <- min(Alphas)
  }
  Beta <- Gamma / mAlpha
  
  names(Alphas) <- row.names(spxp)
  res <- list(Gamma=Gamma, Beta=Beta, mAlpha=mAlpha, Alphas=Alphas)
  
  return(res)
}

