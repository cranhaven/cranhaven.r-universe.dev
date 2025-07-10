#' (Internal function) Check the different value of X and Y, internally used for
#' parallelization
#'
#' @param x The first number
#' @param y The second number
#'
#' @return The different number of \code{x} and \code{y}.

diff.xy = function(x,y){
  ret = abs(x-y)
  return(ret)
}

#' (Internal function) Calculae a vector of EigenFit values, internally used for
#' parallelization
#'
#' @param eigen.value A vector of Eigenvalues return from \code{\link{svd}}
#' ($d), \code{rARPACK::svds} ($d),  \code{\link{eigen}}
#' ($values) or \code{rARPACK::eigs} ($values).
#'
#' @return A vector of all possible values for EigenFit.

cal.eigen.fit = function(eigen.value){
  I=log(eigen.value)
  X = I
  Y = I
  Y = Y[2:length(I)]
  X = X[1:(length(I)-1)]
  ret = mapply(diff.xy,X,Y)
  return(ret)
}

#' (Internal function) Calculate a vector of different values from a vector of
#' EigenFit values, internally used for parallelization
#'
#' @param eigen.value A vector of Eigenvalues return from \code{svd} ($d),
#' \code{rARPACK::svds} ($d), \code{eigen} ($values) or \code{rARPACK::eigs} ($values).
#'
#' @return A vector of different values from a vector of EigenFit values

diff.eigen.fit = function(eigen.value){
  I=log(eigen.value)
  deviation=I-median(I)
  I=I[which(deviation>0)]

  X = I
  Y = I
  Y = Y[2:length(I)]
  X = X[1:(length(I)-1)]
  ret = log(mapply(diff.xy,X,Y))
  return(ret)
}

#' (Internal function) Perform regression models, internally used for
#' parallelization
#'
#' @param X A vector of data
#' @param PC A matrix of principal components
#' @param method Specify a method to be used for regression model, which can be
#' "linear", "poisson", and "negative.binomial". Default = "linear"
#'
#' @return A vector of residuals computed from regression model
#'
#' @import stats
#' @import MASS

do.glm = function(X,PC,method="linear"){
  if (method=="poisson"){
    ret = glm(X ~ PC ,family=poisson(), na.action=na.exclude)$residuals
  } else if (method=="negative.binomial"){
    ret = glm.nb(X ~ PC, na.action=na.exclude)$residuals
  }else{
    ret = glm(X ~ PC, family= gaussian(), na.action=na.exclude)$residuals
  }

  return(ret)
}

#' (Internal function) Replace missing values by specified values, internally
#' used for parallelization
#'
#' @param X A vector of data
#' @param missing The old characters representing a missing value, for example, 'NA' or '-'
#' @param rep The new characters to replace the old characters, for example, 'NULL' or '-1'
#'
#' @return A vector of data with replaced character for miss values


replace.missing <- function(X,missing=NA,rep){
  if (is.na(missing)){
    idx = which(is.na(X))
    X[idx] = rep[idx]
  }else{
    idx = which(X == missing)
    X[idx] = rep[idx]
  }
  return(X)
}
