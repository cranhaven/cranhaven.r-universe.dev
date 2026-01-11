#' @title Parameter Matrix
#' @description Make a matrix of parameters
#' @param x object for dispatch
#' @param ... additional arguments for method
#' @return a parameter matrix object
parammat <- function(x, ...)
    UseMethod("parammat", x)

#' @title Parameter Matrix
#' @description Make a matrix of parameters from a matrix of block assignments
#' @param zleft block assignment matrix on the left
#' @param zright block assignment matrix on the right
#' @param params the parameters object
#' @param ... (unused)
#' @return a matrix of parameters of size |\code{zleft}| x |\code{zright}|
parammat.matrix <- function(zleft, zright, params, ...){
    p <- parammat(params, dim(zleft)[1])
    out <- array(0, c(params$dimtheta, dim(zleft)[2], dim(zright)[2]))
    for(d in 1:params$dimtheta)
        out[d,,] <- t(zleft) %*% p[d,,] %*% zright
    out
}

#' @title Parameter Matrix
#' @description Make a matrix of parameters from a \code{blocks} and \code{params} object
#' @param x a \code{blocks} object
#' @param params a \code{params} object
#' @param ... (unused)
#' @return an \code{NxN} matrix \code{P}, with \code{P[i,j] = } the parameter governing edge \code{ij}
#' @export
parammat.blocks <- function(x, params, ...)
    parammat(blockmat(x), blockmat(x), params)

#' @title Parameter Matrix
#' @description Make a matrix of parameters from a \code{params} object
#' @param x a \code{params} object
#' @param kappa - number of blocks to compute for matrix (optional)
#' @param ... (unused)
#' @return a matrix of parameters
parammat.params <- function(x, kappa, ...){
    ## possible to extend passed the number of parameters
    ## so we can look at empty blocks connecting with \theta_0
    if(missing(kappa))
        kappa <- x$kappa
    out <- array(0, c(x$dimtheta, kappa, kappa))
    if(kappa > 1){
        for(d in 1:x$dimtheta){
            out[d,,] <- matrix(x$theta0[d], kappa, kappa)
            diag(out[d,,])[1:x$kappa] <- x$thetak[,d, drop=FALSE]
        }
    } else{
        out[,,] <- x$thetak
    }
    out
}

#' @title Parameter Matrix
#' @description Make a matrix of parameters from an \code{sbm} object
#' @param x an \code{sbm} object
#' @param ... (unused)
#' @return a matrix of parameters
parammat.sbm <- function(x, ...)
    parammat(x$blocks, x$params)
