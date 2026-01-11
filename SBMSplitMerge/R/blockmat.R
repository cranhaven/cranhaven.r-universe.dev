#' @title Block matrix
#' @description converts \code{x} to a matrix of block assignments
#' @param x object for dispatch
#' @param ... additional arguments for method
#' @return matrix of block assignment indicators
#' @seealso \code{\link{blockmat.sbm}} \code{\link{blockmat.blocks}} \code{\link{blockmat.numeric}}
blockmat <- function(x, ...)
    UseMethod("blockmat", x)

#' @title Block matrix
#' @description converts a vector of block assignments to a matrix of block assignments
#' @param x a numeric-vector of node-to-block assignments
#' @param kappa number of blocks
#' @return matrix with \code{kappa} rows and a 1 at \code{(k,i)} if node \code{i} is in block \code{k} under \code{x}
blockmat.numeric <- function(x, kappa){
    out <- diag(kappa)[ , x, drop=FALSE]
    rownames(out) <- paste0("block",1:kappa)
    colnames(out) <- paste0("node",1:length(x))
    out
}

#' @rdname blockmat.numeric
blockmat.factor <- blockmat.numeric

#' @title Block matrix
#' @description converts block assignments of a \code{blocks} object to a matrix of block assignments
#' @param blocks a \code{blocks} object
#' @param kappa number of blocks in matrix
#' @return matrix with \code{kappa} rows and a 1 at \code{(k,i)} if node \code{i} is in block \code{k} under \code{blocks}
blockmat.blocks <- function(blocks, kappa){
    if(missing(kappa))
        kappa <- blocks$kappa
    blockmat(blocks$z, kappa)
}

#' @title Block matrix
#' @description converts block assignments of an \code{sbm} object to a matrix of block assignments
#' @param SBM an \code{sbm} object
#' @param kappa number of blocks in matrix
#' @return matrix with \code{kappa} rows and a 1 at \code{(k,i)} if node \code{i} is in block \code{k} under \code{SBM}
blockmat.sbm <- function(SBM, kappa)
    blockmat(SBM$blocks, kappa)
