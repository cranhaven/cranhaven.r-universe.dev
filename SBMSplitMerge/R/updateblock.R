#' @title Update the block assignment of a node
#' @description change the block assignment in \code{x} of a node to a new block
#' @param x object for dispatch
#' @param ... additional arguments for method
#' @return object like `x` with updated block structure
#' @seealso \code{\link{updateblock.blocks}} \code{\link{updateblock.sbm}}
updateblock <- function(x, ...){
    UseMethod("updateblock")
}

#' @title Update the block assignment of a node
#' @description change the block assignment in an \code{blocks} object to a new block
#' @param blocks a \code{\link{blocks}} object
#' @param newblock the new block for node i
#' @param i the node to update
#' @return new \code{blocks} object
updateblock.blocks <- function(blocks, i, newblock){
    if(newblock <= blocks$kappa){
        tmp <- blocks$z
        tmp[i] <- newblock
    } else {
        tmp <- blocks$z
        levels(tmp) <- c(levels(tmp), newblock)
        tmp[i] <- newblock
    }
    blocks(tmp)
}

#' @title Update the block assignment of a node
#' @description change the block assignment in an \code{sbm} object to a new block
#' @param currsbm an \code{\link{sbm}} object
#' @param newblock the new block for node i
#' @param i the node to update
#' @param model an \code{\link{sbmmod}} object
#' @note If adding a new block, this draws from the prior
#' @return new \code{sbm} object
updateblock.sbm <- function(currsbm, i, newblock, model){
    currkappa <- currsbm$blocks$kappa
    newblocks <- updateblock(currsbm$blocks, i, newblock)
    if(newblocks$kappa > currkappa){
        newparams <- params(currsbm$params$theta0,
                            rbind(currsbm$params$thetak, model$param$r(1)$thetak))
    } else{
        newparams <- currsbm$params
    }
    sbm(newblocks, newparams)
}
