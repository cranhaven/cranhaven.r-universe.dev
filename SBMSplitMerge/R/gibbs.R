#' @title Gibbs-like reassignment of nodes to the current set of blocks
#' @description Sweep through the set of nodes and reassign to the current set of blocks given the current number of blocks
#' @param currsbm an \code{\link{sbm}} object
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} object
#' @return updated \code{\link{sbm}} object with new block assignments
drawblocks.gibbs <- function(currsbm, edges, sbmmod){
    for(i in 1:currsbm$numnodes)
        currsbm <- drawblock.gibbs(i, currsbm, edges, sbmmod)
    currsbm
}

#' @title Gibbs-like reassignment of nodes to the current set of blocks
#' @description Reassign node `i`  to the current set of blocks given the current number of blocks and the other block assignments
#' @param i the node to reassign
#' @param currsbm an \code{\link{sbm}} object
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} object
#' @return updated \code{sbm} object with new block assignment for i
drawblock.gibbs <- function(i, currsbm, edges, sbmmod){
    p <- sbmmod$block$dcond(currsbm$blocks, i) +
        nodelike(currsbm$blocks, currsbm$params, edges, i, sbmmod)[1:currsbm$blocks$kappa]
    updateblock(currsbm, i, rcat(1, normaliselogs(p)), sbmmod)
}
