#' @title Likelihood of node assignment
#' @description Calculate the likelihood of a nod belonging to each of block
#' @details the number of blocks considered is either the number of blocks in \code{sbm (kappa)} or \code{kappa+1} when \code{sbmmod} has a variable number of blocks.
#' care is taken for data which is directed and with loops.
#' @param blocks an \code{\link{blocks}} object
#' @param params an \code{\link{params}} object
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} object
#' @param i the node of interest
#' @param ... additional arguments for \code{nodelike.blocks}
#' @return likelihood of edges emanating from node i
nodelike <- function(blocks, params, edges, i, sbmmod, ...){
    ## number of blocks to make prob vector
    kappa <- blocks$kappa + !sbmmod$block$fixkappa
    ## assignment matrix
    z <- blockmat(blocks, kappa)
    ## parameters between i and j for i in each of the kappa blocks
    p <- parammat(diag(kappa), z, params)
    ## likelihood matrix of edges *from* i
    like <- sbmmod$edge$logd(edges$E[rep(i, kappa),], p)
    if(!edges$sym){
        ## if not symmettric need to account for edges *to* i separately
        p <- parammat(z, diag(kappa), params)
        ## likelihood matrix of edges *to* i
        like <- like + t(sbmmod$edge$logd(edges$E[,rep(i, kappa)], p))
    }
    if(!edges$loops)
        like <- like[,-i]
    rowSums(like, na.rm=TRUE)
}
