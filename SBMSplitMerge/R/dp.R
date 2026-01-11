#' @title Draw block memberships
#' @description Draw block memberships in a Dirichlet process sampler
#' @details iteratively updates the block  assignment of each node using a Dirichlet process update move
#' @param currsbm current \code{\link{sbm}} object
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} object
#' @return updated \code{sbm} object
drawblocks.dp <- function(currsbm, edges, sbmmod){
    for(i in 1:currsbm$numnodes)
        currsbm <- drawblock.dp(i, currsbm, edges, sbmmod)
    currsbm
}

#' @title Draw block membership
#' @description Draw block membership in a Dirichlet process sampler
#' @details sample a new block assignment for i under a Dirichlet process.
#' Care needs to be taken with singleton blocks to update the parameter model in \code{currsbm}.
#' @param i node to update
#' @param currsbm current \code{sbm} object
#' @param edges an \code{\link{edges}} object
#' @param sbmmod an \code{\link{sbmmod}} object
#' @return updated \code{sbm} object
#' @seealso For full algorithm details see \url{http://doi.org/10.17635/lancaster/thesis/296}
drawblock.dp <- function(i, currsbm, edges, sbmmod){
    ## current block of i
    currb <- as.numeric(as.character(currsbm$blocks$z[i]))

    ## probability calculations
    p <- nodelike(currsbm$blocks, currsbm$params, edges, i, sbmmod) +
            sbmmod$block$dcond(currsbm$blocks, i)

    ## choose new block
    propb <- rcat(1, normaliselogs(p))

    ## if (propb == currb) - do nothing i is assigned to its current block
    ## otherwise:
    if( propb != currb ){
        if( currsbm$blocks$sizes[currb] > 1 ){
            ## if not currently in a singleton block - then update the block assignment
            currsbm <- updateblock(currsbm, i, propb, sbmmod)
       } else{
            ## the current block is a singleton
            if( propb > currsbm$blocks$kappa ){
                ## and the proposed block is a singleton:
                ## resample parameter since labels dont matter
                currsbm$params$thetak[currb,] <- sbmmod$param$r(1)$thetak
            } else{
                ## moving out of currb leaves it empty:
                ## drop thetak[,b] from the state
                p <- params(
                    currsbm$params$theta0
                   ,
                    currsbm$params$thetak[-currb,,drop=FALSE]
                )
                ##  update the blocks
                z <- as.numeric(as.character(updateblock(currsbm$blocks, i, propb)$z))
                currsbm <- sbm(blocks(z), p)
            }
        }
    }
    currsbm
}
