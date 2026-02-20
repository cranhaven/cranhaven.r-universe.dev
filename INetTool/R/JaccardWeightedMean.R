#' JWmean
#'
#' @description This function computes the Mean Weighted Jaccard Distance for
#' Multilayer Networks.
#' @param graphL list of different graphs in igraph format with same nodes.
#'
#' @return a number: the mean distance
#' @import igraph
#' @export
#'
#' @examples
#' data("graphL_data")
#' JWmean(graphL_data)
JWmean <- function(graphL)
{


    #To check if the names of the nodes are the same
    comp <- utils::combn(1:length(graphL), 2)
    for (j in 1:(dim(comp)[2]))
    {
        if(names(table(igraph::V(graphL[[comp[1,j]]])==igraph::V(graphL[[comp[2,j]]])))=="TRUE")
        {}else{stop("Check:Not same nodes in all the graphs")}
    }


    A <- NULL
    for (l in 1:length(graphL))
    {
        AdjW <- igraph::as_adjacency_matrix(graphL[[l]], attr="weight")
        triA <- get_lower_tri_noDiag(AdjW)
        vettriA <- as.vector(triA)
        vetI <- vettriA[!is.na(vettriA)]
        A <- rbind(A,vetI)
    }


    #weighted jaccard similarity matrix setup
    sim.jac <- matrix(0, nrow=nrow(A), ncol=nrow(A))
    #weighted jaccard function
    pairs <- t(utils::combn(1:nrow(A), 2))
    for (i in 1:nrow(pairs)){
        num <- sum(sapply(1:ncol(A), function(x)(min(A[pairs[i,1],x],A[pairs[i,2],x]))))
        den <- sum(sapply(1:ncol(A), function(x)(max(A[pairs[i,1],x],A[pairs[i,2],x]))))
        sim.jac[pairs[i,1],pairs[i,2]] <- num/den
        sim.jac[pairs[i,2],pairs[i,1]] <- num/den
    }
    sim.jac[which(is.na(sim.jac))] <- 0
    diag(sim.jac) <- NA

    #weighted jaccard distance
    dist.jac <- 1-sim.jac
    #print(dist.jac)
    return(mean(dist.jac, na.rm=TRUE))

}

