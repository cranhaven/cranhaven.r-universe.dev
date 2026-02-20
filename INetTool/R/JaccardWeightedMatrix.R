#' JWmatrix
#'
#' @description This function computes the Jaccard weighted matrix distance
#' between all the pairs of graphs.
#' @param graphL list of graphs as igraph objects with the same nodes.
#'
#' @return weighted Jaccard distance matrix
#' @export
#' @import igraph
#'
#' @examples
#' data("graphL_data")
#' JWmatrix(graphL_data)

JWmatrix <- function(graphL)
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
  rownames(sim.jac) <- rownames(A)
  colnames(sim.jac) <- rownames(A)
  #weighted jaccard function
  pairs <- t(utils::combn(1:nrow(A), 2))
  for (i in 1:nrow(pairs)){
    num <- sum(sapply(1:ncol(A), function(x)(min(A[pairs[i,1],x],A[pairs[i,2],x]))))
    den <- sum(sapply(1:ncol(A), function(x)(max(A[pairs[i,1],x],A[pairs[i,2],x]))))
    sim.jac[pairs[i,1],pairs[i,2]] <- num/den
    sim.jac[pairs[i,2],pairs[i,1]] <- num/den
  }
  sim.jac[which(is.na(sim.jac))] <- 0
  diag(sim.jac) <- 1

  #weighted jaccard distance
  dist.jac <- 1-sim.jac

  if (length(names(graphL))>0){

    rownames(dist.jac) <- names(graphL)
    colnames(dist.jac) <- names(graphL)
  }
                      
  #print(dist.jac)
  return(dist.jac)

}
