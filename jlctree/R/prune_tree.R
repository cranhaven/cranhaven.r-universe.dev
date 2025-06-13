#' Prunes an \code{rpart} tree to have the desired number of nodes.
#' 
#' @param tree the tree to prune, an \code{rpart} object.
#' @param maxn desired number of terminal nodes.
#' 
#' @return The pruned tree, an \code{rpart} object.
#' @importFrom rpart rpart prune
#' @export
prune_tree <- function(tree, maxn){
    if(class(tree)!= 'rpart'){
        stop("'tree' must be an rpart object.")
    }

    if (max(tree$cptable[,'nsplit']) <= 1){ return (tree) }

    if (max(tree$cptable[,'nsplit']) > maxn-1){
        minsplit <- max(tree$cptable[,'nsplit'][tree$cptable[,'nsplit'] <= maxn-1])
        prunecp <- min(tree$cptable[,'CP'][tree$cptable[,'nsplit'] <= minsplit])
        tree <- prune(tree, prunecp)
    }
    return (tree)
}


