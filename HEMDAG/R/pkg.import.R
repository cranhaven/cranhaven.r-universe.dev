#' @importFrom graph graphNEL nodes edges numEdges numNodes addNode addEdge edgeWeights degree acc edgeMatrix adj
#' @importFrom RBGL bellman.ford.sp dijkstra.sp transitive.closure tsort
#' @importFrom precrec evalmod auc format_nfold
#' @importFrom preprocessCore normalize.quantiles
#' @importFrom methods setGeneric setMethod
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
#' @importFrom plyr mapvalues
#' @importFrom utils read.table write.table
#' @useDynLib HEMDAG, .registration=TRUE

## quiet concerns of R CMD check.
## avoid warning: no visible binding for global variable
if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("curvetypes","detectCores","i"));
}
