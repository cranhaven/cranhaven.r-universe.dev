## function to convert a vector of strings (either prefab distances,
## C++ code definitions or file names containing code definitions)
## to a vector of function pointers. 

## first version (NOT WORKING) is simply to cut the appropriate code
## from the supersom function

getDistancePointers <- function(dist.fcts,
                                prefabDists = c("sumofsquares", "euclidean",
                                                "manhattan", "tanimoto")) {  
  ## Text distance functions can be predefined names, C++ function
  ## definitions, or file names containing these definitions.
  
  dist.ptrs <- vector(length(dist.fcts), mode = "list")
  
  ## prefab dists first
  if (prefab.idx <- dist.fcts %in% prefabDists) {
    ## first convert to a factor, and then to a pointer...
    dist.ptrs[prefab.idx] <-
      CreateStdDistancePointers(factor(dist.fcts[prefab.idx],
                                       levels = prefabDists))
  }
  
  ## for the rest:
  if (rest.idx <- !prefab.idx) {
   
    if (!exists(dist.fcts)) {
      stop(paste("Cannot find (custom) distance function: ", dist.fcts, sep=""))
    }
    dist.ptrs[[1]] <- eval(call(dist.fcts))
    
  }
  
  dist.ptrs
}
