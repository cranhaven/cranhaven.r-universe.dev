get_leaf_codes <- function(x,
                           inductiveCodingHierarchyMarker) {

  if (is.list(x)) {
    ### If it's a list, it has to be a list of vectors; so
    ### we call ourselves for every vector in the list
    return(lapply(x,
                  get_leaf_codes,
                  inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker));
  } else {
    ### If it's just a vector, simply lapply over it
    return(unlist(lapply(x,
                         get_leaf_code,
                         inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker),
                  recursive=FALSE));
  }

}
