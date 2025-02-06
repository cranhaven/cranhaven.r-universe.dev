#' Print the sorting order
#' 
#' Print the sorting order of a priority queue.
#' 
#' @param x An CppPriorityQueue object.
#' 
#' @returns Returns \code{"ascending"} or \code{"descending"}.
#' 
#' @seealso \link{cpp_priority_queue}, \link{sort}.
#' 
#' @examples
#' q <- cpp_priority_queue(4:6)
#' sorting(q)
#' # [1] "descending"
#' 
#' q <- cpp_priority_queue(4:6, "ascending")
#' sorting(q)
#' # [1] "ascending"
#' 

#' @aliases sorting,CppPriorityQueue-method

#' @export
methods::setGeneric("sorting", function(x) standardGeneric("sorting"))

#' @include classes.R

#' @export
methods::setMethod("sorting", methods::signature(x = "CppPriorityQueue"), function(x) {
  return(ifelse(x@ascending, "ascending", "descending"))
})
