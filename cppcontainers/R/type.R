#' Get data type
#' 
#' Obtain the data type of a container.
#' 
#' @param x A \code{cppcontainers} object.
#' 
#' @details The available types are integer, double, string, and boolean. They correspond to the integer, numeric/ double, character, and logical types in 
#' R.
#' 
#' @returns A named character vector for CppMap, CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects. A character otherwise.
#' 
#' @seealso \link{print}, \link{sorting}, \link{to_r}.
#' 
#' @examples
#' s <- cpp_set(4:6)
#' type(s)
#' # [1] "integer"
#' 
#' m <- cpp_unordered_map(c("hello", "world"), c(0.5, 1.5))
#' type(m)
#' #      key    value 
#' # "string" "double" 
#' 

#' @aliases type,CppSet-method type,CppUnorderedSet-method type,CppMultiset-method type,CppUnorderedMultiset-method type,CppMap-method 
#' type,CppUnorderedMap-method type,CppMultimap-method type,CppUnorderedMultimap-method type,CppStack-method type,CppQueue-method 
#' type,CppPriorityQueue-method type,CppVector-method type,CppDeque-method type,CppForwardList-method type,CppList-method

#' @export
methods::setGeneric("type", function(x) standardGeneric("type"))

#' @include classes.R

#' @export
methods::setMethod("type", methods::signature(x = "CppSet"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppMultiset"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppMap"), function(x) {
  return(c(key = x@key_type, value = x@value_type))
})

#' @export
methods::setMethod("type", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(c(key = x@key_type, value = x@value_type))
})

#' @export
methods::setMethod("type", methods::signature(x = "CppMultimap"), function(x) {
  return(c(key = x@key_type, value = x@value_type))
})

#' @export
methods::setMethod("type", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(c(key = x@key_type, value = x@value_type))
})

#' @export
methods::setMethod("type", methods::signature(x = "CppStack"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppQueue"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppPriorityQueue"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppVector"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppDeque"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppForwardList"), function(x) {
  return(x@type)
})

#' @export
methods::setMethod("type", methods::signature(x = "CppList"), function(x) {
  return(x@type)
})
