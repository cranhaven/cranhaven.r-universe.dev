check_compare_trafo <- function(object, trafos, std) {
  
  if (!inherits(object, "lm")) { 
    stop(paste0("object is of class", class(object), " but it needs to be of 
                class lm." ))
  }
  if (!inherits(trafos, "list")) {
    stop(paste0("trafos needs to be a list with two elements of type trafo."))
  }
  if (length(trafos) != 2) {
    stop(paste0("trafos is of length ", length(trafos), ". trafos needs to be a list with two elements of type trafo."))
  }
  if (!inherits(trafos[[1]], "trafo")) {
      stop("The elements of trafos need to be of type trafo")
  }
  if (!inherits(trafos[[2]], "trafo")) {
    stop("The elements of trafos need to be of type trafo")
  }
  if (!is.logical(std) || length(std) != 1) {
    stop("std must be a logical value. Set std to TRUE or FALSE.")
  }
}