#' Convert Numbered Index to Named Index of List Element
#'
#' @param vec \code{numeric} numeric index of list element.
#' @param obj \code{list}
#'
#' @return \code{character} named index of list element.
#' 
#' @export
#'
#' @examples
#' d <- list(a = list(b = list(c = 3, d = 5), e = c(2,4)))
#' num_idx <- c(1,1,2)
#' convert_idx_to_name(num_idx, d)
convert_idx_to_name <- function(vec, obj) {
  
  vec_name <- character()
  
  for (i in seq_along(vec)) {
    if (i == 1) {
      elements <- names(obj)
    } else {
      elements <- names(obj[[vec[1:(i-1)]]])
    }
    if (is.null(elements)) {
      stop("One or more elements of list are not named. Could not identify",
           " element.")
    }
    element_name <- elements[vec[i]]
    # append.
    vec_name <- append(vec_name, element_name)
  }
  
  vec_name
  
}
