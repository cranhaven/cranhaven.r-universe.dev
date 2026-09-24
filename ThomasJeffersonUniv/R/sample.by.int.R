


#' @title Indices of Stratified Sampling
#' 
#' @param f \link[base]{factor}
#' 
#' @param ... potential parameters of \link[base]{sample.int}
#' 
#' @details
#' End user should use \link[base]{interaction} to combine multiple \link[base]{factor}s.
#' 
#' @returns 
#' Function [sample.by.int] returns an \link[base]{integer} \link[base]{vector}.
#' 
#' @examples
#' id1 = sample.by.int(state.region, size = 2L)
#' state.region[id1]
#' 
#' id2 = sample.by.int(f = with(npk, interaction(N, P)), size = 2L)
#' npk[id2, c('N', 'P')] # each combination selected 2x
#' 
#' @seealso `dplyr::slice_sample`
#' @export
sample.by.int <- function(f, ...) {
  
  # CRAN does not allow ?base::.Internal
  #f <- as.factor(f)
  #ind <- .Internal(split(seq_along(f), f)) # base::split.default
  
  ind <- split.default(seq_along(f), f = f)
  ret <- lapply(ind, FUN = sample, ...)
  sort.int(unlist(ret, use.names = FALSE))
}

