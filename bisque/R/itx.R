#' Named inverse transformation functions
#' 
#' Evaluates the inverse of the named link function at the locations
#' \code{x}.
#' 
#' @importFrom stats plogis
#' 
#' @param x Values at which to evaluate the inverse link function
#' @param link Character vector specifying link function for which the 
#'   inverse link function should be evaluated.  Supports \code{'identity'},
#'   \code{'log'}, and \code{'logit'}.
#' @param linkparams Optional list of additional parameters  for link functions.
#'   For example, the logit function can be extended to allow mappings to any 
#'   closed interval.  There should be one list entry for each link function.  
#'   Specify NA if defaults should be used.
#'   
#' @export
#' 
#' @examples 
#' bisque:::itx(0, 'logit', list(NA))
#' 
itx = function(x, link, linkparams) {
  # function to invert link transformations
  for(i in 1:length(link)) {
    x[i] = switch (link[i],
                   'identity' = x[i],
                   'log' = exp(x[i]),
                   'logit' = {
                     range = linkparams[[i]]
                     if(any(is.na(range))) {
                       range = c(0,1)
                     }
                     plogis(x[i]) * diff(range) + range[1]
                   }
    )
  }
  x
}
