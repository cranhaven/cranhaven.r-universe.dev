#' Named transformation functions
#' 
#' Evaluates the named link function at the locations \code{x}.
#' 
#' @importFrom stats qlogis
#' 
#' @param x Values at which to evaluate the link function
#' @param link Character vector specifying link function to evaluate.  Supports 
#'   \code{'identity'}, \code{'log'}, and \code{'logit'}.
#' @param linkparams Optional list of additional parameters  for link functions.
#'   For example, the logit function can be extended to allow mappings to any 
#'   closed interval.  There should be one list entry for each link function.  
#'   Specify NA if defaults should be used.
#'   
#' @export
#' 
#' @examples 
#' bisque:::tx(0.5, 'logit', list(NA))
#' 
tx = function(x, link, linkparams) {
  # function to apply link transformations
  for(i in 1:length(link)) {
    x[i] = switch (link[i],
                   'identity' = x[i],
                   'log' = log(x[i]),
                   'logit' = {
                     range = linkparams[[i]]
                     if(any(is.na(range))) {
                       range = c(0,1)
                     }
                     qlogis((x[i] - range[1])/diff(range))
                   }
    )
  }
  x
}