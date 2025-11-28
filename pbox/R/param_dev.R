##############################################################
#' Compute Parameter Deviations
#'
#' @description
#' Internal method to calculate ± 1, 2, 3 standard deviations for given parameters.
#'
#' @param param Numeric vector of parameters.
#' @param sigma Numeric value representing standard deviation (default is 0.05).
#' @param range Numeric vector specifying range of deviations (default is seq(-3, 3, 1)).
#' @return Numeric vector of parameters adjusted by the specified deviations.
#' @name param_dev
#' @examples
#' param_dev(31)
#' @export
setGeneric("param_dev",
           def=function(param="numeric", sigma = 0.05, range = seq(-3, 3, 1)) {
             standardGeneric("param_dev")
             })


#' @rdname param_dev
#' @export
setMethod("param_dev",
          definition=function(param="numeric", sigma = 0.05, range = seq(-3, 3, 1)) {
            param + range * sigma
          })
