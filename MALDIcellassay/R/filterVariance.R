#' Filter for high variance signals
#'
#' @param vars    Numeric vector, variances of signals
#' @param method  Character, filtering method. One of "mean" (default), "median", "q25", "q75" (25 and 75% quantile) or "none".
#' @param verbose Logical, print logs to console.
#'
#' @return Indices of spectra with a high variance
#'
#' @export
#' 
#' @importFrom stats median quantile
#' 
#' @examples
#' data(Blank2022intmat)
#' # get variance of each peak
#' vars <- apply(Blank2022intmat, 2, var)
#' highVarIndicies <- filterVariance(vars, method = "mean", verbose = TRUE)
filterVariance <- function(vars, 
                           method = c("mean", "median", "q25", "q75", "none"), 
                           verbose = TRUE) {
  method <- match.arg(method)
  switch(method,
    "mean" = {
      idx <- which(vars > mean(vars))
      if(verbose) {
        cat("      Found", length(idx), "peaks with high variance using `mean` method.\n")
      }
      
    },
    "median" = {
      idx <- which(vars > median(vars))
      if(verbose) {
        cat("      Found", length(idx), "peaks with high variance using `median` method..\n")
      }
      
    },
    "q25" = {
      idx <- which(vars > quantile(vars, 0.25))
      if(verbose) {
        cat("      Found", length(idx), "peaks with high variance using 25%-quantile method..\n")
      }
      
    },
    "q75" = {
      idx <- which(vars > quantile(vars, 0.75))
      if(verbose) {
        cat("      Found", length(idx), "peaks with high variance using 75%-quantile method..\n")
      }
      
    },
    "none" = {
      # get all indicies, no filtering applied
      idx <- 1:length(vars)
      if(verbose) {
        cat("      No variance filtering applied. Using all peaks.\n")
      }
      
    }
  )
  return(idx)
}
