#' Split Continuous Variable into Quantile Groups (Survey Version)
#' 
#' Complex survey version of \code{\link{quant_groups}}. Speeds up process of 
#' creating quantile groups based on survey weighted percentiles.
#' 
#' @param x Formula, e.g. \code{~varname}.
#' @param by Formula, e.g. \code{~varname}.
#' @param groups Numeric value specifying number of quantile groups.
#' @param probs Numeric vector.
#' @param design A \code{svydesign} or \code{svrepdesign} object.
#' 
#' 
#' @return Factor variable.
#' 
#' 
#' @export
quant_groups_svy <- function(x, by = NULL, groups = 4, probs = NULL, design) {
  
  # If probs unspecified, create from groups
  if (is.null(probs)) {
    probs <- seq(0, 1, 1 / groups)
  }
  
  # Calculate quantiles depending on whether there is a by variable
  if (is.null(by)) {
    
    quantiles <- svyquantile(x, design = design, quantiles = probs, na.rm = TRUE)
    quantiles[c(1, length(quantiles))] <- c(-Inf, Inf)
    groups <- cut(x = design$variables[, rownames(quantiles)], 
                  breaks = quantiles, labels = FALSE)
    
  } else {

    quantiles <- svyby(x, by = by, FUN = svyquantile, design = design, 
                       quantiles = probs, keep.var = FALSE, na.rm = TRUE)
    quantiles[, 2] <- -Inf
    quantiles[, ncol(quantiles)] <- Inf
    
    xname <- rownames(svyquantile(x, design = design, quantiles = 0.5, na.rm = TRUE))
    x.values <- design$variables[, xname]
    by.values <- design$variables[, names(quantiles)[1]]
    
    groups <- c()
    for (ii in 1: nrow(quantiles)) {
      locs <- which(by.values == quantiles[ii, 1])
      groups[locs] <- cut(x = x.values[locs], breaks = quantiles[ii, -1], 
                          include.lowest = TRUE, labels = FALSE)
    }
    
  }
  
  return(groups)
  
}
