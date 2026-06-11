#' Create Quantile Groups (Complex Survey Data)
#' 
#' Complex survey version of \code{\link{create_qgroups}}. Relies heavily on the 
#' \strong{survey} package [1,2].
#' 
#' @param x Numeric vector.
#' @param groups Numeric value, e.g. 3 for tertiles, 4 for quartiles, etc.
#' @param probs Numeric vector.
#' @param strata Factor specifying subgroups to calculate quantiles within. For 
#' multivariable subgroups, you can use \code{\link{interaction}}.
#' @param design Survey design object.
#' @param svyquantile_list Arguments to pass to 
#' \code{\link[survey]{svyquantile}}.
#' @param cut_list Arguments to pass to \code{\link{cut}}.
#' 
#' @return Factor variable.
#' 
#' @references
#' 1. Therneau, T. (2015). A Package for Survival Analysis in S. R package
#' version 2.38. \url{https://cran.r-project.org/package=survival}.
#'
#' 2. Therneau, T.M. and Grambsch, P.M. (2000). Modeling Survival Data:
#' Extending the Cox Model. Springer, New York. ISBN 0-387-98784-3.
#' 
#' @export
create_qgroups_svy <- function(x, 
                               groups = 4, 
                               probs = seq(1/groups, 1 - 1/groups, 1/groups), 
                               strata = NULL, 
                               design, 
                               svyquantile_list = list(na.rm = TRUE), 
                               cut_list = list(include.lowest = TRUE)) {
  
  if (is.null(strata)) {
    cutpoints <- do.call(
      svyquantile, 
      c(list(x = ~x, design = design, quantiles = probs), 
        svyquantile_list)
    )
    return(do.call(cut, c(list(x = x, breaks = c(-Inf, cutpoints, Inf)), 
                          cut_list)))
  }
  
  if (class(strata) != "factor") {
    strata <- as.factor(strata)
  }
  if (is.null(cut_list$labels)) {
    cut_list$labels <- paste("Q", 1: (length(probs) + 1), sep = "")
  }
  y <- rep(NA, length(x))
  for (ii in levels(strata)) {
    locs <- which(strata == ii)
    x_ii <- x[locs]
    cutpoints <- do.call(
      svyquantile, 
      c(list(x = ~x_ii, design = design[locs, ], quantiles = probs), 
        svyquantile_list)
    )
    y[locs] <- as.vector(do.call(cut, c(list(x = x_ii, breaks = c(-Inf, cutpoints, Inf)), 
                                        cut_list)))
  }
  as.factor(y)
  
}
