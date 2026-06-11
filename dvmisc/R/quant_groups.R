#' Split Continuous Variable into Quantile Groups
#' 
#' Splits a continuous variable into quantiles groups. Basically combines 
#' \code{\link[stats]{quantile}} and \code{\link[base]{cut}} into a single 
#' function. Note that \code{\link{create_qgroups}} will likely supersede this 
#' function in future versions of \strong{dvmisc}. 
#' 
#' @param x Numeric vector.
#' @param groups Numeric value specifying number of quantile groups.
#' @param probs Numeric vector specifying probabilities.
#' @param quantile.list Arguments to pass to \code{\link[stats]{quantile}}.
#' @param cut.list Arguments to pass to \code{\link{cut}}. 
#' 
#' @return Factor variable.
#' 
#' @examples 
#' # Convert values from N(0, 1) into quintiles (i.e. 5 groups)
#' x <- rnorm(1000)
#' groups <- quant_groups(x, 5)
#' table(groups)
#' 
#' @export
quant_groups <- function(x, groups = 4, probs = NULL, 
                         quantile.list = NULL, cut.list = NULL) {
  
  # If probs unspecified, create from groups
  if (is.null(probs)) {
    probs <- seq(0, 1, 1 / groups)
  }
  
  # Calculate quantiles
  quantile.list <- list_override(list1 = list(na.rm = TRUE), list2 = quantile.list)
  quantiles <- do.call(quantile, c(list(x = x, probs = probs), quantile.list))
  
  # Create quantile groups
  cut.list <- list_override(list1 = list(include.lowest = TRUE), list2 = cut.list)
  groups <- do.call(cut, c(list(x = x, breaks = quantiles), cut.list))
  
  # Print message and return groups
  num.missing <- sum(is.na(groups))
  message(paste("Observations per group: ",
                paste(table(groups), collapse = ", "),
                ". ", num.missing, " missing.",
                sep = ""))
  return(groups)

}