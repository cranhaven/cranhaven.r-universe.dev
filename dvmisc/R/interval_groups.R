#' Split Continuous Variable into Equal-Width Groups
#' 
#' Splits a continuous variable into equal-width groups. Useful for assessing
#' linearity in regression models.
#' 
#' @param x Numeric vector.
#' @param groups Numeric value specifying number of groups to create.
#' @param ... Arguments to pass to \code{\link[base]{cut}}.
#' 
#' @return Factor variable.
#' 
#' @seealso \code{\link[base]{cut}}
#' 
#' @examples 
#' # Convert values from N(0, 1) into 6 equal-width groups
#' x <- rnorm(1000)
#' groups <- interval_groups(x, 6)
#' table(groups)
#' 
#' # Use interval_groups to detect non-linearity
#' set.seed(123)
#' x <- rnorm(1000)
#' y <- 1.5 + 1.25 * x + 0.25 * x^2 + rnorm(1000)
#' plot(tapply(y, interval_groups(x), mean))
#' 
#' @export
interval_groups <- function(x, groups = 5, ...) {
  
  # Figure out break points to split x into even intervals spanning its range
  x.range <- range(x, na.rm = TRUE)
  cut.breaks <- seq(x.range[1], x.range[2], diff(x.range) / groups)
  cut.breaks[c(1, length(cut.breaks))] <- c(-Inf, Inf)
  
  # Create groups
  groups <- cut(x = x, breaks = cut.breaks, ...)
  
  # Print message and return groups
  num.missing <- sum(is.na(groups))
  message(paste("Observations per group: ",
                paste(table(groups), collapse = ", "),
                ". ", num.missing, " missing.",
                sep = ""))
  return(groups)
  
}