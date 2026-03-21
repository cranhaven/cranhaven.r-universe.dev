#' @title Create or update a vector-based linear inequality constraint set
#' 
#' @description
#' \code{linear.constraint.iq} creates a vector-based linear inequality
#' constraint: Aeq(range) * factors <= beq
#'
#' @param constraints.linear the current set of inequality constraints
#' @param range the range of the variables to set (default 1 if factors is NULL)
#' @param b right-hand side scalar
#' @param factors values to set for each variable in the given range
#' @param leq if false then the sign of the constraint will be inversed
#' 
#' @return the new (updated) set of inequality constraints
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
# linear constraint: A(range) * factors <= b
linear.constraint.iq <- function(constraints.linear, range, b, factors=NULL, leq=TRUE) {
  add_a <- rep(0, constraints.linear$n)
  if (is.null(factors)) { add_a[range] <- 1 } else { add_a[range] <- factors }
  if (!leq) {
   add_a <- -1 * add_a
   sum <- -1 * sum
  }
  if (!is.null(constraints.linear$A)) { constraints.linear$A <- rbind(constraints.linear$A, add_a) } else { constraints.linear$A <- add_a }
  if (!is.null(constraints.linear$b)) { constraints.linear$b <- c(constraints.linear$b, b) } else { constraints.linear$b <- b }
  return(constraints.linear)
}