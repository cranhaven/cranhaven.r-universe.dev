#' @title Create or update a vector-based linear equality constraint set
#' 
#' @description
#' \code{linear.constraint.eq} creates a vector-based linear equality
#' constraint: Aeq(range) * factors == beq
#'
#' @param constraints.linear the current set of equality constraints
#' @param range the range of the variables to set (default 1 if factors is NULL)
#' @param beq right-hand side scalar
#' @param factors values to set for each variable in the given range
#' 
#' @return the new (updated) set of equality constraints
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
linear.constraint.eq <- function(constraints.linear, range, beq, factors=NULL) {
  add_a <- rep(0, constraints.linear$n)
  if (is.null(factors)) { add_a[range] <- 1 } else { add_a[range] <- factors }
  if (!is.null(constraints.linear$Aeq)) { constraints.linear$Aeq <- rbind(constraints.linear$Aeq, add_a) } else { constraints.linear$Aeq <- add_a }
  if (!is.null(constraints.linear$beq)) { constraints.linear$beq <- c(constraints.linear$beq, beq) } else { constraints.linear$beq <- beq }
  return(constraints.linear)
}
