################################################################################
#
# A function to build basic constraint matrices from keywords
#
################################################################################

#' Common constraints
#'
#' Build a constraint matrix from common simple constraints. Internally used
#' by \code{\link{g}} to construct index-specific constraint matrices.
#' 
#' @param p The number of variables.
#' @param first Indicates sign constraint for first coefficient.
#' Recommended for identifiability if no other constraint is passed. 
#' @param sign Sign constraint applied to all coefficients. \code{0}: no constraint, 
#' @param monotone Monotonicity constraint. \code{0}: no constraint, \code{-1}:
#'  decreasing coefficients and \code{1}: increasing coefficients.
#' @param convex Convexity constraint. \code{0}: no constraint, \code{-1}:
#'  convex coefficients and \code{1}: concave coefficients.
#'
#' @details 
#' For monotonicity and convexity / concavity, the function assumes the 
#' coefficients are ordered. For instance, for increasing monotone coefficients,
#' the first one will be lower than the second, which be lower than the
#' third and so on.
#' 
#' The function automatically removes redundant constraints. For instance,
#' if both \code{sign = 1} and \code{monotone = 1}, then only the sign
#' constraint on the first variable is kept as others are not needed.
#' 
#' Note that, for all arguments, any number can be passed to the function. In
#' which case, the sign of the argument is used. Therefore passing
#' \code{monotone = 3.14} is the same as passing \code{monotone = 1}.
#' 
#' @return 
#' A p-column constraint matrix.
#' 
#' @examples 
#' # By default, produces only the identifiability constraint
#' build_constraints(4)
#' 
#' # Positive and increasing coefficients
#' build_constraints(4, sign = 1, monotone = 1)
#' 
#' # Concavity constraint
#' build_constraints(7, convex = -1)
#' 
#' # Any numeric can be passed to the function
#' build_constraints(5, monotone = pi)
#' 
#' @export
build_constraints <- function(p, first = 0, sign = 0, monotone = 0, convex = 0)
{
  # Initialize list of constraint matrix and constraints
  cmatlist <- vector("list", 4)
  allcons <- sign(c(first, sign, monotone, convex))
  
  # Identifiability constraint
  # Only if there is no overall sign constraint
  if (allcons[1] & !allcons[2]){
    cmatlist[[1]] <- allcons[1] * c(1, rep_len(0, p - 1))
  }
  
  # Sign constraint
  if (allcons[2]){
    cmatlist[[2]] <- allcons[2] * diag(p)
  }
  
  # Monotone and convexity constraints
  for (i in 3:4){
    if (allcons[i]){
      # Matrix
      cmatlist[[i]] <- allcons[i] * diff(diag(p), diff = i - 2)
      
      # Remove redundant constraints
      if (allcons[i - 1]){
        ind <- if (allcons[i - 1] == allcons[i]) 1 else p
        cmatlist[[i - 1]] <- cmatlist[[i - 1]][ind,]
      }
    }
  }
  
  # Put everything together and return
  do.call(rbind, cmatlist)
}
