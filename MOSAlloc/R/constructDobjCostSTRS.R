# Filename: constructDobjCostSTRS.R
#
# Date: 22.06.2025
# Author: Felix Willems
# Contact: mail.willemsf+MOSAlloc@gmail.com
#         (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
# Licensing: GPL-3.0-or-later
#
# Please report any bugs or unexpected behavior to
# mail.willemsf+MOSAlloc@gmail.com
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
#
#---------------------------------------------------------------------------
#
#' @title Constructor for cost objective components
#'
#' @description A helper function for generating cost matrix \code{D} and
#' fixed cost vector \code{d} under stratified random sampling (STRS) as
#' input to the multiobjective allocation function \code{mosalloc()}.
#'
#' @param X_cost (type: \code{matrix})
#' A matrix containing stratum- (rows) and type- (columns) specific
#' cost coefficients associated with fixed cost. Types of cost might be, e.g.
#' '$ US', 'minutes', 'sample size', etc.
#' @param X_fixed (type: \code{matrix})
#' A matrix containing stratum- (rows) and type- (columns) specific
#' cost coefficients associated with fixed cost.
#' @param list (type: \code{list})
#' A list of lists taking subpopulation- (domain/area) specific arguments.
#' Elements are lists containing the following components corresponding
#' to one specific cost type:
#' \cr \code{..$stratum_id} (type: \code{numeric})
#' A vector containing the indices of the strata considered for the current
#' objective. The indices must coincide with the row numbers of \code{X_cost}.
#' \cr \code{..$c_type} (type: \code{character} or \code{numeric})
#' The column name or column index of \code{X_cost} to be addressed.
#' \cr \code{..$name} (type: \code{character})
#' The name of the subpopulation (domain/area).
#'
#' @return The function \code{constructDobjCostSTRS()} returns a list containing
#' @returns \code{$D} (type: \code{matrix}): the cost coefficient matrix for
#' cost objectives and
#' @returns \code{$d} (type: \code{vector}): the vector of fixed costs
#' \cr usable as input to the multiobjective allocation function
#' \code{mosalloc()}.
#'
#' @examples
#' # Assume we are given two regions stratified into three strata each. We now
#' # might balance the cost of surveying between both regions.
#'
#' # Stratum-specific variable cost
#' ch <- c(25, 40, 33, 18, 53, 21)
#' names(ch) <- c("R1_S1", "R1_S2", "R1_S3",
#'                "R2_S1", "R2_S2", "R2_S3")
#'
#' # Stratum-specific fixed cost
#' cf <- c(55, 50, 55, 50, 55, 50)
#' names(cf) <- c("R1_S1", "R1_S2", "R1_S3",
#'                "R2_S1", "R2_S2", "R2_S3")
#'
#' # The input \code{D} and \code{d} to \code{mosalloc()} can be specified as
#' # follows:
#'
#' D <- matrix(c(ch[1:3], rep(0, 6), ch[4:6]), 2, 6, byrow = TRUE)
#' d <- as.vector(c(sum(cf[1:3]), sum(cf[4:6])))
#'
#' # Using \code{constructDobjCostSTRS()} this can also be done via
#'
#' X_cost <- matrix(ch, ncol = 1)
#' colnames(X_cost) <- "$ US"
#'
#' X_fixed <- matrix(cf, ncol = 1)
#' colnames(X_fixed) <- "$ US"
#'
#' list <- list(list(stratum_id = 1:3, c_type = "$ US", name = "R1"),
#'              list(stratum_id = 4:6, c_type = "$ US", name = "R2"))
#' Dc <- constructDobjCostSTRS(X_cost, X_fixed, list)
#'
#' # Evaluation of the output
#' Dc$D - D
#' Dc$d - d
#'
#' @export

constructDobjCostSTRS <- function(X_cost, X_fixed, list) {

  # Check input parameter
  if (is.null(X_fixed)) {
    X_fixed <- X_cost - X_cost
    }

  if (!(is.matrix(X_cost) && is.matrix(X_fixed))) {
    stop("X_cost is not a matrix or X_fixed is not a vector!")
  }

  if (!all(dim(X_cost) == dim(X_fixed))) {
    stop("Dimension of X_cost and X_fixed differ!")
  }

  if (!is.list(list)) stop("list is not a list!")

  # For each list element construct the corresponding precision components
  out <- lapply(list, function(L) {
    # Check if all parameter are specified
    if (!all(c("stratum_id", "c_type", "name") %in% names(L))) {
      stop("Incorrect format of list!")
    }

    if (all(as.integer(L$stratum_id) != as.numeric(L$stratum_id))) {
      stop("stratum_id is not an index!")
    }
    if (length(L$stratum_id) > dim(X_cost)[1]) {
      stop("Stratum out of range!")
    }

    A1 <- matrix(0, nrow = 1, ncol = dim(X_cost)[1] + 1)
    A1[1, L$stratum_id] <- X_cost[L$stratum_id, L$c_type]
    A1[1, ncol(A1)] <- sum(X_fixed[L$stratum_id, L$c_type])
    colnames(A1) <- c(1:dim(X_cost)[1], "")
    rownames(A1) <- paste0(L$c_type, "_", L$name)
    A1
  })

  # Construct output
  res <- do.call(rbind, out)
  D <- res[, -ncol(res), drop = FALSE]
  d <- as.vector(res[, ncol(res)])
  names(d) <- rownames(D)
  list(D = D, d = d)
}