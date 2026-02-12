# Filename: constructArestrSTRS.R
#
# Date: 06.06.2025; modified: 31.12.2025
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
#' @title Constructor for precision constraints
#'
#' @description A helper function for generating precision matrix \code{A}
#' and corresponding right-hand side \code{a} under stratified random
#' sampling (STRS) as input to the multiobjective allocation function
#' \code{mosalloc()}.
#'
#' @param X_var (type: \code{matrix})
#' A matrix containing stratum- (rows) and variable- (columns) specific
#' precision units.
#' @param X_tot (type: \code{matrix})
#' A matrix containing stratum- (rows) and variable- (columns) specific
#' totals.
#' @param N (type: \code{vector})
#' A vector of stratum sizes.
#' @param list (type: \code{list})
#' A list of lists taking subpopulation- (domain/area) specific arguments.
#' Elements are lists containing the following components which in turn
#' correspond to one specific precision restriction:
#' \cr \code{..$stratum_id} (type: \code{numeric})
#' A vector containing the indices of the strata considered for the current
#' restriction. The indices must coincide with the corresponding row numbers of
#' \code{X_var} and \code{X_tot}.
#' \cr \code{..$variate} (type: \code{character} or \code{numeric})
#' The column name or column index of \code{X_var} to be addressed.
#' \cr \code{..$measure} (type: \code{character}) "RSE" (relative standard
#' error) or "VAR" (variance).
#' \cr \code{..$bound} (type: \code{numeric})
#' An upper bound to "RSE" (or "VAR").
#' \cr \code{..$name} (type: \code{character})
#' The name of the subpopulation (domain/area).
#' @param fpc (type: \code{logical})
#' A \code{TRUE} or \code{FALSE} statement indicating whether the finite
#' population correction should be considered.
#'
#' @return The function \code{constructArestrSTRS()} returns a list containing
#' @returns \code{A} (type: \code{matrix}) a precision matrix for the
#' quality restrictions and
#' @returns \code{a} (type: \code{vector}) a precision vector for the
#' corresponding right-hand side
#' \cr usable as input to the multiobjective allocation function
#' \code{mosalloc()}.
#'
#' @examples
#' # Artificial population of 50 568 business establishments and 5 business
#' # sectors (data from Valliant, R., Dever, J. A., & Kreuter, F. (2013).
#' # Practical tools for designing and weighting survey samples. Springer.
#' # https://doi.org/10.1007/978-1-4614-6449-5, Example 5.2 pages 133-9)
#'
#' # See also https://umd.app.box.com/s/9yvvibu4nz4q6rlw98ac/file/297813512360
#' # file: Code 5.3 constrOptim.example.R
#'
#' Nh <- c(6221, 11738, 4333, 22809, 5467) # stratum sizes
#'
#' # Revenues
#' mh.rev <- c(85, 11, 23, 17, 126) # mean revenue
#' Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0) # standard deviation revenue
#'
#' # Employees
#' mh.emp <- c(511, 21, 70, 32, 157) # mean number of employees
#' Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00) # std. dev. employees
#'
#' # Proportion of establishments claiming research credit
#' ph.rsch <- c(0.8, 0.2, 0.5, 0.3, 0.9)
#'
#' # Proportion of establishments with offshore affiliates
#' ph.offsh <- c(0.06, 0.03, 0.03, 0.21, 0.77)
#'
#' # Matrix containing stratum-specific variance components
#' X_var <- cbind(Sh.rev**2,
#'                Sh.emp**2,
#'                ph.rsch * (1 - ph.rsch) * Nh/(Nh - 1),
#'                ph.offsh * (1 - ph.offsh) * Nh/(Nh - 1))
#' colnames(X_var) <- c("rev", "emp", "rsch", "offsh")
#'
#' # Matrix containing stratum-specific totals
#' X_tot <- cbind(mh.rev, mh.emp, ph.rsch, ph.offsh) * Nh
#' colnames(X_tot) <- c("rev", "emp", "rsch", "offsh")
#'
#' # Examples
#' #----------------------------------------------------------------------------
#' # Example 1: Assume we require at maximum 5 % relative standard error (RSE)
#' # for estimates of the proportion of businesses with offshore affiliates.
#' #
#' # The input \code{A} and \code{a} to \code{mosalloc} can be calculated as
#' # follows:
#' A <- matrix(ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2,
#' nrow = 1)
#' a <- sum(ph.offsh * (1 - ph.offsh) * Nh**2/(Nh - 1)
#' )/sum(Nh * ph.offsh)**2 + 0.05**2
#'
#' # Using \code{constructArestrSTRS()} this can also be done via
#' list <- list(list(stratum_id = 1:5, variate = "offsh", measure = "RSE",
#'                   bound = 0.05, name = "pop"))
#' Ac <- constructArestrSTRS(X_var, X_tot, Nh, list, fpc = TRUE)
#'
#' # or equivalently by
#' list <- list(list(stratum_id = 1:5, variate = 4, measure = "RSE",
#'                   bound = 0.05, name = "pop"))
#' Ac <- constructArestrSTRS(X_var, X_tot, Nh, list, fpc = TRUE)
#'
#' # Evaluation of the output
#' Ac$A - A
#' Ac$a - a
#'
#' # Example 2: Assume we require at maximum 5 % relative standard error for
#' # estimates of the proportion of businesses with offshore affiliates and
#' # for estimates of the proportion of businesses claiming research credit
#' # separately for strata 1:2 and 3:5 each.
#'
#' list <- list(list(stratum_id = 1:2, variate = "offsh", measure = "RSE",
#'                   bound = 0.05, name = "D1"),
#'              list(stratum_id = 3:5, variate = "offsh", measure = "RSE",
#'                   bound = 0.05, name = "D2"),
#'              list(stratum_id = 1:2, variate = "rsch", measure = "RSE",
#'                   bound = 0.05, name = "D1"),
#'              list(stratum_id = 3:5, variate = "rsch", measure = "RSE",
#'                   bound = 0.05, name = "D2"))
#' Ac <- constructArestrSTRS(X_var, X_tot, Nh, list, fpc = TRUE)
#'
#' @export

constructArestrSTRS <- function(X_var, X_tot, N, list, fpc = TRUE) {

  # Check input parameter
  if (!(is.matrix(X_var) && is.matrix(X_tot))) {
    stop("X_var or X_tot is not a matrix!")
  }

  if (!all(dim(X_var) == dim(X_tot))) {
    stop("Dimensions of X_var and X_tot differ!")
  }

  if (!is.vector(N)) {
    stop("N is not a vector!")
  }

  if (length(N) != dim(X_var)[1]) {
    stop("Number of strata do not match!")
  }

  if (!is.list(list)) stop("list is not a list!")

  if (!is.logical(fpc)) stop("fpc is not a logical!")

  # For each list element construct the corresponding precision components
  out <- lapply(list, function(L) {
    # Check if all parameter are specified
    lnams <- c("stratum_id", "variate", "measure", "bound", "name")
    if (!all(lnams %in% names(L))) {
      stop("Incorrect format of list!")
    }

    if (all(as.integer(L$stratum_id) != as.numeric(L$stratum_id))) {
      stop("stratum_id is not an index!")
    }

    if (length(L$stratum_id) > length(N)) {
      stop("Statum out of range!")
    }

    if (L$measure == "RSE") {
      A1 <- matrix(0, nrow = 1, ncol = length(N) + 1)
      A1[1, L$stratum_id] <- X_var[L$stratum_id, L$variat] * N[L$stratum_id
      ]**2  / sum(X_tot[L$stratum_id, L$variat])**2
    }
    if (L$measure == "VAR") {
      A1 <- matrix(0, nrow = 1, ncol = length(N) + 1)
      A1[1, L$stratum_id] <- X_var[L$stratum_id, L$variat] * N[L$stratum_id]**2
    }
    if (fpc == TRUE) {
      A1[ncol(A1)] <- A1[-ncol(A1)] %*% (1 / N) + L$bound**2
    } else {
      A1[ncol(A1)] <- L$bound**2
    }
    colnames(A1) <- c(1:length(N), "")
    rownames(A1) <- paste0(L$variate, "_", L$name)
    A1
  })

  # Construct output
  res <- do.call(rbind, out)
  A <- res[, -ncol(res), drop = FALSE]
  a <- as.vector(res[, ncol(res)])
  names(a) <- rownames(A)
  list(A = A, a = a)
}