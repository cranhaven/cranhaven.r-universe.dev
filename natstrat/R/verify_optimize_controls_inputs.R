
#' Verify the inputs to \code{\link{optimize_controls}()}
#'
#' Makes sure that the inputs to \code{\link{optimize_controls}()} are in the correct
#' format and feasible.
#'
#' @inheritParams optimize_controls
#'
#' @return No return value. If there is a problem with the inputs to \code{\link{optimize_controls}()},
#' an error is raised.
#'
#' @keywords internal

verify_inputs <- function(X, importances, ratio, q_s, st, z, treated, integer, solver) {
  if (is.data.frame(X))
    X <- as.matrix(X)
  stopifnot(!is.null(ratio) || !is.null(q_s))
  stopifnot(is.null(ratio) | (is.vector(ratio) & all(ratio > 0)))
  stopifnot(is.vector(st) | is.factor(st))
  stopifnot(is.vector(z) | is.factor(z))
  stopifnot(is.matrix(X))
  stopifnot(length(z) == (dim(X)[1]))
  stopifnot(length(z) == length(st))
  stopifnot(is.logical(integer))
  if (!solver %in% c("gurobi", "Rglpk")) {
    stop("\"solver\" must be one of \"gurobi\" or \"Rglpk\".",
         call. = FALSE)
  }
  if (solver == "gurobi" && !requireNamespace("gurobi", quietly = TRUE)) {
    stop("Package \'gurobi\' needed if \"solver\" parameter set to \"gurobi\". Please
         install it or switch the \"solver\" parameter to \"Rglpk\".",
         call. = FALSE)
  }
  z <- factor(z)
  group <- levels(z)
  if (!treated %in% group) {
    stop("\"treated\" must be one of the values in \"z\".")
  }
  n_s <- table(z, st)
  if (min(n_s[group == treated, ]) == 0) {
    warning("Note that at least one stratum has no treated individuals.")
  }
  if (!is.null(q_s)) {
    if (is.vector(q_s)) {
      q_s <- matrix(c(q_s, n_s[group == treated, ]), byrow = TRUE, nrow = 2, dimnames = list(NULL, names(q_s)))
      if (group[1] == treated) {
        q_s <- q_s[c(2, 1), ]
      }
    }
    if (any(q_s[, colnames(n_s)] > n_s)) {
      stop("At least one of the entries for `q_s` is greater than the number of units available in the stratum.
           Please lower `q_s` such that all entries are at most the number of available units.",
           call. = FALSE)
    }
  }
  stopifnot(sort(names(importances)) == sort(colnames(X)))
}


#' Verify the inputs for supplemental comparisons to \code{\link{optimize_controls}()}
#'
#' Makes sure that the inputs for supplemental comparisons to \code{\link{optimize_controls}()} are in the correct
#' format and feasible.
#'
#' @inheritParams optimize_controls
#'
#' @return No return value. If there is a problem with the inputs to \code{\link{optimize_controls}()},
#' an error is raised.
#'
#' @keywords internal

verify_multi_comp_inputs <- function(q_s, q_star_s, n_s, treated, treated_star, weight_star, group, correct_sizes) {
  if (is.null(treated_star)) {
    stop("If \"q_star_s\" is not \"NULL\", \"treated_star\" must also specify the treatment group for reference in each supplemental comparison.")
  } else if (is.list(treated_star)) {
    stop("\"treated_star\" should be a scalar or vector.")
  }
  if (!is.null(q_star_s) & is.list(q_star_s) & length(q_star_s) != length(treated_star)) {
    stop("\"q_star_s\" should be a list with the same number of elements as the vector \"treated_star\".")
  }
  if (!is.null(weight_star)) {
    if(is.list(weight_star)) {
      stop("\"weight_star\" should be a scalar or vector.")
    }
    if(length(weight_star) != length(treated_star)) {
      stop("\"weight_star\" should be a vector with the same number of elements as the vector \"treated_star\".")
    }
  }
  if (is.null(q_star_s)) {
    stop("If \"treated_star\" is not \"NULL\", \"q_star_s\" must also be specified.")
  }
  if (correct_sizes) {
    warning("Sample sizes are only correct in expectation for multiple comparisons. \"correct_sizes\" has thus been switched to `FALSE`.")
  }
  
  if (!all(treated_star %in% group)) {
    stop("Each entry of \"treated_star\" must be one of the values in \"z\".")
  }
  
  for (t in treated_star) {
    if (min(n_s[group == t, ]) == 0) {
      warning("Note that at least one stratum has no treated individuals for at least one supplemental comparison.")
    }
  }
  
  if (!is.null(q_star_s)) {
    if (!is.list(q_star_s)) {
      q_star_s <- list(q_star_s)
    }
    # Set up q_s for first comparison
    if (is.vector(q_s)) {
      q_s <- matrix(c(q_s, n_s[group == treated, ]), byrow = TRUE, nrow = 2, dimnames = list(NULL, names(q_s)))
      if (group[1] == treated) {
        q_s <- q_s[c(2, 1), ]
      }
    }
    Q_s <- q_s
    
    # Check whether a sample size within a comparison is too large
    for (comp in 1:length(q_star_s)) {
      q <- q_star_s[[comp]]
      t <- treated_star[comp]
      if (is.vector(q)) {
        q <- matrix(c(q, n_s[group == t, ]), byrow = TRUE, nrow = 2, dimnames = list(NULL, names(q)))
        if (group[1] == t) {
          q <- q[c(2, 1), ]
        }
      }
      
      Q_s <- Q_s + q
      
      if (any(q[, colnames(n_s)] > n_s)) {
        stop("At least one of the entries for `q_star_s` is greater than the number of units available in the stratum.
           Please lower `q_star_s` such that all entries are at most the number of available units.",
             call. = FALSE)
      }
    }
    # Check whether sample size across comparisons is too large
    if (any(Q_s[, colnames(n_s)] > n_s)) {
      stop("The total number of units desired across comparisons for at least one stratum
            is greater than the number of units available in the stratum.
            Please lower `q_s` or `q_star_s` accordingly.",
           call. = FALSE)
    }
  }
}



#' Process the desired sample sizes for \code{\link{optimize_controls}()}
#'
#' Processes the inputs to \code{\link{optimize_controls}()} to formulate sample size constraints.
#'
#' @inheritParams optimize_controls
#'
#' @return A matrix of sample sizes for each treatment and stratum.
#'
#' @keywords internal

process_qs <- function(ratio, q_s, n_s, treated, k, group, st_vals, stratios) {
  if (!is.null(q_s) & is.vector(q_s)) {
    q_s <- matrix(rep(q_s, k), byrow = TRUE, nrow = k, dimnames = list(NULL, names(q_s)))
    q_s[group == treated, ] <- n_s[group == treated, ]
  }
  if (!is.null(ratio) & length(ratio) == 1) {
    ratio <- rep(ratio, k)
    ratio[group == treated] <- 1
  }
  # Determine number of controls desired for each stratum
  if (is.null(q_s)) {
    if (is.null(ratio)) {
      ratio <- sapply(1:nrow(stratios), function(i) min(1, min(stratios[i, ])))
    }
    q_s <- round(ratio %*% t(n_s[group == treated, ]))
    if (any(q_s > n_s)) {
      stop("The ratio you specified is not feasible.
            Please supply `q_s` instead of `ratio` or lower the `ratio` input.",
           call. = FALSE)
    }
    colnames(q_s) <- st_vals
  } else {
    q_s <- q_s[, st_vals]
  }
  
  return(q_s)
}

