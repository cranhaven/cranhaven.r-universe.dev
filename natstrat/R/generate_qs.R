#' Calculate desired number of controls per stratum
#'
#' Figure out how many units to take from each stratum when some strata are deficient.
#' The result should be used as an input to \code{\link{optimize_controls}()}.
#'
#' @inheritParams stand
#' @inheritParams optimize_controls
#' @param max_ratio a numeric or vector specifying the maximum ratio to allow in a stratum to achieve
#'   the overall \code{ratio} specified. If \code{NULL}, it is set by default to 1.1 times the desired \code{ratio}.
#'   To have no maximum ratio, set this to \code{Inf}.
#' @param max_extra_s single numeric or named vector or matrix with values corresponding to the maximum desired number
#'   of extra controls to be chosen from each stratum to achieve the overall
#'   \code{ratio} specified. If this is a vector, the names should correspond to the stratum
#'   values from \code{st}. If there are more than two treatment levels,
#'   this should be a matrix with one row per treatment level, in the same
#'   order as the levels of \code{z}. The default is 5 for each stratum in each treatment group.
#'   To have no maximum, set this to \code{Inf}.
#'   If both \code{max_ratio} and \code{max_s} are specified, the
#'   maximum of the two will be used for each stratum.
#' @param strata_dist matrix with both row and column names with names corresponding
#'   to the stratum values from \code{st} and entries corresponding to the distance
#'   associated with taking a control from the stratum associated with the row when
#'   the desired stratum is the one associated with the column. Lower distance values
#'   are more desirable replacements. Typically the diagonal should be 0, meaning
#'   there is no penalty for choosing a unit from the correct stratum.
#' @return A named vector stating how many controls to take from each stratum.
#'
#' @export

generate_qs <- function(z, st, ratio, treated = 1,
                        max_ratio = NULL, max_extra_s = 5, strata_dist = NULL) {
  z <- factor(z)
  # Make sure inputs are good
  verify_inputs_EMD(ratio = ratio, st = st, z = z)

  # Look at strata counts
  frtab <- table(z, st)
  if (min(frtab[levels(z) == treated, ]) == 0) {
    warning("Note that at least one stratum has no treated individuals.")
  }
  stratios <- frtab / frtab[levels(z) == treated, ]
  st_vals <- as.character(colnames(frtab))  # stratum values
  S <- length(st_vals)  # number of strata
  # Determine number of controls desired for each stratum
  if (length(ratio) == 1) {
    ratio <- rep(ratio, length(levels(z)))
    ratio[levels(z) == treated] <- 1
  }
  desired_qs <- round(ratio %*% t(frtab[levels(z) == treated, ]))

  # Make sure the maximum per strata input is okay
  if (is.null(max_extra_s)) {
    max_extra_s <- 5
  }
  if (length(max_extra_s) == 1) {
    max_extra_s <- rep(max_extra_s, S)
    names(max_extra_s) <- st_vals
  }
  if (is.vector(max_extra_s)) {
    if (length(max_extra_s) != S) {
      stop("\"max_extra_s\" should be a scalar,
             a vector with length matching the number of strata occuring in \"st\",
             or a matrix with number of columns matching the number of strata and
             number of rows matching the number of treatment levels.",
           call. = FALSE)
    }
    if(is.null(names(max_extra_s))) {
      stop("If \"max_extra_s\" is a vector, it must have names corresponding to the stratum values.")
    }
    max_extra_s <- matrix(rep(max_extra_s, length(levels(z))), nrow = length(levels(z)),
                          byrow = TRUE, dimnames = list(NULL, names(max_extra_s)))
    max_extra_s[levels(z) == treated, ] <- rep(0, S)
  }
  if (is.matrix(max_extra_s) & is.null(dimnames(max_extra_s))) {
    stop("If \"max_extra_s\" is a matrix,
             it must have column names corresponding to the stratum values and
             rows should be in the order of the treatment levels in \"z\".")
  }
  max_extra_s <- max_extra_s[, st_vals]

  max_s <- desired_qs + max_extra_s

  if (length(max_ratio) == 1) {
    max_ratio <- rep(max_ratio, length(levels(z)))
    max_ratio[levels(z) == treated] <- 1
  }
  if (is.null(max_ratio)) {
    max_ratio <- 1.1 * ratio
  }
  for (row in 1:length(levels(z))) {
    if (max_ratio[row] < Inf) {
    max_s[row, ] <- pmax(max_s[row, ], round(max_ratio[row] * frtab[levels(z) == treated, ]))
    } else {
      max_s[row, ] <- pmax(max_s[row, ], Inf)
    }
  }

  max_s <- pmin(max_s, frtab)

  if (sum(max_s) < sum(desired_qs)) {
    stop("The \"max_ratio\" or \"max_extra_s\" supplied is not feasible because
           it does not allow for enough extra controls in the strata where they are available.
           Please increase these maximums or reduce the desired ratio.")
  }

  # Solve for minimum work value of earth-movers distance if not enough controls in every stratum
  if (is.null(strata_dist)) {
    # No preference for which controls are selected
    strata_dist <- matrix(1, nrow = S, ncol = S)
    diag(strata_dist) <- 0
  } else {
    if (is.null(rownames(strata_dist)) | is.null(colnames(strata_dist))) {
      stop("\"strata_dist\" must have both column and row names corresponding to the stratum values.")
    }
    strata_dist <- strata_dist[st_vals, st_vals]
  }
  strata_dist_flat <- matrix(t(strata_dist), nrow = 1)
  colnames(strata_dist_flat) <- rep(st_vals, each = S)

  q_s <- matrix(0, nrow = length(levels(z)), ncol = S, dimnames = list(levels(z), st_vals))

  for (row in 1:nrow(q_s)) {
    if (any(stratios[row, ] < ratio[row])) {
      # Find how many controls to take from each stratum using earth-movers distance

      q_s[row, ] <- presolve_EMD(S, desired_qs[row, ], max_s[row, ], strata_dist_flat)
    } else {
      # No need for earth-movers distance if there are enough controls per strata
      q_s[row, ] <- desired_qs[row, ]
    }
  }

  return(q_s)
}

#' Verify the inputs to the earthmover's distance problem
#'
#' Check that the ratio, strata, and treated indicator provided to \code{\link{generate_qs}()}
#' are in the correct forms and that the desired ratio is feasible
#' across the population.
#'
#' @inheritParams generate_qs
#'
#' @return No return value. If there is a problem with the inputs to \code{\link{generate_qs}()},
#' an error is raised.
#'
#' @keywords internal


verify_inputs_EMD <- function(ratio, st, z, treated = 1) {
  stopifnot((is.vector(ratio) & all(ratio > 0)) |
              (is.matrix(ratio) & all(ratio > 0)))
  stopifnot(is.vector(st) | is.factor(st))
  stopifnot(is.factor(z))
  stopifnot(length(z) == length(st))
  frtab <- table(z)
  if (length(ratio) == 1) {
    ratio <- rep(ratio, length(levels(z)))
    ratio[levels(z) == treated] <- 1
  }
  desired_Q <- ratio * frtab[levels(z) == treated]
  if (any(desired_Q > frtab)) {
    stop(paste0("There are not enough total units in one of the treatment levels to use the ratio you provided.
               The maximum ratios you may have for the treatment levels are ",
                paste(sapply(frtab / frtab[levels(z) == treated], function(x) {
                  plyr::round_any(x, accuracy = .001, f = floor)}), collapse = ", "),
                " which will select all units."),
         call. = FALSE)
  }
}

