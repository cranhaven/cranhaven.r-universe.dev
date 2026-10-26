# ==============================================================================
# Coarsened Exact Matching (CEM)
# ==============================================================================

#' Coarsened Exact Matching
#'
#' Coarsens continuous variables into bins, then performs exact matching on
#' the coarsened values. Units in strata containing both left and right units
#' are kept; others are pruned. Matched units receive weights inversely
#' proportional to stratum sizes to maintain balance.
#'
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param vars Character vector of variable names to coarsen and match on
#' @param cutpoints Named list of break vectors per variable. If NULL,
#'   automatic binning is used.
#' @param n_bins Binning method when \code{cutpoints} is NULL: \code{"sturges"}
#'   (default), \code{"fd"} (Freedman-Diaconis), \code{"scott"}, or an integer
#'   specifying the number of bins for all variables.
#' @param grouping Character vector of variable names to match exactly
#'   (without coarsening). These are typically categorical variables.
#' @param keep Which units to return: \code{"all"} (default) returns all units
#'   with weight 0 for unmatched, \code{"matched"} drops unmatched units.
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#'
#' @return An S3 object of class \code{c("cem_result", "couplr_result")}
#'   containing:
#' \describe{
#'   \item{matched}{Tibble with columns \code{id}, \code{side}, \code{stratum},
#'     \code{weight}}
#'   \item{strata_summary}{Tibble with per-stratum counts}
#'   \item{info}{List with \code{n_strata}, \code{n_matched_left},
#'     \code{n_matched_right}, \code{n_pruned_left}, \code{n_pruned_right},
#'     \code{method}, \code{vars}}
#' }
#'
#' @details
#' CEM algorithm:
#' \enumerate{
#'   \item Coarsen each numeric variable using \code{\link[base]{cut}} with
#'     either user-specified breakpoints or automatic binning (Sturges, FD, or
#'     Scott rule)
#'   \item Categorical variables in \code{grouping} are kept as-is
#'   \item Create strata by concatenating all coarsened values
#'   \item Drop strata with 0 left or 0 right units
#'   \item Compute CEM weights: left units get weight 1, right units get
#'     weight \code{n_left_in_stratum / n_right_in_stratum} so that the total
#'     weight of right units in each stratum equals the number of left units
#' }
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(
#'   id = 1:20, age = rnorm(20, 40, 10),
#'   income = rnorm(20, 50000, 10000)
#' )
#' right <- data.frame(
#'   id = 21:60, age = rnorm(40, 42, 10),
#'   income = rnorm(40, 52000, 10000)
#' )
#' result <- cem_match(left, right, vars = c("age", "income"))
#' print(result)
#'
#' @export
cem_match <- function(left, right, vars,
                      cutpoints = NULL,
                      n_bins = "sturges",
                      grouping = NULL,
                      keep = "all",
                      left_id = "id",
                      right_id = "id") {

  # --- Input validation ---
  if (!is.data.frame(left) || !is.data.frame(right)) {
    stop("left and right must be data frames", call. = FALSE)
  }
  if (is.null(vars) || length(vars) == 0) {
    stop("vars must be specified", call. = FALSE)
  }
  if (!left_id %in% names(left)) {
    stop(sprintf("left_id column '%s' not found in left data", left_id),
         call. = FALSE)
  }
  if (!right_id %in% names(right)) {
    stop(sprintf("right_id column '%s' not found in right data", right_id),
         call. = FALSE)
  }
  if (!keep %in% c("all", "matched")) {
    stop("keep must be 'all' or 'matched'", call. = FALSE)
  }

  all_vars <- c(vars, grouping)
  missing_left <- setdiff(all_vars, names(left))
  missing_right <- setdiff(all_vars, names(right))
  if (length(missing_left) > 0) {
    stop("Variables not found in left: ", paste(missing_left, collapse = ", "),
         call. = FALSE)
  }
  if (length(missing_right) > 0) {
    stop("Variables not found in right: ", paste(missing_right, collapse = ", "),
         call. = FALSE)
  }

  # --- Coarsen variables ---
  l_ids <- as.character(left[[left_id]])
  r_ids <- as.character(right[[right_id]])
  n_left <- nrow(left)
  n_right <- nrow(right)

  # Build stratum labels for each unit
  left_strata_parts <- list()
  right_strata_parts <- list()

  for (v in vars) {
    lv <- left[[v]]
    rv <- right[[v]]
    combined <- c(lv, rv)

    if (is.numeric(combined)) {
      # Determine breaks
      if (!is.null(cutpoints) && v %in% names(cutpoints)) {
        breaks <- cutpoints[[v]]
        # Ensure breaks cover the range
        breaks <- sort(unique(c(-Inf, breaks, Inf)))
      } else {
        breaks <- .auto_breaks(combined, n_bins)
      }
      left_strata_parts[[v]] <- as.character(cut(lv, breaks = breaks,
                                                  include.lowest = TRUE))
      right_strata_parts[[v]] <- as.character(cut(rv, breaks = breaks,
                                                   include.lowest = TRUE))
    } else {
      # Categorical: use as-is
      left_strata_parts[[v]] <- as.character(lv)
      right_strata_parts[[v]] <- as.character(rv)
    }
  }

  # Exact-match grouping variables
  for (v in grouping) {
    left_strata_parts[[v]] <- as.character(left[[v]])
    right_strata_parts[[v]] <- as.character(right[[v]])
  }

  # Concatenate to form stratum labels
  left_strata <- do.call(paste, c(left_strata_parts, sep = "|"))
  right_strata <- do.call(paste, c(right_strata_parts, sep = "|"))

  # --- Find common strata ---
  left_tbl <- data.frame(id = l_ids, side = "left", stratum = left_strata,
                         stringsAsFactors = FALSE)
  right_tbl <- data.frame(id = r_ids, side = "right", stratum = right_strata,
                          stringsAsFactors = FALSE)
  all_tbl <- rbind(left_tbl, right_tbl)

  # Count per stratum per side
  strata_counts <- stats::aggregate(
    id ~ stratum + side, data = all_tbl, FUN = length
  )
  names(strata_counts)[3] <- "n"

  # Pivot to wide: stratum, n_left, n_right
  left_counts <- strata_counts[strata_counts$side == "left",
                               c("stratum", "n")]
  names(left_counts)[2] <- "n_left"
  right_counts <- strata_counts[strata_counts$side == "right",
                                c("stratum", "n")]
  names(right_counts)[2] <- "n_right"

  strata_summary <- merge(left_counts, right_counts, by = "stratum",
                          all = TRUE)
  strata_summary$n_left[is.na(strata_summary$n_left)] <- 0L
  strata_summary$n_right[is.na(strata_summary$n_right)] <- 0L

  # Keep strata with both sides present
  matched_strata <- strata_summary$stratum[strata_summary$n_left > 0 &
                                            strata_summary$n_right > 0]

  # --- Compute weights ---
  # Build lookup: stratum -> n_left, n_right
  strata_info <- strata_summary[strata_summary$stratum %in% matched_strata, ]
  rownames(strata_info) <- strata_info$stratum

  left_matched <- left_strata %in% matched_strata
  right_matched <- right_strata %in% matched_strata

  # Build matched tibble
  rows <- list()

  # Left units
  for (i in seq_len(n_left)) {
    s <- left_strata[i]
    if (s %in% matched_strata) {
      rows[[length(rows) + 1]] <- data.frame(
        id = l_ids[i], side = "left", stratum = s, weight = 1.0,
        stringsAsFactors = FALSE
      )
    } else if (keep == "all") {
      rows[[length(rows) + 1]] <- data.frame(
        id = l_ids[i], side = "left", stratum = s, weight = 0.0,
        stringsAsFactors = FALSE
      )
    }
  }

  # Right units
  for (i in seq_len(n_right)) {
    s <- right_strata[i]
    if (s %in% matched_strata) {
      si <- strata_info[s, ]
      w <- si$n_left / si$n_right
      rows[[length(rows) + 1]] <- data.frame(
        id = r_ids[i], side = "right", stratum = s, weight = w,
        stringsAsFactors = FALSE
      )
    } else if (keep == "all") {
      rows[[length(rows) + 1]] <- data.frame(
        id = r_ids[i], side = "right", stratum = s, weight = 0.0,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) > 0) {
    matched_tbl <- tibble::as_tibble(dplyr::bind_rows(rows))
  } else {
    matched_tbl <- tibble::tibble(
      id = character(0), side = character(0),
      stratum = character(0), weight = numeric(0)
    )
  }

  strata_summary_tbl <- tibble::as_tibble(strata_summary)
  strata_summary_tbl$matched <- strata_summary_tbl$stratum %in% matched_strata

  info <- list(
    n_strata = nrow(strata_summary),
    n_matched_strata = length(matched_strata),
    n_matched_left = sum(left_matched),
    n_matched_right = sum(right_matched),
    n_pruned_left = sum(!left_matched),
    n_pruned_right = sum(!right_matched),
    method = "cem",
    vars = vars,
    grouping = grouping
  )
  info <- c(info, design_estimand(n_left, info$n_matched_left))

  result <- list(
    matched = matched_tbl,
    strata_summary = strata_summary_tbl,
    info = info
  )

  structure(result, class = c("cem_result", "couplr_result"))
}


#' Compute automatic break points for CEM binning
#'
#' @param x Combined numeric vector from left and right
#' @param n_bins Binning method or integer
#' @return Numeric vector of break points
#' @keywords internal
.auto_breaks <- function(x, n_bins) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(c(-Inf, Inf))

  if (is.numeric(n_bins) && length(n_bins) == 1) {
    n <- as.integer(n_bins)
    breaks <- seq(min(x), max(x), length.out = n + 1)
  } else {
    method <- tolower(as.character(n_bins)[1])
    h <- switch(method,
      sturges = graphics::hist(x, breaks = "Sturges", plot = FALSE),
      fd = , freedman = , `freedman-diaconis` =
        graphics::hist(x, breaks = "FD", plot = FALSE),
      scott = graphics::hist(x, breaks = "Scott", plot = FALSE),
      graphics::hist(x, breaks = "Sturges", plot = FALSE)
    )
    breaks <- h$breaks
  }

  # Ensure -Inf and Inf are included
  c(-Inf, breaks[breaks > min(x) & breaks < max(x)], Inf)
}


#' Print Method for CEM Results
#'
#' @param x A cem_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.cem_result <- function(x, ...) {
  cat("\nCoarsened Exact Matching Result\n")
  cat("===============================\n\n")
  cat(sprintf("  Strata: %d total, %d matched\n",
              x$info$n_strata, x$info$n_matched_strata))
  cat(sprintf("  Left:   %d matched, %d pruned\n",
              x$info$n_matched_left, x$info$n_pruned_left))
  cat(sprintf("  Right:  %d matched, %d pruned\n",
              x$info$n_matched_right, x$info$n_pruned_right))
  cat(sprintf("  Variables: %s\n", paste(x$info$vars, collapse = ", ")))
  if (length(x$info$grouping) > 0) {
    cat(sprintf("  Exact grouping: %s\n",
                paste(x$info$grouping, collapse = ", ")))
  }
  cat("\n")
  invisible(x)
}
