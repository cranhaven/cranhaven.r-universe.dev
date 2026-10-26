# ==============================================================================
# Ecosystem Interoperability - cobalt, MatchIt, marginaleffects
# ==============================================================================

#' Convert couplr Result to matchit Object
#'
#' Constructs a \code{matchit}-class S3 object from a couplr result, enabling
#' use with any function that accepts \pkg{MatchIt} objects (e.g.,
#' \pkg{cobalt}, \pkg{marginaleffects}).
#'
#' @param result A couplr result object (matching_result, full_matching_result,
#'   cem_result, or subclass_result)
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param formula Optional formula used for matching. If not provided, a
#'   default formula is constructed from \code{result$info$vars}.
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#' @param estimand Target estimand stamped on the matchit object: one of
#'   \code{"ATT"}, \code{"ATC"} or \code{"ATE"}. \code{NULL} (default) reads it
#'   from the design, which every couplr front door records as
#'   \code{info$estimand}. MatchIt and marginaleffects read this field to pick
#'   the target population and the weighting of the effect estimate, so give it
#'   explicitly whenever the design does not determine it -- in particular when
#'   \code{left} holds the controls, which makes the design's left-focal
#'   weighting an ATC.
#' @param ... Additional arguments (ignored)
#'
#' @return An S3 object of class \code{"matchit"} with fields:
#' \describe{
#'   \item{match.matrix}{Match matrix (treated x controls)}
#'   \item{treat}{Named treatment vector (1/0)}
#'   \item{weights}{Matching weights}
#'   \item{X}{Covariate matrix}
#'   \item{call}{Original call}
#'   \item{info}{Metadata from couplr}
#' }
#'
#' @examples
#' \dontrun{
#' left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
#' right <- data.frame(id = 6:15, age = runif(10, 20, 70))
#' result <- match_couples(left, right, vars = "age")
#' mi <- as_matchit(result, left, right)
#' # Now use with cobalt:
#' cobalt::bal.tab(mi)
#' }
#'
#' @export
as_matchit <- function(result, left, right,
                       formula = NULL,
                       left_id = "id", right_id = "id",
                       estimand = NULL,
                       ...) {

  estimand <- .resolve_estimand(result, estimand)

  # Get match_data for weights and subclass
  md <- match_data(result, left, right, left_id = left_id,
                   right_id = right_id)

  if (nrow(md) == 0) {
    stop("No matched units to convert", call. = FALSE)
  }

  # Determine variable names
  vars <- result$info$vars
  if (is.null(vars)) {
    # Try to infer from data
    exclude <- c("id", left_id, right_id, "treatment", "weights",
                 "subclass", "distance")
    vars <- setdiff(names(md), exclude)
  }

  # Build formula if not provided
  if (is.null(formula)) {
    formula <- stats::as.formula(
      paste("treatment ~", paste(vars, collapse = " + "))
    )
  }

  # A matchit object holds one entry per unit, named by the unit. match_data()
  # holds one row per pair for pair designs, so a unit in several pairs (ratio
  # > 1, with replacement) has several rows, and the two sides may number their
  # units the same way. Units are therefore keyed by id, qualified by side
  # whenever an id occurs on both sides, and every per-unit field is read from
  # the unit's rows through that key.
  side <- ifelse(md$treatment == 1L, "left", "right")
  key <- .matchit_unit_keys(md$id, side)
  first <- !duplicated(key)
  unit_key <- key[first]
  unit_of_row <- factor(key, levels = unit_key)

  treat <- stats::setNames(md$treatment[first], unit_key)
  wts <- stats::setNames(
    as.numeric(tapply(md$weights, unit_of_row, sum)), unit_key)

  X_cols <- intersect(vars, names(md))
  X <- as.data.frame(md[first, X_cols, drop = FALSE])
  rownames(X) <- unit_key

  # A pair distance belongs to a unit only when the unit is in one pair.
  distance <- if ("distance" %in% names(md) && all(first)) {
    stats::setNames(md$distance, unit_key)
  } else {
    NULL
  }

  # Pair designs: one row per left unit, one column per partner slot.
  match_matrix <- NULL
  subclass <- NULL
  if (inherits(result, "matching_result")) {
    is_left <- side == "left"
    pair_left <- stats::setNames(key[is_left], md$subclass[is_left])
    pair_right <- stats::setNames(key[!is_left], md$subclass[!is_left])
    left_units <- unique(key[is_left])
    partners <- split(unname(pair_right[names(pair_left)]),
                      factor(unname(pair_left), levels = left_units))
    width <- max(lengths(partners))
    match_matrix <- matrix(
      unlist(lapply(partners, function(p) {
        c(p, rep(NA_character_, width - length(p)))
      })),
      nrow = length(left_units), byrow = TRUE,
      dimnames = list(left_units, NULL))

    # A matched set is a left unit with its partners. A right unit reused by
    # several left units belongs to no single set, so subclass is left out,
    # as MatchIt does for matching with replacement.
    set_of_row <- unname(pair_left[as.character(md$subclass)])
    sets_per_unit <- tapply(set_of_row, unit_of_row,
                            function(s) length(unique(s)))
    if (all(sets_per_unit == 1L)) {
      subclass <- stats::setNames(
        factor(set_of_row[first], levels = left_units), unit_key)
    }
  } else if ("subclass" %in% names(md)) {
    subclass <- stats::setNames(as.factor(md$subclass[first]), unit_key)
  }

  # Determine method label
  method_label <- if (inherits(result, "full_matching_result")) {
    "full"
  } else if (inherits(result, "cem_result")) {
    "cem"
  } else if (inherits(result, "subclass_result")) {
    "subclass"
  } else {
    "nearest"
  }

  structure(
    list(
      match.matrix = match_matrix,
      model = list(formula = formula),
      treat = treat,
      distance = distance,
      weights = wts,
      subclass = subclass,
      X = X,
      call = match.call(),
      info = list(
        method = method_label,
        source = "couplr",
        couplr_info = result$info
      ),
      nn = NULL,
      method = method_label,
      estimand = estimand,
      formula = formula
    ),
    class = "matchit"
  )
}

# Unit names for a matchit object: the id itself, or "left:<id>" and
# "right:<id>" when some id occurs on both sides, so that every name refers to
# one unit.
.matchit_unit_keys <- function(id, side) {
  id <- as.character(id)
  if (length(intersect(id[side == "left"], id[side == "right"])) > 0L) {
    paste0(side, ":", id)
  } else {
    id
  }
}

# The estimand a matchit object is labelled with. It comes from the design,
# which every front door records in info$estimand, and a caller can override it
# -- the design knows how it weights, not which side of the caller's data holds
# the treated units. There is no default: a guessed estimand propagates into the
# reported causal quantity rather than failing loudly.
.resolve_estimand <- function(result, estimand) {
  if (!is.null(estimand)) {
    estimand <- toupper(as.character(estimand))
    if (length(estimand) != 1L || !estimand %in% c("ATT", "ATE", "ATC")) {
      stop("estimand must be one of 'ATT', 'ATE', 'ATC'", call. = FALSE)
    }
    return(estimand)
  }

  est <- result$info$estimand
  if (is.null(est) || is.na(est)) {
    stop("This result carries no estimand, so there is nothing to label the ",
         "matchit object with. MatchIt and marginaleffects read that field to ",
         "choose the target population, so pass estimand = \"ATT\", \"ATC\" ",
         "or \"ATE\".", call. = FALSE)
  }

  dropped <- result$info$focal_discarded
  if (!is.null(dropped) && !is.na(dropped) && dropped > 0L) {
    warning(sprintf(
      paste0("The design did not retain %d of the focal (left) units, so ",
             "estimand = \"%s\" refers to the matched focal subset rather ",
             "than to all of them."),
      dropped, est), call. = FALSE)
  }

  est
}


# ==============================================================================
# cobalt bal.tab methods
# ==============================================================================

#' Balance Table for Matching Results (cobalt integration)
#'
#' S3 method enabling \code{cobalt::bal.tab()} on couplr result objects.
#' Requires the \pkg{cobalt} package to be installed.
#'
#' @param x A couplr result object
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param data Data frame used for subclassification (for subclass_result only)
#' @param ... Additional arguments. Arguments named in [as_matchit()]'s
#'   signature go to the conversion; the rest go to \code{cobalt::bal.tab()}.
#'
#' @return A cobalt balance table object
#'
#' @details
#' These methods convert couplr results to the format cobalt expects
#' (a matchit-class object) and then delegate to cobalt's own
#' \code{bal.tab.matchit()} method. The \pkg{cobalt} package must be
#' installed but is not required for couplr to function.
#'
#' @name bal.tab.matching_result
NULL

# One conversion for the three pair-shaped result classes. `...` carries
# arguments for two different functions, so it is split by whose formals name
# them: as_matchit() gets its own, cobalt::bal.tab() gets the rest. Forwarding
# the whole of `...` to both hands each function the other's arguments.
.bal_tab_via_matchit <- function(x, left, right, ...) {
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required for bal.tab(). Install with: install.packages('cobalt')",
         call. = FALSE)
  }
  dots <- list(...)
  own <- setdiff(names(formals(as_matchit)), c("result", "left", "right", "..."))
  to_matchit <- dots[intersect(names(dots), own)]
  to_cobalt <- dots[setdiff(names(dots), own)]

  mi <- do.call(as_matchit, c(list(x, left, right), to_matchit))
  do.call(cobalt::bal.tab, c(list(mi), to_cobalt))
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.matching_result <- function(x, left, right, ...) {
  .bal_tab_via_matchit(x, left, right, ...)
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.full_matching_result <- function(x, left, right, ...) {
  .bal_tab_via_matchit(x, left, right, ...)
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.cem_result <- function(x, left, right, ...) {
  .bal_tab_via_matchit(x, left, right, ...)
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.subclass_result <- function(x, data = NULL, ...) {
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required for bal.tab(). Install with: install.packages('cobalt')",
         call. = FALSE)
  }
  md <- match_data(x, data = data)
  treat_var <- if ("treatment" %in% names(md)) "treatment" else x$info$treatment
  cobalt::bal.tab(md, treat = treat_var, weights = "weights", ...)
}
