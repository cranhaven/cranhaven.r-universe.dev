# ==============================================================================
# Design paths: one matching per knob value, solved as one loop
# ==============================================================================
# A caliper is a modelling choice, and the way to make it is to look at what the
# matching does across a range of values. Doing that by calling match_couples()
# once per value solves the same problem from cold every time, and the values
# are not independent problems: ascending, each one is the last one with pairs
# added. Every arc the previous value placed is still an arc, every cost it
# reported is unchanged, and the matching it found is still a feasible matching.
#
# So a path point is the edge-generation loop run again over what the previous
# point left behind, with the new pairs supplied by the widening instead of by a
# pricing round. The state that carries -- the compiled problem, the flow, the
# candidate set, the structure the pairs are found through -- lives in C++ and
# stays there for the whole sweep (src/flow/flow_path.h).
#
# Direction is a correctness requirement rather than a preference. Widening adds
# pairs and leaves the incumbent matching feasible; narrowing withdraws pairs
# the matching may be standing on, which is a repair this loop does not do. A
# descending sweep is refused and told why rather than sorted behind the
# caller's back, because a caller who wrote one meant something else.

# The knobs a path can sweep. A knob is the name of the `match_couples()`
# argument the path moves, what a value of it means, and what makes a value
# legal; adding one is an entry here and an entry point beside
# `lap_match_path_lazy()`.
.path_knobs <- function() {
  list(
    max_distance = list(
      widens = "raising the cut admits pairs it used to forbid",
      check = function(values) {
        if (any(values <= 0)) {
          stop("`values` are distances under vary = \"max_distance\" and must ",
               "be positive, which is what `max_distance` itself takes.",
               call. = FALSE)
        }
      }
    )
  )
}

.check_path_values <- function(values, knob, vary) {
  if (!is.numeric(values) || length(values) == 0L) {
    stop("`values` must be a numeric vector with at least one value.",
         call. = FALSE)
  }
  if (anyNA(values)) {
    stop("`values` holds a missing value, and every point of a path is a ",
         "problem to solve.", call. = FALSE)
  }
  bad <- which(diff(values) <= 0)
  if (length(bad) > 0L) {
    k <- bad[1L]
    stop("`values` must ascend: value ", k + 1L, " is ", values[k + 1L],
         " and value ", k, " is ", values[k], ". Each point is solved from the ",
         "matching the point before it found, which holds only while every ",
         "value admits every pair the one before it admitted: for ", vary,
         ", ", knob$widens, ", and the matching stays feasible. A descending ",
         "sweep withdraws pairs the matching may be using, which is a ",
         "different question from the one `match_path()` answers.",
         call. = FALSE)
  }
  knob$check(values)
  invisible(TRUE)
}

#' Match across a range of one design choice
#'
#' Solves the matching once for each value of one argument and returns what
#' happened at each: how many units were matched, at what total distance, how
#' balanced the matched sample is, and the certificate saying the matching is
#' optimal for that value. The points are solved as one sequence rather than one
#' at a time, each starting from the matching the previous value found.
#'
#' `values` must ascend. Each point is solved from the point before it, which
#' works because a wider value only ever adds pairs to choose from; a descending
#' sweep takes pairs away from a matching already using them, and is refused
#' rather than reordered.
#'
#' @param left,right Data frames to match. Both are required.
#' @param vars Character vector of matching variables, present in both.
#' @param vary The argument to sweep. `"max_distance"` sweeps the distance cut.
#' @param values The values to sweep it through, ascending.
#' @param distance Distance metric, as in [match_couples()].
#' @param weights,scale,auto_scale,sigma Distance construction, as in
#'   [match_couples()].
#' @param calipers Per-variable calipers held fixed across the path, as in
#'   [match_couples()].
#' Balance is read on the caller's own variables rather than on the coordinates
#' the solver worked in, so a scaled or weighted distance does not change the
#' scale the balance is reported on. It is the same reading [balance_diagnostics()]
#' gives for a single matching, taken at every point of the sweep.
#'
#' @param certify Whether each point carries a checked certificate. `TRUE` by
#'   default: the certificate is what says a point's matching is the optimal one
#'   for its value, which is the claim a path is read for.
#' @param left_id,right_id Name of the column holding the unit identifier, or
#'   NULL (default) to use a column called `id`, then meaningful row names,
#'   then synthesized ids with a warning. See [match_couples()].
#' @param keep_per_row,width,tol,max_rounds The edge-generation loop's search
#'   knobs, shared with `memory_mode = "implicit"`. Each point converges on any
#'   of them. `width` is the columns the seed gives a row, and 0, the default,
#'   sizes it from the number of columns; `$search$seed_width` reports what the
#'   path used.
#'
#' @return An object of class `couplr_path`: `$path`, one row per point,
#'   `$balance`, one row per point per variable, and the match vector,
#'   certificate, round record and Hall witness for each of them.
#'
#' @examples
#' set.seed(1)
#' left <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20))
#' right <- data.frame(id = 1:60, x = rnorm(60), y = rnorm(60))
#' path <- match_path(left, right, vars = c("x", "y"),
#'                    vary = "max_distance", values = c(0.5, 1, 2, Inf))
#' path$path
#' path$balance
#'
#' @export
match_path <- function(left, right, vars,
                       left_id = NULL,
                       right_id = NULL,
                       vary = "max_distance",
                       values,
                       distance = "euclidean",
                       weights = NULL,
                       scale = FALSE,
                       auto_scale = FALSE,
                       calipers = NULL,
                       sigma = NULL,
                       certify = TRUE,
                       keep_per_row = .implicit_defaults()$keep_per_row,
                       width = .implicit_defaults()$width,
                       tol = .implicit_defaults()$tol,
                       max_rounds = .implicit_defaults()$max_rounds) {

  knobs <- .path_knobs()
  if (!is.character(vary) || length(vary) != 1L || is.na(vary) ||
      !vary %in% names(knobs)) {
    stop("`vary` must be one of ", paste0("\"", names(knobs), "\"",
                                          collapse = ", "),
         ", and names the argument the path sweeps.", call. = FALSE)
  }
  knob <- knobs[[vary]]
  if (missing(values)) {
    stop("`values` names the ", vary, " values the path is over, and there is ",
         "no default set worth guessing at.", call. = FALSE)
  }
  .check_path_values(values, knob, vary)

  if (missing(right) || is.null(right) || missing(vars) || is.null(vars)) {
    stop("`match_path()` takes two data frames and the variables to match on.",
         call. = FALSE)
  }
  if (!is.logical(certify) || length(certify) != 1L || is.na(certify)) {
    stop("`certify` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    stop("`tol` must be a single non-negative number.", call. = FALSE)
  }
  .check_positive_count(keep_per_row, "keep_per_row")
  .check_seed_width(width)
  .check_positive_count(max_rounds, "max_rounds")

  if (auto_scale) {
    preproc <- preprocess_matching_vars(
      left, right, vars,
      auto_scale = TRUE,
      scale_method = if (identical(scale, FALSE)) "auto" else scale,
      check_health = TRUE,
      remove_problematic = TRUE,
      verbose = TRUE
    )
    vars <- preproc$vars
    if (preproc$scaling_method != "none") scale <- preproc$scaling_method
  }

  validate_matching_inputs(left, right, vars)
  weights <- validate_weights(weights, vars)
  calipers <- validate_calipers(calipers, vars)

  left_ids <- extract_ids(left, "left", left_id, warn_synthetic = TRUE)
  right_ids <- extract_ids(right, "right", right_id, warn_synthetic = TRUE)

  # The path states its problem the way the loop does: one specification of the
  # complete problem, and the pairs generated from it. The knob the path sweeps
  # is left off the specification and supplied per point.
  spec <- build_cost_matrix(left, right, vars, distance, weights, scale,
                            sigma = sigma, memory_mode = "implicit")
  spec <- apply_all_constraints(spec, left, right, vars,
                                max_distance = Inf, calipers = calipers)

  # The loop matches every row, so the short side has to be the rows. On a
  # specification this is a field swap; the match vectors and the certificate
  # come back in the caller's orientation below.
  transposed <- spec$n_left > spec$n_right
  work <- if (transposed) transpose_lazy_cost_spec(spec) else spec

  raw <- lap_match_path_lazy(
    work$left_mat, work$right_mat, work$distance,
    lazy_cost_spec_inv_cov(work), as.numeric(values),
    lazy_cost_spec_calipers(work),
    maximize = FALSE, keep_per_row = keep_per_row, width = width,
    tol = tol, max_rounds = max_rounds, certify = certify
  )

  .new_couplr_path(raw, vary = vary, spec = spec, transposed = transposed,
                   tol = tol, left_ids = left_ids, right_ids = right_ids,
                   left = left, right = right, vars = vars)
}

# What a point's matching did to the covariates, one row per point per variable.
#
# The values read are the caller's own rather than the coordinates the solver
# worked in: scaling and weights are how the distance was built, and a caller
# comparing a point against the unmatched sample is comparing on the scale the
# variables arrived on. That is the same choice `balance_diagnostics()` makes,
# and the per-variable statistics come from the same function, so a point of a
# path and a single matching are balanced the same way.
.path_balance <- function(match_out, left, right, vars) {
  lapply(match_out, function(m) {
    rows <- which(!is.na(m) & m > 0L)
    cols <- m[rows]

    # A point that matched nothing still gets a row per variable, because a
    # path is read as a sequence and a gap in it is part of the reading.
    # `calculate_var_balance()` answers an empty sample with NA throughout.
    tbl <- dplyr::bind_rows(lapply(vars, function(v) {
      tibble::as_tibble(
        calculate_var_balance(left[[v]][rows], right[[v]][cols], v))
    }))
    list(var_stats = tbl, overall = .overall_balance(tbl, length(vars)))
  })
}

# Read the sweep back in the caller's orientation and vocabulary. A point's
# answer is a matching over the caller's left units, its cost is a total
# distance, and its proof is an assignment certificate.
.new_couplr_path <- function(raw, vary, spec, transposed, tol,
                             left_ids, right_ids, left, right, vars) {
  n_points <- length(raw$match)
  n_left <- spec$n_left

  match_out <- lapply(raw$match, function(m) {
    m <- as.integer(m)
    if (transposed) .certify_invert_match(m, n_left) else m
  })

  certificate <- lapply(raw$certificate, function(cert) {
    if (is.null(cert)) NULL else .new_assignment_certificate(
      cert, transposed = transposed, tol = tol)
  })

  witness <- lapply(seq_len(n_points), function(k) {
    w <- raw$witness[[k]]
    if (is.null(w)) return(NULL)
    w$certified <- raw$witness_certified[[k]]
    w$transposed <- transposed
    w
  })

  pts <- raw$points
  status <- as.character(pts$status)
  n_matched <- as.integer(pts$n_matched)
  total <- as.numeric(pts$total_cost)

  # A value too tight for a complete matching still admits a largest one, and
  # a single call at that value returns it, so the point does too: the
  # one-to-one design solved by the design loop at that cut. The witness saying
  # why no complete matching exists stays beside it.
  for (k in which(status == "infeasible")) {
    at <- spec
    at$max_distance <- as.numeric(pts$value)[k]
    partial <- suppressWarnings(.lazy_partial_matching(at))
    rows <- partial$matched_rows
    m <- integer(n_left)
    m[rows] <- partial$matched_cols
    match_out[[k]] <- m
    n_matched[k] <- length(rows)
    if (length(rows)) {
      status[k] <- "partial"
      total[k] <- sum(lazy_pair_distances(spec, rows, partial$matched_cols))
    }
  }

  # A point that found no matching has no total to report. Zero is what summing
  # nothing gives and it plots as the cheapest point on the path, which is the
  # opposite of what a caliper too tight to match under means.
  total[!(status %in% c("optimal", "partial"))] <- NA_real_

  balance <- .path_balance(match_out, left, right, vars)

  path <- tibble::tibble(
    value             = as.numeric(pts$value),
    status            = status,
    n_matched         = n_matched,
    total_distance    = total,
    mean_abs_std_diff = vapply(balance, function(b) b$overall$mean_abs_std_diff,
                               numeric(1)),
    max_abs_std_diff  = vapply(balance, function(b) b$overall$max_abs_std_diff,
                               numeric(1)),
    certified         = as.logical(pts$certified),
    seconds           = as.numeric(pts$seconds),
    n_rounds          = as.integer(pts$n_rounds),
    candidate_edges   = as.numeric(pts$candidate_edges),
    pairs_added       = as.numeric(pts$pairs_added),
    edges_evaluated   = as.numeric(pts$edges_evaluated)
  )
  names(path)[1] <- vary

  # The per-variable reading behind those two columns, as one table rather than
  # one per point, because what it is read for is a variable's balance across
  # the sweep.
  balance_tbl <- dplyr::bind_rows(
    lapply(seq_len(n_points), function(k) {
      tbl <- balance[[k]]$var_stats
      tbl$value <- as.numeric(pts$value)[k]
      tbl[, c("value", setdiff(names(tbl), "value")), drop = FALSE]
    })
  )
  names(balance_tbl)[1] <- vary

  structure(
    list(
      path        = path,
      balance     = balance_tbl,
      vary        = vary,
      match       = match_out,
      certificate = certificate,
      rounds      = lapply(raw$rounds, tibble::as_tibble),
      witness     = witness,
      search      = list(
        seed_width      = as.integer(raw$seed_width),
        possible_edges  = as.numeric(raw$possible_edges),
        candidate_edges = as.numeric(raw$candidate_edges),
        edges_evaluated = as.numeric(raw$edges_evaluated)
      ),
      ids         = list(left = left_ids, right = right_ids),
      transposed  = transposed
    ),
    class = "couplr_path"
  )
}

#' Print a design path
#'
#' Shows how many pairs the whole sweep generated out of how many exist, and
#' then the point table: one row per value, with the matching it found and what
#' finding it cost.
#'
#' @param x A `couplr_path` from [match_path()].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.couplr_path <- function(x, ...) {
  cat("<couplr_path> ", nrow(x$path), " points over ", x$vary, "\n", sep = "")
  cat("  ", format(x$search$candidate_edges, big.mark = ","), " of ",
      format(x$search$possible_edges, big.mark = ","),
      " pairs generated, ", format(x$search$edges_evaluated, big.mark = ","),
      " evaluated\n", sep = "")
  print(x$path)
  invisible(x)
}
