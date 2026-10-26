# ==============================================================================
# Full Matching - Variable-ratio group matching
# ==============================================================================
# The optimal route compiles the design into the package's flow model and
# solves it there. The network is the one src/flow/flow_compile.h describes:
# at min_controls == 1 both sides carry a lower bound of one and the pair arcs
# unit capacity, so the arcs a solve places are an edge cover of the admissible
# pairs; a cheapest cover is minimal and a minimal cover is a disjoint union of
# stars, which is exactly a full matching. Above one, a group centred on a
# single right unit cannot meet the bound, so every group is one-to-many: the
# auxiliary source feeds each centre through an arc bounded by [min_controls,
# max_controls], every unit passes one unit of flow to the sink, and the centres
# are the left units, so the bounds count right units whichever side is larger.
#
# Solving there is what makes the node potentials and the optimality
# certificate available: both are properties of the flow, and the group
# memberships alone cannot reconstruct either.

# Full matching with both group shapes admissible. The solve returns an edge
# cover, and a cheapest cover is minimal, but a zero-cost arc can survive in a
# cover that is not: dropping any arc whose two ends are both met by another
# arc leaves every unit covered and costs nothing, and repeating it until none
# is left makes every component a star. The stars are the groups.
.full_match_groups_symmetric <- function(compiled, flow) {
  block <- compiled$block
  placed <- flow[seq_len(block$n_arcs) + block$first_arc - 1] > 0
  li <- block$row[placed]
  rj <- block$col[placed]

  n_left <- as.integer(compiled$shape$n_centres)
  n_right <- as.integer(compiled$shape$n_units)

  repeat {
    dl <- tabulate(li, nbins = n_left)
    dr <- tabulate(rj, nbins = n_right)
    slack <- which(dl[li] > 1L & dr[rj] > 1L)
    if (!length(slack)) break
    # One at a time: dropping two arcs that share an end could uncover it.
    drop <- slack[1L]
    li <- li[-drop]
    rj <- rj[-drop]
  }

  # Components of a union of stars: every arc joins a leaf to its centre, so
  # labels propagate in one pass per side until they stop moving.
  gl <- integer(n_left)
  gr <- integer(n_right)
  g <- 0L
  for (k in seq_along(li)) {
    a <- li[k]; b <- rj[k]
    if (gl[a] == 0L && gr[b] == 0L) {
      g <- g + 1L
      gl[a] <- g; gr[b] <- g
    } else if (gl[a] == 0L) {
      gl[a] <- gr[b]
    } else if (gr[b] == 0L) {
      gr[b] <- gl[a]
    } else if (gl[a] != gr[b]) {
      # Two stars joined by one arc is a path, which the pruning above removes,
      # so reaching here means the cover was not minimal.
      stop("full matching produced a component that is not a star", call. = FALSE)
    }
  }

  list(group_of_left = gl, group_of_right = gr, n_groups = g)
}

# Read a solved flow back as group memberships in the caller's left/right
# terms. Above a lower bound of one, a centre holding fewer than min_controls
# units did not meet the bound on its own arc, so it holds no group and its
# units stay unmatched.
.full_match_groups <- function(compiled, flow, min_controls) {
  if (isTRUE(compiled$shape$symmetric)) {
    return(.full_match_groups_symmetric(compiled, flow))
  }
  n_centres <- as.integer(compiled$shape$n_centres)
  n_units <- as.integer(compiled$shape$n_units)

  block <- compiled$block
  placed <- flow[seq_len(block$n_arcs) + block$first_arc - 1] > 0
  centre_of_unit <- integer(n_units)
  centre_of_unit[block$col[placed]] <- block$row[placed]

  held <- tabulate(block$row[placed], nbins = n_centres) >= min_controls
  centre_group <- integer(n_centres)
  centre_group[held] <- seq_len(sum(held))

  group_of_unit <- integer(n_units)
  in_group <- centre_of_unit > 0L
  group_of_unit[in_group] <- centre_group[centre_of_unit[in_group]]

  list(group_of_left = centre_group, group_of_right = group_of_unit,
       n_groups = sum(held))
}

# Node potentials in the caller's left/right terms. They are one representative
# of the dual, in the gauge the solver fixes: the potential at the auxiliary
# source is zero.
.full_match_potentials <- function(compiled, potential) {
  layout <- compiled$layout
  rows <- potential[layout$row_base + seq_len(layout$n_rows) - 1L]
  cols <- potential[layout$col_base + seq_len(layout$n_cols) - 1L]
  list(left = rows, right = cols)
}

# What the solve terminated on, with one reading applied to it. A shortfall
# leaves centres below min_controls, and those groups dissolve; a partial
# matching that lost every one of them has nothing left to report but
# infeasibility. Bounds the compiler rejects never reach a solver at all.
.full_match_status <- function(bounds_feasible, solver_status, n_groups) {
  if (!isTRUE(bounds_feasible)) {
    return(.validate_status("infeasible"))
  }
  status <- .validate_status(solver_status)
  if (!identical(status, "optimal") && n_groups == 0L) {
    return(.validate_status("infeasible"))
  }
  status
}

#' Full Matching
#'
#' Assigns every unit (left and right) to a matched group with variable ratios
#' (1:k or k:1). Unlike 1:1 matching, full matching does not discard units,
#' producing matched groups where each group contains at least one left and one
#' right unit.
#'
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param vars Character vector of variable names to match on
#' @param distance Distance metric: \code{"euclidean"} (default),
#'   \code{"mahalanobis"}, \code{"manhattan"}, or a custom function
#' @param min_controls Minimum number of right units per group (default: 1)
#' @param max_controls Maximum number of right units per group (default: Inf)
#' @param caliper Maximum allowable distance for a match. Units with no
#'   eligible partner within the caliper are left unmatched.
#' @param caliper_sd If not NULL, caliper is expressed in standard deviations
#'   of the pooled distance distribution rather than absolute units.
#' @param weights Named numeric vector of variable weights
#' @param scale Scaling method: \code{FALSE} (default), \code{"robust"},
#'   \code{"standardize"}, or \code{"range"}
#' @param auto_scale If TRUE, automatically preprocess and scale variables
#' @param sigma Optional covariance matrix for Mahalanobis distance
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#' @param method Matching algorithm: \code{"optimal"} (default) uses min-cost
#'   max-flow to find the globally optimal group assignment minimizing total
#'   distance; \code{"greedy"} uses a fast two-pass heuristic.
#' @param memory_mode One of "auto" (default), "dense" or "implicit". "auto"
#'   warns if the dense cost matrix would consume a large fraction of free
#'   system RAM. "dense" skips the RAM check entirely. "implicit" solves
#'   \code{method = "optimal"} without building
#'   the pair set: the flow is solved over a growing subset of pairs, the pairs
#'   it omits are priced against the flow's node potentials, and the subset
#'   grows until none prices below zero, so the groups are optimal over every
#'   pair; a \code{search} element records what that cost. Under "implicit"
#'   \code{caliper_sd} reads every pair's distance once to take its standard
#'   deviation, holding two running sums rather than the distances. "lazy" is
#'   not available here: a flow solved over every pair holds every pair.
#'
#' @return An S3 object of class \code{c("full_matching_result", "couplr_result")}
#'   containing:
#' \describe{
#'   \item{groups}{Tibble with columns \code{group_id}, \code{id}, \code{side}
#'     (\code{"left"}/\code{"right"}), and \code{weight}}
#'   \item{status}{What the solver terminated on, one of
#'     \code{"optimal"} (every unit placed in a group meeting
#'     \code{min_controls}, at minimum total distance),
#'     \code{"partial"} (groups formed, some units left over, under a
#'     maximum-cardinality-then-minimum-cost objective),
#'     \code{"infeasible"} (no group meets the requested bounds), or
#'     \code{"heuristic"} (\code{method = "greedy"}, which neither claims nor
#'     checks optimality). See \code{\link{solver_status_values}}.}
#'   \item{info}{List with \code{n_groups}, \code{n_left}, \code{n_right},
#'     \code{n_unmatched_left}, \code{n_unmatched_right}, \code{method},
#'     \code{vars}}
#'   \item{unmatched}{Left and right IDs that no group contains. Every unit is
#'     either a row of \code{groups} or an entry here.}
#'   \item{potentials}{Node potentials from the flow solve, a list with elements
#'     \code{left} and \code{right} holding one value per unit. They are one
#'     representative of the dual, in the gauge the solver fixes. Present for
#'     \code{method = "optimal"} only.}
#'   \item{certificate}{A \code{flow_certificate} from
#'     \code{\link{verify_flow}}, checking the solved flow and its potentials
#'     against the optimality conditions. \code{status} says what the solver
#'     terminated on; this says what was checked. Present for
#'     \code{method = "optimal"} only. Under \code{memory_mode = "implicit"}
#'     the check covers the pairs the flow was solved over, and
#'     \code{omitted_proven_floor} bounds the reduced cost of every pair it
#'     omitted; \code{certified_optimal} holds both together.}
#'   \item{search}{Under \code{memory_mode = "implicit"}: the seed width, the
#'     pairs generated (\code{candidate_edges}) against the pairs there are
#'     (\code{possible_edges}), the distances computed
#'     (\code{edges_evaluated}) and one row per round.}
#' }
#'
#' @details
#' `full_match()` builds matched groups of variable size. Under the default
#' `min_controls = 1` it solves full matching in the sense of Hansen and
#' Klopfer (2006): a group holds either one left unit and several right ones or
#' one right unit and several left ones, and both shapes may appear in the same
#' solution.
#' `max_controls` bounds the many side there, whichever side that is.
#'
#' A lower bound above one admits only the one-to-many shape, because a group
#' built around a single right unit holds exactly one of them and cannot meet a
#' lower bound of two. Every group is then one left unit with between
#' `min_controls` and `max_controls` right ones. Both bounds count right units
#' whichever side holds more, so an instance with too few right units to give
#' every left unit that many is refused as `"infeasible"` rather than answered
#' with groups counted the other way round.
#'
#' Two algorithms are available:
#'
#' \strong{Optimal} (\code{method = "optimal"}, default): Solves a min-cost
#' max-flow problem that minimizes total distance across all group assignments
#' simultaneously, with the optimum found via Dijkstra's algorithm with Johnson
#' potentials. At \code{min_controls = 1} the network is an edge cover over the
#' admissible pairs, which is what lets a group be centred on either side; above
#' one it is one centre per group and the centres are the left units.
#'
#' \strong{Greedy} (\code{method = "greedy"}): A fast two-pass heuristic:
#' \enumerate{
#'   \item Each left unit picks its nearest eligible right unit
#'   \item Remaining right units are assigned to their nearest already-matched
#'     left unit, respecting \code{max_controls}
#' }
#' This is faster but does not guarantee globally optimal results.
#'
#' Every left unit carries weight 1, and the right units of a group share a
#' total weight equal to the number of left units in that group, so the two
#' sides of a group weigh the same. A group holding one left unit and k right
#' units therefore gives each right unit \code{1/k}; a group holding k left
#' units and one right unit gives that right unit \code{k}. \code{"greedy"}
#' builds one left unit per group and produces only the first shape.
#' \code{"optimal"} at \code{min_controls = 1} centres a group on whichever
#' side is larger, so both arise; above one the centres are the left units and
#' the first shape is the only one.
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
#' right <- data.frame(id = 6:20, age = runif(15, 20, 70))
#' result <- full_match(left, right, vars = "age")
#' print(result)
#'
#' @export
full_match <- function(left, right, vars,
                       distance = "euclidean",
                       min_controls = 1,
                       max_controls = Inf,
                       caliper = NULL,
                       caliper_sd = NULL,
                       weights = NULL,
                       scale = FALSE,
                       auto_scale = FALSE,
                       sigma = NULL,
                       left_id = "id",
                       right_id = "id",
                       method = "optimal",
                       memory_mode = "auto") {

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

  method <- match.arg(method, c("optimal", "greedy"))

  min_controls <- as.integer(min_controls)
  if (is.na(min_controls) || min_controls < 1) {
    stop("min_controls must be a positive integer", call. = FALSE)
  }
  if (!is.infinite(max_controls)) {
    max_controls <- as.integer(max_controls)
    if (is.na(max_controls) || max_controls < min_controls) {
      stop("max_controls must be >= min_controls", call. = FALSE)
    }
  }

  # --- Preprocessing ---
  if (auto_scale) {
    preproc <- preprocess_matching_vars(
      left, right, vars,
      auto_scale = TRUE,
      scale_method = if (identical(scale, FALSE)) "auto" else scale,
      check_health = TRUE, remove_problematic = TRUE, verbose = TRUE
    )
    vars <- preproc$vars
    if (preproc$scaling_method != "none") {
      scale <- preproc$scaling_method
    }
  }

  validate_matching_inputs(left, right, vars)
  weights <- validate_weights(weights, vars)

  # Extract IDs
  l_ids <- as.character(left[[left_id]])
  r_ids <- as.character(right[[right_id]])

  # --- Distance matrix ---
  # "lazy" is refused here: a flow solved over every pair holds every pair.
  # "implicit" states the problem as a specification the design loop solves
  # without building it, which only the optimal flow can consume.
  cost_matrix <- build_cost_matrix(left, right, vars, distance, weights, scale,
                                   sigma = sigma, memory_mode = memory_mode,
                                   caller_supports_lazy = FALSE,
                                   caller_supports_implicit =
                                     identical(method, "optimal"))
  implicit <- is_lazy_cost_spec(cost_matrix)

  # The reduction to an edge cover needs costs that do not reward extra
  # arcs: a cheapest cover is inclusion-minimal only when no arc is worth
  # keeping for its own sake. Under a negative cost the cheapest cover takes
  # every negative arc and the minimality prune then removes arcs the
  # objective wanted, so the answer is not the cheapest full matching. Every
  # built-in metric is non-negative; a custom distance function need not be.
  # A matrix is checked here. A lazy specification, which can carry a
  # user-supplied function, is checked in the implicit solve
  # (src/flow/flow_rcpp.cpp), where its distances are first evaluated.
  if (!implicit) {
    finite_costs <- cost_matrix[is.finite(cost_matrix)]
    if (length(finite_costs) && min(finite_costs) < 0) {
      stop("full_match() needs non-negative distances: the cheapest edge ",
           "cover is a full matching only when no arc is worth keeping for ",
           "its own sake. The smallest distance here is ",
           format(min(finite_costs)), ". Shift the custom distance so its ",
           "smallest value is zero.", call. = FALSE)
    }
  }

  n_left <- if (implicit) cost_matrix$n_left else nrow(cost_matrix)
  n_right <- if (implicit) cost_matrix$n_right else ncol(cost_matrix)

  # --- Caliper ---
  caliper_val <- NULL
  if (!is.null(caliper_sd)) {
    if (implicit) {
      pooled_sd <- cpp_lazy_distance_sd(cost_matrix$left_mat, cost_matrix$right_mat,
                                        cost_matrix$distance,
                                        lazy_cost_spec_inv_cov(cost_matrix))
      if (!is.na(pooled_sd)) caliper_val <- caliper_sd * pooled_sd
    } else {
      finite_dists <- cost_matrix[is.finite(cost_matrix)]
      if (length(finite_dists) > 1) {
        caliper_val <- caliper_sd * stats::sd(finite_dists)
      }
    }
  } else if (!is.null(caliper)) {
    caliper_val <- caliper
  }

  # A distance beyond the caliper is no pair: Inf in a matrix, the source's
  # distance cut in a specification, which rules it out by the same test.
  if (!is.null(caliper_val)) {
    if (implicit) {
      cost_matrix$max_distance <- caliper_val
    } else {
      cost_matrix[cost_matrix > caliper_val] <- Inf
    }
  }

  potentials <- NULL
  certificate <- NULL
  search <- NULL

  if (method == "optimal") {
    if (implicit) {
      # --- Optimal full matching, grown pair by pair over the specification ---
      knobs <- .implicit_defaults()
      compiled <- lap_design_implicit(
        cost_matrix$left_mat, cost_matrix$right_mat, cost_matrix$distance,
        lazy_cost_spec_inv_cov(cost_matrix), cost_matrix$max_distance,
        lazy_cost_spec_calipers(cost_matrix), "full_match",
        as.numeric(min_controls),
        if (is.infinite(max_controls)) Inf else as.numeric(max_controls),
        knobs$keep_per_row, knobs$width, knobs$tol, knobs$max_rounds, TRUE
      )

      if (isTRUE(compiled$bounds_feasible)) {
        solved <- list(status = compiled$status)
        read <- .full_match_groups(compiled, compiled$flow, min_controls)
        potentials <- .full_match_potentials(compiled, compiled$potential)
        if (is.function(cost_matrix$distance)) {
          placed <- as.numeric(compiled$flow) > 0
          lazy_pair_distances(cost_matrix, compiled$block$row[placed],
                              compiled$block$col[placed])
        }
        if (!is.null(compiled$certificate)) {
          certificate <- .new_flow_certificate(compiled$certificate, knobs$tol)
        }
        search <- list(
          seed_width      = as.integer(compiled$search$seed_width),
          candidate_edges = as.numeric(compiled$search$candidate_edges),
          possible_edges  = as.numeric(compiled$search$possible_edges),
          edges_evaluated = as.numeric(compiled$search$edges_evaluated),
          n_rounds        = as.integer(compiled$search$n_rounds),
          rounds          = tibble::as_tibble(compiled$search$rounds)
        )
      } else {
        solved <- NULL
        read <- list(group_of_left = integer(n_left),
                     group_of_right = integer(n_right), n_groups = 0L)
      }
    } else {
      # --- Optimal full matching, compiled and solved as a flow ---
      compiled <- lap_flow_compile_full_match(
        cost_matrix, as.numeric(min_controls),
        if (is.infinite(max_controls)) Inf else as.numeric(max_controls)
      )

      if (isTRUE(compiled$bounds_feasible)) {
        solved <- .flow_solve(.flow_problem(
          n_nodes = compiled$problem$n_nodes,
          supply = compiled$problem$supply,
          arcs = tibble::tibble(tail = compiled$problem$tail,
                                head = compiled$problem$head,
                                lower = compiled$problem$lower,
                                upper = compiled$problem$upper,
                                cost = compiled$problem$cost)
        ))
        read <- .full_match_groups(compiled, solved$flow, min_controls)
        potentials <- .full_match_potentials(compiled, solved$potential)
        certificate <- verify_flow(solved)
      } else {
        solved <- NULL
        read <- list(group_of_left = integer(n_left),
                     group_of_right = integer(n_right), n_groups = 0L)
      }
    }

    # group_of_left / group_of_right: 1-based group IDs, 0 = unmatched
    gol <- read$group_of_left
    gor <- read$group_of_right

    groups_rows <- list()
    n_groups_out <- 0L
    matched_left_idx <- integer(0)
    matched_right_idx <- integer(0)

    for (g in seq_len(read$n_groups)) {
      left_in_g <- which(gol == g)
      right_in_g <- which(gor == g)
      n_right_in_group <- length(right_in_g)
      n_left_in_group <- length(left_in_g)

      # A group needs a unit on each side: it is the pairing that carries the
      # weight ratio and the matched distance. A one-sided group holds no
      # match, so its members stay unmatched.
      if (n_left_in_group == 0L || n_right_in_group == 0L) next

      n_groups_out <- n_groups_out + 1L
      matched_left_idx <- c(matched_left_idx, left_in_g)
      matched_right_idx <- c(matched_right_idx, right_in_g)

      # Every left unit weighs 1, and the group's right units share a total
      # weight equal to the number of left units, so the two sides of a group
      # weigh the same whichever side the group is centred on.
      left_weight <- 1.0
      right_weight <- n_left_in_group / n_right_in_group
      groups_rows[[length(groups_rows) + 1L]] <- tibble::tibble(
        group_id = rep(n_groups_out, n_left_in_group),
        id = l_ids[left_in_g],
        side = rep("left", n_left_in_group),
        weight = rep(left_weight, n_left_in_group)
      )
      groups_rows[[length(groups_rows) + 1L]] <- tibble::tibble(
        group_id = rep(n_groups_out, n_right_in_group),
        id = r_ids[right_in_g],
        side = rep("right", n_right_in_group),
        weight = rep(right_weight, n_right_in_group)
      )
    }

    status <- .full_match_status(compiled$bounds_feasible,
                                 if (is.null(solved)) NULL else solved$status,
                                 n_groups_out)

  } else {
    # --- Greedy group formation ---
    # Step 1: Each left unit picks its nearest eligible right unit
    left_to_right <- integer(n_left)
    right_assigned <- logical(n_right)
    group_of_left <- integer(n_left)

    min_dists <- apply(cost_matrix, 1, function(row) {
      finite <- row[is.finite(row)]
      if (length(finite) == 0) Inf else min(finite)
    })
    left_order <- order(min_dists)

    group_id <- 0L
    groups_list <- vector("list", n_left)

    for (i in left_order) {
      row <- cost_matrix[i, ]
      available <- which(is.finite(row) & !right_assigned)
      if (length(available) == 0) next
      best_j <- available[which.min(row[available])]
      group_id <- group_id + 1L
      left_to_right[i] <- best_j
      right_assigned[best_j] <- TRUE
      group_of_left[i] <- group_id
      groups_list[[group_id]] <- list(left_idx = i, right_idxs = best_j)
    }

    # Step 2: Assign remaining right units to nearest already-matched left unit
    remaining_right <- which(!right_assigned)

    if (length(remaining_right) > 0) {
      matched_left_idxs <- which(group_of_left > 0)
      for (j in remaining_right) {
        col <- cost_matrix[matched_left_idxs, j]
        eligible <- rep(TRUE, length(matched_left_idxs))
        if (!is.infinite(max_controls)) {
          for (k in seq_along(matched_left_idxs)) {
            li <- matched_left_idxs[k]
            gi <- group_of_left[li]
            if (length(groups_list[[gi]]$right_idxs) >= max_controls) {
              eligible[k] <- FALSE
            }
          }
        }
        eligible <- eligible & is.finite(col)
        if (!any(eligible)) next
        best_k <- which(eligible)[which.min(col[eligible])]
        best_left <- matched_left_idxs[best_k]
        gi <- group_of_left[best_left]
        groups_list[[gi]]$right_idxs <- c(groups_list[[gi]]$right_idxs, j)
      }
    }

    # Step 3: Check min_controls constraint
    if (min_controls > 1) {
      keep <- rep(TRUE, length(groups_list))
      for (g in seq_len(group_id)) {
        if (is.null(groups_list[[g]])) { keep[g] <- FALSE; next }
        if (length(groups_list[[g]]$right_idxs) < min_controls) {
          keep[g] <- FALSE
        }
      }
      groups_list <- groups_list[keep]
    }

    status <- "heuristic"

    groups_rows <- list()
    n_groups_out <- 0L
    matched_left_idx <- integer(0)
    matched_right_idx <- integer(0)
    for (g in seq_along(groups_list)) {
      grp <- groups_list[[g]]
      if (is.null(grp)) next
      n_groups_out <- n_groups_out + 1L
      n_right_in_group <- length(grp$right_idxs)
      matched_left_idx <- c(matched_left_idx, grp$left_idx)
      matched_right_idx <- c(matched_right_idx, grp$right_idxs)
      right_weight <- 1 / n_right_in_group
      groups_rows[[length(groups_rows) + 1]] <- tibble::tibble(
        group_id = n_groups_out,
        id = l_ids[grp$left_idx],
        side = "left",
        weight = 1.0
      )
      groups_rows[[length(groups_rows) + 1]] <- tibble::tibble(
        group_id = rep(n_groups_out, n_right_in_group),
        id = r_ids[grp$right_idxs],
        side = rep("right", n_right_in_group),
        weight = rep(right_weight, n_right_in_group)
      )
    }
  }

  # --- Build output tibble ---
  if (length(groups_rows) > 0) {
    groups_tbl <- dplyr::bind_rows(groups_rows)
  } else {
    groups_tbl <- tibble::tibble(
      group_id = integer(0), id = character(0),
      side = character(0), weight = numeric(0)
    )
  }

  # Unmatched is whatever the emitted groups do not cover, so every unit is
  # either a row of `groups` or an entry of `unmatched`, and the counts in
  # `info` describe the rows that were actually written.
  unmatched_left_idx <- setdiff(seq_len(n_left), matched_left_idx)
  unmatched_right_idx <- setdiff(seq_len(n_right), matched_right_idx)

  unmatched <- list(
    left = l_ids[unmatched_left_idx],
    right = r_ids[unmatched_right_idx]
  )

  info <- list(
    n_groups = n_groups_out,
    n_left = n_left,
    n_right = n_right,
    n_matched_left = n_left - length(unmatched_left_idx),
    n_matched_right = n_right - length(unmatched_right_idx),
    n_unmatched_left = length(unmatched_left_idx),
    n_unmatched_right = length(unmatched_right_idx),
    method = paste0("full_", method),
    distance_metric = if (is.function(distance)) "custom" else distance,
    vars = vars
  )
  info <- c(info, design_estimand(n_left, info$n_matched_left))

  result <- list(
    groups = groups_tbl,
    status = status,
    info = info,
    unmatched = unmatched
  )

  # Both sit at the top level rather than inside info, which callers routinely
  # truncate.
  if (!is.null(potentials)) result$potentials <- potentials
  if (!is.null(certificate)) result$certificate <- certificate
  if (!is.null(search)) result$search <- search

  structure(result, class = c("full_matching_result", "couplr_result"))
}


#' Print Method for Full Matching Results
#'
#' @param x A full_matching_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object \code{x}.
#' @export
print.full_matching_result <- function(x, ...) {
  cat("\nFull Matching Result\n")
  cat("====================\n\n")
  cat(sprintf("  Status: %s\n", x$status))
  cat(sprintf("  Groups formed: %d\n", x$info$n_groups))
  cat(sprintf("  Left units:  %d matched, %d unmatched (of %d)\n",
              x$info$n_matched_left, x$info$n_unmatched_left, x$info$n_left))
  cat(sprintf("  Right units: %d matched, %d unmatched (of %d)\n",
              x$info$n_matched_right, x$info$n_unmatched_right, x$info$n_right))

  if (x$info$n_groups > 0) {
    # Group size distribution
    grp_sizes <- table(x$groups$group_id[x$groups$side == "right"])
    cat(sprintf("\n  Right units per group: min=%d, median=%.0f, max=%d\n",
                min(grp_sizes), stats::median(as.numeric(grp_sizes)),
                max(grp_sizes)))
  }
  cat("\n")
  invisible(x)
}
