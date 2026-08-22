#' Generate Block and Cluster Memberships
#' @name generate_groups
#' @description Takes a named probability vector for blocks and clusters and uses
#' [randomizr::complete_ra()] to randomly assign each of `n` units to a
#' block and cluster according to those probabilities.
#'
#' @inheritParams simulate_mab
#'
#' @returns A list containing the factor vectors of group assignments for blocks, clusters, both or `NULL` depending on what was specified. Levels
#' for each vector come from the labels for each block and cluster.
#' @family param
#' @keywords internal
generate_groups <- function(n, blocks = NULL, clusters = NULL) {
  supplied_groups <- list(blocks = blocks, clusters = clusters)
  return_vecs <- list(blocks = NULL, clusters = NULL)

  null_check <- vapply(supplied_groups, is.null, logical(1))

  if (all(null_check)) {
    return(return_vecs)
  } else if (any(null_check)) {
    group <- blocks %||% clusters
    name <- names(supplied_groups)[!null_check]
    return_vec <- randomizr::complete_ra(
      N = n,
      prob_each = group,
      conditions = names(group)
    )
    if (name == "clusters") {
      return_vec <- return_vec[order(return_vec)]
    }
    return_vecs[[name]] <- return_vec

    return(return_vecs)
  } else {
    computed_blocks <- randomizr::complete_ra(
      N = n,
      prob_each = blocks,
      conditions = names(blocks)
    )
    computed_clusters <- vector("character", length = n) |>
      factor(levels = unlist(lapply(clusters, names)))
    for (block in names(clusters)) {
      idx <- block == computed_blocks
      probs <- clusters[[block]]
      computed_clusters[idx] <- randomizr::complete_ra(
        N = sum(idx),
        prob_each = probs,
        conditions = names(probs)
      )
    }
    ord <- order(computed_clusters)
    return_vecs[["clusters"]] <- computed_clusters[ord]
    return_vecs[["blocks"]] <- computed_blocks[ord]
    return(return_vecs)
  }
  return(return_vecs)
}

#' Generate Start and End Indexes
#' @description
#' Generates the start and end indexes for each period based on provided information
#' @name generate_period_idx
#' @inheritParams simulate_mab
#' @returns list of numeric vectors featuring start and end indexes for each period of the simulation
#' @keywords internal
#' @details When not provided period sizes are calculated as `n %/% t` for all periods, with the last as `n %/% t  + n %% t`.
#' @family param

generate_period_idx <- function(n, t, period_sizes = NULL) {
  period_sizes <-
    if (!is.null(period_sizes)) {
      period_sizes
    } else {
      size <- floor(n / t)
      period_sizes <- c(rep(size, t - 1), n - (size * (t - 1)))
      period_sizes
    }
  ends <- cumsum(period_sizes)
  starts <- c(1, ends[-t] + 1)
  return(list(
    period_sizes = period_sizes,
    start_idxs = starts,
    end_idxs = ends
  ))
}

#' Generate Assignment Dates
#' @name generate_assignment_dates
#' @description
#' Generates a `length(n)` vector of assignment dates based on provided information.
#' @inheritParams simulate_mab
#' @returns vector of assignment dates
#' @keywords internal
#'@family param
generate_assignment_dates <- function(n, assignment_dates) {
  if (is.null(assignment_dates)) {
    NULL
  } else if (length(assignment_dates) < n) {
    sort(rep_len(assignment_dates, n))
  } else {
    assignment_dates
  }
}

#' Extract Success Probabilities Per-Unit
#' @name extract_success_prob
#' @description Looks up the success probability for each unit given their treatment
#' assignment and, optionally, their block and/or cluster membership. Handles
#' all supported `p` structures.
#'
#' @inheritParams simulate_mab
#' @inheritParams run_mab
#' @param conditions A character or factor vector of treatment assignments of
#'   length `n`.
#' @param other_idx Character vector of block or cluster assignments to be used as the
#' additional index for extracting from `p`.
#' @returns A numeric vector of length `n` containing the per-unit success
#'   probabilities to be used for outcome observation.
#' @keywords internal
extract_success_prob <- function(
  p,
  conditions,
  other_idx = NULL
) {
  if (!is.null(other_idx)) {
    extract_mat <- matrix(
      data = c(conditions, other_idx),
      ncol = 2
    )
    p[extract_mat]
  } else {
    return(p[conditions, ] |> unname())
  }
}

#' Generate Outcomes Per-Unit
#' @name generate_outcomes
#' @description
#' Uses provided success probabilities to draw a Bernoulli outcome for each unit. If `time_model` is provided, it is also used to compute
#' dates of success
#' @inheritParams impute_outcomes
#' @inheritParams simulate_mab
#' @inheritParams prep_sim_data
#' @inheritParams run_mab
#' @inheritParams compute_prior
#' @param current_period Indicator of the current period of the simulation.
#' @returns Updated `data` object containing all the outcomes generated in the period, such as the treatment assignments, treatment outcomes, and new success dates
#' @keywords internal
#' @family param

generate_outcomes <- function(
  current_data,
  current_period,
  data,
  p,
  idx,
  simulate_dates,
  time_model = NULL,
  time_model_args = NULL
) {
  conditions <- current_data[["mab_condition"]]
  probs <- extract_success_prob(
    p = p,
    conditions = conditions,
    other_idx = current_data[["cluster"]] %||% current_data[["block"]]
  )

  outcomes <- stats::rbinom(
    nrow(current_data),
    1,
    prob = probs
  )
  success_times <- if (simulate_dates) {
    do.call(
      time_model,
      c(
        list(
          n = nrow(current_data),
          current_period = current_period,
          conditions = conditions,
          success = outcomes,
          blocks = current_data[["block"]],
          clusters = current_data[["cluster"]]
        ),
        time_model_args
      )
    )
  } else {
    NULL
  }
  modified_cols <- c("mab_condition", "mab_success")
  is_dt <- data.table::is.data.table(current_data)

  if (is_dt) {
    current_data[, mab_success := outcomes]
  } else {
    current_data[["mab_success"]] <- outcomes
  }

  if (simulate_dates) {
    if (is_dt) {
      current_data[, new_success_date := assignment_date + success_times]
    } else {
      current_data[["new_success_date"]] <- current_data[["assignment_date"]] +
        success_times
    }
    modified_cols <- c(modified_cols, "new_success_date")
  }

  if (is_dt) {
    data[idx, (modified_cols) := current_data[, modified_cols, with = FALSE]]
  } else {
    data[idx, ] <- current_data
  }
  invisible(data)
}
