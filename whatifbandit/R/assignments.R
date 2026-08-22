#' Adaptively Assign Treatments in a Period
#' @description Assigns new treatments for an assignment wave based on the assignment probabilities provided from
#' [compute_bandit()]. Assignments are made randomly with the given probabilities using [randomizr::block_ra()],
#' [randomizr::complete_ra()], [randomizr::cluster_ra()], or [randomizr::block_and_cluster_ra()]
#' depending on whether blocking and/or clustering are used.
#'
#' @name assign_treatments
#' @inheritParams run_mab
#' @inheritParams mab_from_rct
#' @param condition_col Column name of `current_data` which holds original treatment assignments.
#' @param cluster_col Column name of `current_data` which holds cluster assignments.
#' @param probs Named numeric vector; probability of assignment for each treatment condition.
#' @inheritParams compute_prior
#' @returns Updated `tibble` or `data.table` with the new treatment conditions for each observation, and whether imputation is required.
#' If this treatment is different from the original experiment, `impute_req = 1`, otherwise it is 0.
#'
#' @seealso
#'* [randomizr::block_ra()]
#'* [randomizr::complete_ra()]
#'* [randomizr::cluster_ra()]
#'* [randomizr::block_and_cluster_ra()]
#' @keywords internal
#' @family assign
assign_treatments <- function(
  current_data,
  probs,
  blocking = NULL,
  clustering = NULL,
  conditions,
  condition_col = NULL,
  cluster_col = NULL,
  sim_type
) {
  UseMethod("assign_treatments", current_data)
}
#' @describeIn assign_treatments Selects the appropriate `{randomizr}` function and constructs its argument list
#' based on whether blocking and/or clustering are requested.
#' @inheritParams compute_prior
#' @inheritParams assign_treatments
#' @param dt Logical. Whether `current_data` is a data.table.
#' @returns A list with `fn` (the randomizr function) and `args` (its arguments).
#' @keywords internal
build_ra_args <- function(
  current_data,
  probs = NULL,
  conditions,
  blocking,
  clustering,
  cluster_col = NULL,
  dt
) {
  if (blocking && clustering) {
    list(
      fn = randomizr::block_and_cluster_ra,
      args = list(
        blocks = current_data[["block"]],
        clusters = current_data[[cluster_col]],
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  } else if (blocking) {
    list(
      fn = randomizr::block_ra,
      args = list(
        blocks = current_data[["block"]],
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  } else if (clustering) {
    list(
      fn = randomizr::cluster_ra,
      args = list(
        clusters = current_data[[cluster_col]],
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  } else {
    list(
      fn = randomizr::complete_ra,
      args = list(
        N = nrow(current_data),
        prob_each = probs,
        conditions = conditions,
        check_inputs = TRUE
      )
    )
  }
}

#' @describeIn assign_treatments Pre-allocates a character vector and fills treatment assignments
#' for bandit and randomly assigned subsets separately, using the appropriate
#' randomizr function built by [build_ra_args()].
#' @inheritParams compute_prior
#' @inheritParams assign_treatments
#' @returns Character vector of length `nrow(current_data)` with treatment assignments.
#' @keywords internal

compute_assignments <- function(
  current_data,
  probs,
  conditions,
  blocking,
  clustering,
  cluster_col = NULL
) {
  assignments <- vector("character", nrow(current_data))

  ra <- build_ra_args(
    current_data = current_data,
    probs = probs,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    cluster_col = cluster_col
  )
  assignments <- as.character(do.call(
    ra[["fn"]],
    ra[["args"]]
  ))

  return(assignments)
}

#----------------------------------------------------------------------------------

#' @method assign_treatments data.frame
#' @rdname assign_treatments
#' @export
assign_treatments.data.frame <- function(
  current_data,
  probs,
  blocking,
  clustering,
  conditions,
  condition_col = NULL,
  cluster_col = NULL,
  sim_type
) {
  current_data[["mab_condition"]] <- compute_assignments(
    current_data = current_data,
    probs = probs,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    cluster_col = cluster_col
  )

  if (sim_type == "resim") {
    current_data[["impute_req"]] <- as.integer(
      as.character(current_data[["mab_condition"]]) !=
        as.character(current_data[[condition_col]])
    )
  }

  return(current_data)
}

#' @method assign_treatments data.table
#' @rdname assign_treatments
#' @export
assign_treatments.data.table <- function(
  current_data,
  probs,
  blocking,
  clustering,
  conditions,
  condition_col = NULL,
  cluster_col = NULL,
  sim_type
) {
  current_data[,
    mab_condition := compute_assignments(
      current_data = current_data,
      probs = probs,
      conditions = conditions,
      blocking = blocking,
      clustering = clustering,
      cluster_col = cluster_col
    )
  ]

  if (sim_type == "resim") {
    current_data[,
      impute_req := as.integer(
        as.character(mab_condition) != as.character(get(condition_col))
      )
    ]
  }
  return(invisible(current_data))
}
