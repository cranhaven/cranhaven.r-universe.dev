#' Runs Multi-Arm Bandit Trial
#' @name mab_loop
#' @description Performs a full Multi-Arm Bandit (MAB) trial using Thompson Sampling or UCB1.
#' The function provides a loop around each step of the process for each treatment wave, performing adaptive
#' treatment assignment, and outcome imputation or generation as needed.
#'
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @inheritParams simulate_mab
#' @inheritParams prep_sim_data
#' @param num_conditions Number of conditions, equivalent to `length(conditions)`.
#' @param periods Number of simulation periods.
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, with the trial's results.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 or Thompson Sampling values for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each
#' treatment arm at a given period.
#' \item `assignment_quantities`: A numeric vector of the total number of observations assigned to each treatment arm.
#' }
#' @details
#' The first period is used to initialize the trial, so the loop
#' starts at period number 2.
#'
#' @keywords internal
#' @family simulation
mab_loop <- function(
  data,
  sim_type,
  p,
  algorithm,
  control_augment,
  random_assign_prop,
  prior_periods,
  discount_rate,
  whole_experiment = NULL,
  simulate_dates,
  delayed_feedback,
  clustering,
  blocking,
  conditions,
  col_names,
  imputation_information = NULL,
  ndraws,
  verbose,
  period_idxs,
  periods,
  num_conditions,
  time_model = NULL,
  time_model_args = NULL
) {
  bandits <- vector(mode = "list", length = 2)

  bandits[["bandit_stat"]] <- matrix(
    NA,
    nrow = periods,
    ncol = num_conditions,
    dimnames = list(c(), conditions)
  )
  bandits[["assignment_prob"]] <- matrix(
    NA,
    nrow = periods,
    ncol = num_conditions,
    dimnames = list(c(), conditions)
  )
  bandits[["assignment_prob"]][1, ] <- rep(
    1 / num_conditions,
    num_conditions
  )

  if (periods > 1) {
    for (i in 2:periods) {
      current_idx <- period_idxs[["start_idxs"]][i]:period_idxs[["end_idxs"]][i]
      verbose_log(verbose, paste0("Period: ", i))

      prior <- compute_lookback(
        prior_periods = prior_periods,
        current_period = i
      )

      current_data <- data[current_idx, ]
      prior_data <- data[
        period_idxs[["start_idxs"]][prior]:period_idxs[["end_idxs"]][i - 1],
      ]

      current_bandit <- compute_prior(
        current_data = current_data,
        prior_data = prior_data,
        delayed_feedback = delayed_feedback,
        assignment_date_col = col_names[["assignment_date_col"]],
        conditions = conditions,
        discount_rate = discount_rate,
        current_period = i
      ) |>
        compute_bandit(
          algorithm = algorithm,
          num_conditions = num_conditions,
          conditions = conditions,
          current_period = i,
          control_augment = control_augment,
          random_assign_prop = random_assign_prop,
          ndraws = ndraws
        )
      bandits[["bandit_stat"]][i - 1, ] <- current_bandit[["bandit"]]
      bandits[["assignment_prob"]][i, ] <- current_bandit[["assignment_prob"]]

      current_data <- assign_treatments(
        current_data = current_data,
        probs = current_bandit[["assignment_prob"]],
        blocking = blocking,
        clustering = clustering,
        cluster_col = col_names[["cluster_col"]],
        condition_col = col_names[["condition_col"]],
        conditions = conditions,
        sim_type = sim_type
      )

      if (sim_type == "resim") {
        prepped_impute <- prep_imputation(
          current_data = current_data,
          whole_experiment = whole_experiment,
          imputation_information = imputation_information,
          block_cols = col_names[["block_cols"]],
          blocking = blocking,
          delayed_feedback = delayed_feedback,
          current_period = i
        )

        data <- impute_outcomes(
          data = data,
          imputation_info = prepped_impute,
          success_col = col_names[["success_col"]],
          success_date_col = col_names[["success_date_col"]],
          delayed_feedback = delayed_feedback,
          idx = current_idx
        )
      } else if (sim_type == "param") {
        data <- generate_outcomes(
          current_data = current_data,
          data = data,
          p = p,
          idx = current_idx,
          current_period = i,
          simulate_dates = simulate_dates,
          time_model = time_model,
          time_model_args = time_model_args
        )
      } else if (sim_type == "test") {
        # Randomization Inference, No Change in Outcomes
        if (data.table::is.data.table(data)) {
          data[current_idx, mab_condition := current_data[, mab_condition]]
        } else {
          data[["mab_condition"]][current_idx] <- current_data[[
            "mab_condition"
          ]]
        }
      }
    }
  }
  results <- collect_mab_results(
    data = data,
    bandits = bandits,
    algorithm = algorithm,
    conditions = conditions,
    num_conditions = num_conditions,
    periods = periods,
    ndraws = ndraws
  )

  return(results)
}

#-------------------------------------------------------------------------------
#' @name collect_mab_results
#' @title Ends Multi-Arm Bandit Trial
#' @param data Finalized data from [mab_loop()].
#' @param bandits Finalized bandits list of matrices from [mab_loop()].
#' @param periods Total number of periods in Multi-Arm-Bandit trial.
#' @inheritParams mab_loop
#' @inheritParams run_mab
#' @returns  A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, with the trial's results.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 or Thompson Sampling values for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being
#' assigned each treatment arm at a given period.
#' \item `assignment_quantities`: A numeric vector of the total number of observations assigned to each treatment arm.
#' }
#' @description Condenses output from [mab_loop()] into
#' manageable structure. Contains methods for `data.frame` and `data.table`.
#' @keywords internal
#' @family simulation

collect_mab_results <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  UseMethod("collect_mab_results", data)
}
#-------------------------------------------------------------------------------
#' @method collect_mab_results data.frame
#' @rdname collect_mab_results
#' @export
collect_mab_results.data.frame <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  final_summary <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::summarize(
      successes = sum(mab_success, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    as.list() |>
    finalize_prior_list(conditions = conditions)

  final_bandit <- compute_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    num_conditions = num_conditions,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )

  bandits[["bandit_stat"]][periods, ] <- final_bandit[["bandit"]]
  df_bandits <- lapply(bandits, \(x) {
    tibble::as_tibble(x) |>
      dplyr::mutate(period_number = dplyr::row_number())
  })

  assignment_quantities <- data |>
    dplyr::group_by(mab_condition) |>
    dplyr::count() |>
    as_named_vec(
      val = "n",
      name = "mab_condition"
    )

  if (length(assignment_quantities) < length(conditions)) {
    missing <- setdiff(
      conditions,
      names(assignment_quantities)
    )
    assignment_quantities[missing] <- 0
  }

  matrix_idx <- cbind(
    data[["period_number"]],
    match(data[["mab_condition"]], conditions)
  )
  data <- data |>
    dplyr::mutate(
      mab_assign_prob = bandits[["assignment_prob"]][matrix_idx],
      ipw_weights = 1 / mab_assign_prob
    )

  return(list(
    final_data = data,
    bandits = df_bandits[["bandit_stat"]],
    assignment_probs = df_bandits[["assignment_prob"]],
    assignment_quantities = assignment_quantities
  ))
}
#-------------------------------------------------------------------------------
#' @method collect_mab_results data.table
#' @rdname collect_mab_results
#' @export
collect_mab_results.data.table <- function(
  data,
  bandits,
  algorithm,
  periods,
  conditions,
  num_conditions,
  ndraws
) {
  final_summary <- data[,
    .(
      successes = sum(mab_success, na.rm = TRUE),
      n = .N
    ),
    by = mab_condition
  ] |>
    as.list() |>
    finalize_prior_list(conditions = conditions)

  final_bandit <- compute_bandit(
    past_results = final_summary,
    algorithm = algorithm,
    num_conditions = num_conditions,
    conditions = conditions,
    current_period = (periods + 1),
    control_augment = 0,
    ndraws = ndraws
  )
  bandits[["bandit_stat"]][periods, ] <- final_bandit[["bandit"]]
  bandit_stats <- data.table::as.data.table(bandits[["bandit_stat"]])
  bandit_stats[, period_number := .I]

  assignment_probs <- data.table::as.data.table(bandits[["assignment_prob"]])
  assignment_probs[, period_number := .I]

  assignment_quantities <- data[, .(count = .N), by = mab_condition] |>
    as_named_vec(val = "count", name = "mab_condition")
  assignment_quantities <- assignment_quantities[sort(names(
    assignment_quantities
  ))]

  if (length(assignment_quantities) < length(conditions)) {
    missing <- setdiff(conditions, names(assignment_quantities))
    assignment_quantities[missing] <- 0
  }

  matrix_idx <- cbind(
    data[["period_number"]],
    match(data[["mab_condition"]], conditions)
  )
  assign_vec <- bandits[["assignment_prob"]][matrix_idx]
  data[, `:=`(
    mab_assign_prob = assign_vec,
    ipw_weights = 1 / assign_vec
  )]

  return(list(
    final_data = data,
    bandits = bandit_stats,
    assignment_probs = assignment_probs,
    assignment_quantities = assignment_quantities
  ))
}
#------------------------------------------------------------------------------
#' Create Prior Periods
#' @inheritParams mab_from_rct
#' @param current_period The current period of the simulation. Defined by loop structure inside [mab_loop()].
#' @returns Numeric value referring to the period index to look back from.
#' @describeIn mab_loop Used during [mab_loop()] to create a vector of prior periods dynamically based on the specified
#' number of prior periods.
#' @keywords internal

compute_lookback <- function(prior_periods = NULL, current_period) {
  if (is.null(prior_periods)) {
    1
  } else {
    max(current_period - prior_periods, 1)
  }
}
