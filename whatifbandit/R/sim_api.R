#' @title Run a single MAB simulation iteration
#' @name run_mab_single
#' @description Runs a single iteration of a MAB simulation. Intended to be
#' called either directly for ` r = 1` or as the mapped function inside
#' [furrr::future_map()]` for ` r > 1`. Handles all `sim_type`.
#' @inheritParams run_mab
#' @inheritParams simulate_mab
#' @inheritParams prep_sim_data
#' @inheritParams run_mab
#' @returns A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, with the trial's results.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 or Thompson Sampling values for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each
#' treatment arm at a given period.
#' \item `assignment_quantities`: A numeric vector of the total number of observations assigned to each
#' treatment arm.
#' \item `means`: A `tibble` or `data.table` containing the mean estimates of the specified estimators for
#' each treatment arm.
#' \item `contrasts`: A `tibble` or `data.table` containing the contrast estimates of the specified estimators for
#' the specified contrast structure.
#' #' \item `f_stats`: Named numeric vector containing f_stat from IPW and OLS regressions.
#' #' \item `models`: A nested list containing the `lm_robust` objects from regressions, only saved
#' in clustered case.
#' \item `call`: `NULL`; initialized for later assignment.
#' \item `args`: `NULL`; initialized for later assignment.
#' \item `furrr`: `NULL`; initialized for later assignment.
#' }
#' @keywords internal
run_mab_single <- function(
  sim_type,
  algorithm,
  estimators = c("aipw", "ipw", "ols"),
  control_augment = 0,
  random_assign_prop = 0,
  prior_periods = NULL,
  delayed_feedback = FALSE,
  discount_rate = 1,
  conditions,
  blocking,
  clustering,
  col_names,
  ndraws = 5000,
  keep_data = FALSE,
  keep_models = FALSE,
  contrasts = NULL,
  verbose = FALSE,
  r = 1,
  imputation_information = NULL,
  whole_experiment = NULL,
  time_model = NULL,
  time_model_args = NULL,
  p = NULL,
  n = NULL,
  dt = NULL,
  blocks = NULL,
  clusters = NULL,
  equal_probs = NULL,
  assignment_dates = NULL,
  simulate_dates = NULL,
  period_idxs,
  data = NULL
) {
  if (sim_type == "param") {
    data <- prep_sim_data(
      n = n,
      p = p,
      blocks = blocks,
      clusters = clusters,
      blocking = blocking,
      clustering = clustering,
      period_idxs = period_idxs,
      conditions = conditions,
      equal_probs = equal_probs,
      assignment_dates = assignment_dates,
      simulate_dates = simulate_dates,
      time_model = time_model,
      time_model_args = time_model_args,
      dt = dt
    )
  }
  run_mab(
    data = data,
    sim_type = sim_type,
    estimators = estimators,
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    prior_periods = prior_periods,
    delayed_feedback = delayed_feedback,
    whole_experiment = whole_experiment,
    discount_rate = discount_rate,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    col_names = col_names,
    verbose = verbose && r == 1,
    ndraws = ndraws,
    period_idxs = period_idxs,
    keep_data = keep_data || r == 1,
    keep_models = keep_models || r == 1,
    contrasts = contrasts,
    imputation_information = imputation_information,
    time_model = time_model,
    time_model_args = time_model_args,
    p = p,
    simulate_dates = simulate_dates
  )
}
#' Prepares Data for Simulated MAB
#' @name prep_sim_data
#' @description
#' Initializes the data a simulated MAB trial. Generates block and
#' cluster assignments, allocates all required columns, and assigns treatments and
#' outcomes for the first period using equal assignment probabilities.
#' @inheritParams simulate_mab
#' @inheritParams run_mab
#' @param period_idxs List containing vectors which map their entries to the starting row and ending
#' row of each period.
#' @param equal_probs Vector of equal assignment probabilities.
#' @param simulate_dates Logical; whether or not new success dates should be generated using
#' `time_model`. Does not guarantee these new dates are used for assignment, `delayed_feedback` controls
#' that behavior.
#' @family param
#' @returns Initialized `data.table` or `tibble` with the first period simulation conducted, and all
#' required columns for [run_mab()]
#' @keywords internal

prep_sim_data <- function(
  n,
  p,
  blocks = NULL,
  clusters = NULL,
  blocking,
  clustering,
  conditions,
  equal_probs,
  period_idxs,
  simulate_dates,
  assignment_dates = NULL,
  time_model = NULL,
  time_model_args = NULL,
  dt
) {
  df_func <- if (dt) data.table::data.table else tibble::tibble

  blocks_clusters <- generate_groups(
    n = n,
    blocks = blocks,
    clusters = clusters
  )
  period_number <- findInterval(seq_len(n), period_idxs[["start_idxs"]])
  current_idx <- period_idxs[["start_idxs"]][1]:period_idxs[["end_idxs"]][1]

  cols <- list(
    period_number = period_number,
    block = if (!is.null(blocks_clusters[["blocks"]])) {
      as.character(blocks_clusters[["blocks"]])
    } else {
      NULL
    },
    cluster = if (!is.null(blocks_clusters[["clusters"]])) {
      as.character(blocks_clusters[["clusters"]])
    } else {
      NULL
    },
    assignment_date = assignment_dates,
    mab_condition = rep(NA_character_, n),
    mab_success = rep(NA_real_, n)
  )

  if (simulate_dates) {
    cols[["new_success_date"]] <- rep(as.Date(NA), n)
  }

  data <- do.call(df_func, cols)
  if (clustering) {
    check_clusters(data, cluster_col = "cluster")
  }

  data <- assign_treatments(
    current_data = data[current_idx, ],
    probs = equal_probs,
    blocking = blocking,
    clustering = clustering,
    conditions = conditions,
    sim_type = "param",
    cluster_col = "cluster"
  ) |>
    generate_outcomes(
      p = p,
      idx = current_idx,
      current_period = 1,
      data = data,
      simulate_dates = simulate_dates,
      time_model = time_model,
      time_model_args = time_model_args
    )

  return(invisible(data))
}
#------------------------------------------------------------------------------
#' @title Simulates a MAB Trial From Prepared Inputs and Performs Inference
#' @name run_mab
#' @description Internal helper. Centralizes necessary functions to conduct a
#' a MAB trial with adaptive inference. It assumes all inputs have been preprocessed already.
#' @param data `tibble` or `data.table` holding necessary information to complete the simulation.
#' Also used to store simulation outputs.
#' @inheritParams mab_from_rct
#' @inheritParams prep_rct_data
#' @inheritParams simulate_mab
#' @inheritParams prep_sim_data
#' @param imputation_information Object created by [precompute_imputation()] containing the conditional
#' means and success dates
#' for each treatment block to impute from.
#' @param sim_type String; Type of simulation to conduct, either `"resim"`, `"param"`, or `"test"`,
#' for a resimulated rct, simulation from population parameters, or simulation for the randomization joint test.
#' @param estimators Character vector; Which estimators to compute, can include `"aipw"`, `"ipw"`,
#' "ols", and any combination
#' of them in a vector.
#' @param time_model_args Arguments passed to `time_model` function.
#' @param conditions Character vector of treatment condition labels. If a control group is specified
#' the `names` attribute should be present with the control group labeled `"control"`.
#'
#'
#' @returns A named list containing:
#' \itemize{
#' \item `final_data`: The processed `tibble` or `data.table`, with the trial's results.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 or Thompson Sampling values for each period.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each
#' treatment arm at a given period.
#' \item `assignment_quantities`: A numeric vector of the total number of observations assigned to each
#' treatment arm.
#' \item `means`: A `tibble` or `data.table` containing the mean estimates of the specified estimators for
#' each treatment arm.
#' \item `contrasts`: A `tibble` or `data.table` containing the contrast estimates of the specified estimators for
#' the specified contrast structure.
#' #' \item `f_stats`: Named numeric vector containing f_stat from IPW and OLS regressions.
#' #' \item `models`: A nested list containing the `lm_robust` objects from regressions, only saved
#' in clustered case.
#' \item `call`: `NULL`; initialized for later assignment.
#' \item `args`: `NULL`; initialized for later assignment.
#' \item `furrr`: `NULL`; initialized for later assignment.
#' }
#' @keywords internal
#' @family simulation

run_mab <- function(
  data,
  sim_type,
  estimators = c("aipw", "ipw", "ols"),
  p = NULL,
  algorithm,
  control_augment,
  random_assign_prop,
  prior_periods,
  discount_rate,
  simulate_dates = NULL,
  delayed_feedback,
  whole_experiment = NULL,
  conditions,
  blocking,
  clustering,
  col_names,
  imputation_information = NULL,
  verbose,
  ndraws,
  period_idxs,
  keep_data,
  keep_models,
  contrasts,
  time_model = NULL,
  time_model_args = NULL
) {
  verbose_log(verbose, "Starting Bandit Trial")
  periods <- length(period_idxs[[1]])
  num_conditions <- length(conditions)

  sim_results <- mab_loop(
    data = data,
    sim_type = sim_type,
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop,
    prior_periods = prior_periods,
    discount_rate = discount_rate,
    whole_experiment = whole_experiment,
    simulate_dates = simulate_dates,
    delayed_feedback = delayed_feedback,
    num_conditions = num_conditions,
    conditions = conditions,
    blocking = blocking,
    clustering = clustering,
    col_names = col_names,
    imputation_information = imputation_information,
    verbose = verbose,
    ndraws = ndraws,
    period_idxs = period_idxs,
    periods = periods,
    p = p,
    time_model = time_model,
    time_model_args = time_model_args
  )

  verbose_log(verbose, "Computing final simulation estimates")

  num_clusters <- if (clustering) {
    length(unique(sim_results[["final_data"]][["cluster"]]))
  } else {
    NULL
  }

  contrasts_list <- if (!is.null(contrasts)) {
    build_contrast_matrices(
      conditions = conditions,
      contrasts = contrasts,
      bandits = sim_results[["bandits"]]
    )
  } else {
    NULL
  }
  dt <- data.table::is.data.table(sim_results[["final_data"]])

  aw_aipw_estimates <- if ("aipw" %in% estimators) {
    iaipw <- compute_iaipw(
      data = sim_results[["final_data"]],
      assignment_probs = sim_results[["assignment_probs"]],
      conditions = conditions,
      periods = periods
    )
    means <- estimate_aw_aipw(
      data = sim_results[["final_data"]],
      assignment_probs = sim_results[["assignment_probs"]],
      iaipw = iaipw,
      periods = periods,
      conditions = conditions,
      clustering = clustering,
      cluster_col = col_names[["cluster_col"]],
      num_clusters = num_clusters
    )
    contrasts <- compute_contrast(
      C = contrasts_list,
      coefs = as_named_vec(means, "mean", "mab_condition"),
      vcov = diag(means[["se"]]^2),
      df = unique(means[["df"]]),
      estimator = "AW-AIPW",
      dt = dt,
      conditions = conditions
    )
    list(
      means = fill_missing_conditions(means, conditions, "AW-AIPW"),
      contrasts = contrasts
    )
  }

  ipw_estimates <- if ("ipw" %in% estimators) {
    estimate_lm_bundle(
      ipw = TRUE,
      estimator = "IPW",
      conditions = conditions,
      sim_results = sim_results,
      col_names = col_names,
      clustering = clustering,
      num_clusters = num_clusters,
      dt = dt,
      contrasts_list = contrasts_list
    )
  }
  ols_estimates <- if ("ols" %in% estimators) {
    estimate_lm_bundle(
      ipw = FALSE,
      estimator = "OLS",
      conditions = conditions,
      sim_results = sim_results,
      col_names = col_names,
      clustering = clustering,
      num_clusters = num_clusters,
      dt = dt,
      contrasts_list = contrasts_list
    )
  }
  estimates <- lapply(
    list(means = "means", contrasts = "contrasts"),
    \(item) {
      combine_estimates(
        aw_aipw_estimates[[item]],
        ipw_estimates[[item]],
        ols_estimates[[item]]
      )
    }
  )
  models <- if (keep_models) {
    list(
      ipw = ipw_estimates[["model"]],
      ols = ols_estimates[["model"]]
    )
  }
  final_data <- if (keep_data) sim_results[["final_data"]] else NULL

  results <- list(
    final_data = final_data,
    bandits = sim_results[["bandits"]],
    assignment_probs = sim_results[["assignment_probs"]],
    assignment_quantities = sim_results[["assignment_quantities"]],
    means = estimates[["means"]],
    contrasts = estimates[["contrasts"]],
    f_stats = c(
      IPW = ipw_estimates[["f_stat"]],
      OLS = ols_estimates[["f_stat"]]
    ),
    models = models,
    args = NULL,
    call = NULL,
    furrr = NULL
  )
  return(results)
}
#' @name condense_results
#' @title Condenses results of repeated simulations.
#' @inheritParams mab_from_rct
#' @param dt Logical; Whether to output `data.table`s or `tibble`s. When` r * number_of_periods > 100000`, `dt = TRUE`, even if the user passed data is not a
#' `data.table`.
#' @param mabs List of outputs from repeated [run_mab()] calls.
#' @returns A named list containing:
#' \itemize{
#' \item `final_data`: `tibble` or `data.table` containing the nested `tibble`s/`data.table`s from each trial. Only provided when `keep_data = TRUE`.
#' \item `bandits`: A `tibble` or `data.table` containing the UCB1 values or Thompson Sampling posterior distributions for each period and trial. Wide format,
#' each row is a period, and each column is a treatment.
#' \item `assignment_probs`: A `tibble` or `data.table` containing the probability of being assigned each treatment arm at a given period and trial. Wide format,
#' each row is a period, and each column is a treatment.
#' \item `assignment_quantities`: A `tibble` or `data.table` containing total number of observations assigned to each
#' treatment arm in each trial.
# \item `means`: A `tibble` or `data.table` containing the mean estimates of the specified estimators for
#' each treatment arm in each trial.
#' \item `contrasts`: A `tibble` or `data.table` containing the contrast estimates of the specified estimators for
#' the specified contrast structure for each trial.
#' \item `f_stats`: Named `tibble` or `data.table` containing f_stat from IPW and OLS regressions
#' for each trial.
#' \item `models`: A nested list containing the `lm_robust` objects from regressions, only saved
#' in clustered case, and when `keep_models = TRUE`
#' }
#' @details This function iterates over every element in `mabs` and extracts the required element to place in a condensed list
#' for the final output.
#' @keywords internal
condense_results <- function(dt, keep_data, keep_models, mabs) {
  r <- length(mabs)
  names(mabs) <- as.character(1:r)
  elements <- c(
    "bandits",
    "assignment_probs",
    "assignment_quantities",
    "means",
    "f_stats"
  )
  elements <- if (is.null(mabs[[1]][["contrasts"]])) {
    elements
  } else {
    c(elements, "contrasts")
  }

  extract <- \(item) lapply(mabs, `[[`, item)

  bind_dt <- \(item) {
    if (item %in% c("assignment_quantities", "f_stats")) {
      data.table::rbindlist(
        extract(item) |> lapply(as.list),
        idcol = "trial",
        use.names = TRUE
      )[, trial := as.numeric(trial)]
    } else {
      data.table::rbindlist(
        extract(item),
        idcol = "trial",
        use.names = TRUE
      )[, trial := as.numeric(trial)]
    }
  }

  bind_df <- \(item) {
    extract(item) |>
      dplyr::bind_rows(.id = "trial") |>
      dplyr::mutate(trial = as.numeric(trial))
  }

  bind_func <- if (dt) bind_dt else bind_df
  nest_func <- if (dt) {
    \() {
      data.table::data.table(
        trial = seq_len(r),
        data = list(extract("final_data"))
      )
    }
  } else {
    \() tibble::tibble(trial = seq_len(r), data = extract("final_data"))
  }
  results <- lapply(elements, bind_func)
  names(results) <- elements
  results[["final_data"]] <- if (keep_data) nest_func() else NULL

  results[["models"]] <- if (keep_models) {
    lapply(
      c(ipw = "ipw", ols = "ols"),
      \(estimator) {
        lapply(mabs, \(mab) mab[["models"]][[estimator]])
      }
    )
  } else {
    NULL
  }

  return(results)
}
