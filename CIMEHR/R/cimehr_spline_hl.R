CIMEHR_spline_HL <- function(
    data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    outcome_fixed_structure = NULL, covars_outcome_random_link_ZY = NULL,
    outcome_random_structure = NULL, outcome_random_intercept = TRUE,
    covars_obs_fixed_XO = NULL, covars_obs_random_link_ZO = NULL,
    trajectory_covars = NULL, spline_type = c("natural", "bspline"),
    baseline_df = 5L, trajectory_df = 3L, baseline_knots = NULL,
    trajectory_knots = NULL, boundary_knots = NULL, reference_time = NULL,
    spline_degree = 3L, stage3_chunk_size = 100000L,
    inference = c("stacked", "conditional", "none"), nuisance_fit = NULL,
    retain_nuisance_fit = FALSE, retain_influence = FALSE, entry_col = NULL, ...) {
  spline_type <- match.arg(spline_type)
  inference <- match.arg(inference)
  data <- as.data.frame(data)
  .cimehr_check_numeric_columns(data, c(covars_visit_XV, covars_outcome_fixed_XY,
    covars_outcome_random_link_ZY, covars_obs_fixed_XO, covars_obs_random_link_ZO,
    trajectory_covars))
  .validate_data(data, id_col, time_col, y_col, r_col,
    unique(c(covars_visit_XV, covars_outcome_fixed_XY, covars_outcome_random_link_ZY,
      covars_obs_fixed_XO, covars_obs_random_link_ZO, trajectory_covars)), entry_col, censor_col)
  curve_refit_data <- data
  masked <- .mask_entry_visits(data, id_col, time_col, y_col, r_col, entry_col)
  data <- masked$data
  engine <- .cimehr_hl_engine()
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  ans <- withCallingHandlers(engine$.cimehr_spline_fit(
    data = data, id_col = id_col, time_col = time_col, y_col = y_col, r_col = r_col,
    censor_col = censor_col, entry_col = entry_col, covars_visit_XV = covars_visit_XV,
    covars_outcome_fixed_XY = covars_outcome_fixed_XY,
    outcome_fixed_structure = outcome_fixed_structure,
    covars_outcome_random_link_ZY = covars_outcome_random_link_ZY,
    outcome_random_structure = outcome_random_structure,
    outcome_random_intercept = outcome_random_intercept,
    covars_obs_fixed_XO = covars_obs_fixed_XO,
    covars_obs_random_link_ZO = covars_obs_random_link_ZO,
    trajectory_covars = trajectory_covars, spline_type = spline_type,
    baseline_df = baseline_df, trajectory_df = trajectory_df,
    baseline_knots = baseline_knots, trajectory_knots = trajectory_knots,
    boundary_knots = boundary_knots, reference_time = reference_time,
    spline_degree = spline_degree, stage3_chunk_size = stage3_chunk_size,
    inference = inference, nuisance_fit = nuisance_fit,
    retain_nuisance_fit = retain_nuisance_fit, retain_influence = retain_influence, ...),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  elapsed <- proc.time()[["elapsed"]] - started
  ans <- .cimehr_tag_fit(ans,'cimehr_spline_hl')
  ans <- .normalize_fit(ans, 'cimehr_spline_hl', elapsed, warnings)
  ans$diagnostics$n_visits_at_entry_excluded <- masked$n
  ans$call <- match.call()
  ans$parameter_table <- .outcome_table(ans)
  class(ans) <- c("cimehr_hl_fit", "cimehr_spline_fit", "cimehr_fit", "list")
  ans$refit <- .curve_capture_refit(curve_refit_data, list(
    method = "cimehr_spline_hl", id = id_col, time = time_col, outcome = y_col,
    obs_indicator = r_col, censor_col = censor_col, entry_col = entry_col,
    visit_covs = covars_visit_XV, cimehr_visit_covs = covars_visit_XV,
    long_covs = covars_outcome_fixed_XY,
    outcome_random_covs = covars_outcome_random_link_ZY,
    obs_random_covs = if(is.null(covars_obs_random_link_ZO))character() else covars_obs_random_link_ZO,
    obs_fixed_covs = covars_obs_fixed_XO,
    outcome_fixed_structure = outcome_fixed_structure,
    outcome_random_structure = outcome_random_structure,
    outcome_random_intercept = outcome_random_intercept,
    focal = if(length(covars_outcome_fixed_XY))covars_outcome_fixed_XY[1L] else NULL,
    seed = 1L,
    controls = c(list(trajectory_covars = trajectory_covars, spline_type = spline_type,
      baseline_df = baseline_df, trajectory_df = trajectory_df,
      baseline_knots = baseline_knots, trajectory_knots = trajectory_knots,
      boundary_knots = boundary_knots, reference_time = reference_time,
      spline_degree = spline_degree, stage3_chunk_size = stage3_chunk_size,
      inference = inference, nuisance_fit = nuisance_fit,
      retain_nuisance_fit = retain_nuisance_fit, retain_influence = retain_influence),
      list(...))))
  ans$diagnostics$object_bytes <- as.numeric(object.size(ans))
  .cimehr_signal_identification_warning(ans)
  ans
}
