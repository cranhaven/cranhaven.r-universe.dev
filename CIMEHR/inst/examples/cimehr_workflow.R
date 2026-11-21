# Complete CIMEHR workflow using the packaged simulated data.
# The generating seed and parameters are in cimehr_example$truth.
# Export files with export_results(directory).

# load-data
library(CIMEHR)
data("cimehr_example", package = "CIMEHR")
participants <- cimehr_example$participants
visits <- cimehr_example$visits
truth <- cimehr_example$truth
cimehr_example$truth[c("seed", "n")]
head(participants)
head(visits)

# prepare-data
stopifnot(!anyDuplicated(participants$id),
          all(visits$id %in% participants$id),
          all(participants$Z %in% c(0, 1)))
original_Z <- participants$Z
participants$group <- factor(participants$Z, levels = c(0, 1),
                             labels = c("Reference", "Exposed"))
group_matrix <- model.matrix(~ group, data = participants)
participants$Z <- unname(group_matrix[, "groupExposed"])
stopifnot(all(participants$Z == original_Z))

analysis_data <- merge(participants, visits, by = "id", all.x = TRUE,
                       sort = FALSE)
analysis_data <- analysis_data[order(analysis_data$id, analysis_data$time,
                                    na.last = TRUE), ]
rownames(analysis_data) <- NULL

no_visit <- is.na(analysis_data$time)
stopifnot(all(is.na(analysis_data$R[no_visit])),
          all(is.na(analysis_data$Y[no_visit])),
          all(analysis_data$R[!no_visit] %in% c(0, 1)))
measured <- !no_visit & analysis_data$R == 1
unmeasured_visit <- !no_visit & analysis_data$R == 0
stopifnot(all(is.finite(analysis_data$Y[measured])),
          all(is.na(analysis_data$Y[unmeasured_visit])))

sample_counts <- data.frame(
  eligible_participants = nrow(participants),
  participants_with_visits = length(unique(visits$id)),
  participants_with_measurements = length(unique(analysis_data$id[measured])),
  clinic_visits = nrow(visits),
  recorded_measurements = sum(measured)
)
sample_counts

# fit-models
method_keys <- c("cimehr", "cimehr_hl", "cimehr_spline", "cimehr_spline_hl")
method_labels <- setNames(c("CIMEHR", "CIMEHR-HL", "CIMEHR-spline",
                            "CIMEHR-spline-HL"), method_keys)
model_arguments <- list(
  data = analysis_data,
  id = "id", time = "time", outcome = "Y",
  obs_indicator = "R", censor_col = "C",
  visit_covs = c("Z", "X"),
  cimehr_visit_covs = c("Z", "X"),
  obs_fixed_covs = c("Z", "X"),
  long_covs = c("Z", "X"),
  outcome_random_covs = "Z", obs_random_covs = "Z",
  outcome_random_intercept = TRUE
)

fits <- setNames(lapply(method_keys, function(key) {
  do.call(fit_method, c(list(method = key), model_arguments))
}), method_keys)

# outcome-results
outcome_results <- do.call(rbind, lapply(names(fits), function(key) {
  tab <- summary_outcome(fits[[key]])
  theta0 <- if ("theta.Intercept" %in% tab$parameter)
    "theta.Intercept" else "theta.(Intercept)"
  at <- match(c("beta.Z", "beta.X", theta0, "theta.Z"), tab$parameter)
  stopifnot(!anyNA(at))
  tab <- tab[at, ]
  data.frame(
    method = unname(method_labels[key]),
    parameter = c("beta_Z", "beta_X", "theta_0", "theta_Z"),
    truth = as.numeric(c(truth$beta[c("Z", "X")], truth$theta[c("b0", "bZ")])),
    estimate = tab$estimate, se = tab$se, lower = tab$lower, upper = tab$upper,
    inference_valid = tab$inference_valid, row.names = NULL
  )
}))
outcome_display <- data.frame(
  Method = outcome_results$method, Parameter = outcome_results$parameter,
  Truth = outcome_results$truth, Estimate = outcome_results$estimate,
  SE = outcome_results$se,
  `95% CI` = sprintf("(%.4f, %.4f)", outcome_results$lower,
                    outcome_results$upper), check.names = FALSE
)
knitr::kable(outcome_display, digits = 4, row.names = FALSE,
             caption = paste("Outcome fixed-effect and shared-loading estimates.",
                             "SEs and 95% CIs use the fitted stacked covariance."))

# stage-results
all_stage_results <- do.call(rbind, lapply(names(fits), function(key) {
  do.call(rbind, lapply(c("visiting", "observation", "outcome"), function(stage) {
    tab <- switch(stage,
      visiting = summary_visiting(fits[[key]]),
      observation = summary_observation(fits[[key]]),
      outcome = summary_outcome(fits[[key]]))
    data.frame(method = unname(method_labels[key]), stage = stage,
      tab[, c("parameter", "estimate", "se", "lower", "upper",
              "inference_valid", "inference_status")], row.names = NULL)
  }))
}))
summary_visiting(fits[["cimehr"]])
summary_observation(fits[["cimehr"]])
confint(fits[["cimehr"]])

# inspect-diagnostics
fit <- fits[["cimehr"]]
outcome_diagnostics <- summary_outcome(fit)
outcome_diagnostics[, c("parameter", "ci_available", "inference_valid",
                        "inference_status", "inference_reason")]
fit$diagnostics$warnings

# point-curves
time_grid <- seq(0, 60, length.out = 121L)
profiles <- data.frame(Z = c(0, 1), X = c(0, 0))
profiles

baseline_curves <- lapply(fits, meanfit, type = "baseline", times = time_grid)
profile_curves <- lapply(fits, meanfit, newdata = profiles, times = time_grid)
head(as.data.frame(baseline_curves[["cimehr"]]))
head(as.data.frame(profile_curves[["cimehr"]]))

# plot-profiles
plot(profile_curves[["cimehr"]], xlab = "Months since baseline",
     ylab = "Population mean biomarker (arbitrary units)")

# standardized-curve
reference_profiles <- participants[, c("Z", "X")]
standardized <- meanfit(fits[["cimehr"]], newdata = reference_profiles,
                        aggregate = TRUE, times = time_grid)
head(summary(standardized))

# spline-intervals
spline_intervals <- meanfit(fits[["cimehr_spline"]], newdata = profiles,
                            times = time_grid, se.fit = TRUE, conf.int = .95)
head(summary(spline_intervals))

# bootstrap-intervals
B <- 50L
bootstrap_seed <- 20261014L
profile_weights <- c(.5, .5)
population_intercept <- truth$baseline_intercept +
  sum(profile_weights * (truth$beta["Z"] * profiles$Z +
                         truth$beta["X"] * profiles$X))
bootstrap_curves <- list()
curve_rows <- list()
bootstrap_rows <- list()
for (key in names(fits)) {
  for (type in c("baseline", "population")) {
    args <- list(object = fits[[key]], type = type, times = time_grid,
                 bandwidth = 12, bootstrap = TRUE, B = B,
                 seed = bootstrap_seed, workers = 1L, conf.int = .95)
    if (type == "population") {
      args$newdata <- profiles
      args$aggregate <- TRUE
      args$weights <- profile_weights
    }
    curve <- do.call(meanfit, args)
    name <- paste(key, type)
    bootstrap_curves[[name]] <- curve
    tab <- as.data.frame(curve)
    tab$method <- unname(method_labels[key])
    tab$type <- type
    intercept <- if (type == "baseline") truth$baseline_intercept else
      population_intercept
    tab$truth <- intercept + truth$baseline_slope * tab$time
    curve_rows[[name]] <- tab
    bootstrap_rows[[name]] <- data.frame(
      method = unname(method_labels[key]), type = type, B = B,
      successful = curve$bootstrap$successful_fits, n_points = nrow(tab),
      min_n_boot = min(tab$n_boot),
      max_absolute_curve_error = max(abs(tab$fit - tab$truth)),
      all_finite_CI = all(is.finite(tab$lower) & is.finite(tab$upper))
    )
  }
}
curve_results <- do.call(rbind, curve_rows)
curve_results$method <- factor(curve_results$method,
                               levels = unname(method_labels))
bootstrap_summary <- do.call(rbind, bootstrap_rows)

# bootstrap-diagnostics
knitr::kable(bootstrap_summary[, c("method", "type", "B", "successful",
                                  "n_points", "min_n_boot", "all_finite_CI")],
             row.names = FALSE,
             col.names = c("Method", "Curve", "Resamples", "Successful fits",
                           "Time points", "Minimum usable resamples",
                           "Finite intervals"))

# comparison-plot-function
library(ggplot2)
comparison_plot <- function(type) {
  tab <- curve_results[curve_results$type == type, ]
  ggplot(tab, aes(x = time)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#7B3294", alpha = .17) +
    geom_line(aes(y = fit, colour = "Estimate", linetype = "Estimate"),
              linewidth = .85) +
    geom_line(aes(y = truth, colour = "True curve", linetype = "True curve"),
              linewidth = .7) +
    facet_wrap(~ method, ncol = 2) +
    scale_colour_manual(values = c("Estimate" = "#7B3294",
                                   "True curve" = "#111111"), name = NULL) +
    scale_linetype_manual(values = c("Estimate" = "solid",
                                     "True curve" = "22"), name = NULL) +
    scale_x_continuous(breaks = seq(0, 60, by = 12),
                       expand = expansion(mult = c(.02, .02))) +
    scale_y_continuous(breaks = seq(-2, 4, by = 1)) +
    labs(x = "Months since baseline",
         y = if (type == "baseline") "Baseline mean outcome" else
           "Population-average mean outcome") +
    theme_classic(base_size = 12, base_family = "sans") +
    theme(legend.position = "right", legend.key.width = grid::unit(1, "cm"),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          panel.spacing = grid::unit(1.2, "lines"),
          plot.margin = margin(10, 10, 10, 10))
}
baseline_figure <- comparison_plot("baseline")
population_figure <- comparison_plot("population")

# baseline-comparison
baseline_figure

# population-comparison
population_figure

# installed-results
results_directory <- system.file("examples", "results", package = "CIMEHR")
list.files(results_directory)
system.file("examples", "cimehr_workflow.R", package = "CIMEHR")

# export-results
export_results <- function(directory) {
  stopifnot(is.character(directory), length(directory) == 1L,
            !is.na(directory), nzchar(directory))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("The output directory could not be created.")
  write.csv(outcome_results, file.path(directory, "outcome_estimates.csv"),
             row.names = FALSE)
  write.csv(all_stage_results, file.path(directory, "all_stage_estimates.csv"),
             row.names = FALSE)
  write.csv(curve_results, file.path(directory, "baseline_population_curves.csv"),
             row.names = FALSE)
  write.csv(bootstrap_summary, file.path(directory, "bootstrap_summary.csv"),
             row.names = FALSE)
  ggsave(file.path(directory, "baseline_95CI.pdf"), plot = baseline_figure,
         width = 9.4, height = 6.3, device = grDevices::pdf, useDingbats = FALSE)
  ggsave(file.path(directory, "population_trajectory_95CI.pdf"),
         plot = population_figure, width = 9.4, height = 6.3,
         device = grDevices::pdf, useDingbats = FALSE)
  invisible(list.files(directory, full.names = TRUE))
}
