test_that("results$plot error handling works as expected", {
  res <- results(size_coef = 2, size_coef_degrees = 0)

  # At least one must be TRUE
  expect_error(res$plot(), "At least one of `stats`, `trace`, or `model_assessment` must be TRUE.")

  # Stats with no samples
  expect_error(res$plot(stats = TRUE), "No samples available to plot.")

  # Trace when not estimated
  expect_error(res$plot(trace = TRUE), "Model has not been estimated yet. Cannot plot results.")

  # Model assessment when not available
  expect_error(res$plot(model_assessment = TRUE), "No model assessment available to plot.")
})

test_that("results$plot works for trace, stats, and model assessment on undirected model", {
  n_units <- 20
  block <- matrix(nrow = 5, ncol = 5, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 5, block, simplify = FALSE)))

  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = FALSE, family_x = "binomial", family_y = "binomial")
  gt_coef <- c(1, -1, -1)
  gt_coef_pop <- rnorm(n = n_units, -2, 1)

  sampler_obj <- sampler.iglm(
    n_burn_in = 2, n_simulation = 5,
    sampler_x = sampler.net.attr(n_proposals = n_units * 2),
    sampler_y = sampler.net.attr(n_proposals = n_units * 2),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 2),
    init_empty = FALSE
  )

  model_fit <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y + attribute_x + degrees,
    coef = gt_coef, coef_degrees = gt_coef_pop, sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )

  model_fit$simulate()
  model_fit$set_target(model_fit$get_samples()[[1]])
  model_fit$estimate()

  # Test trace plot
  pdf(NULL)
  expect_silent(model_fit$results$plot(trace = TRUE))
  dev.off()

  # Test stats plot
  pdf(NULL)
  expect_silent(model_fit$results$plot(stats = TRUE))
  dev.off()

  # Test single-model assessment plot (undirected)
  model_fit$assess(
    formula = ~ degree_distribution + geodesic_distances_distribution +
      edgewise_shared_partner_distribution + dyadwise_shared_partner_distribution +
      y_distribution + x_distribution,
    plot = FALSE
  )

  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()

  # Test multi-model comparison plot
  assess_copy <- model_fit$results$model_assessment
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE, assess_copy))
  dev.off()

  # Test MCMC diagnostics
  model_fit$assess(formula = ~ degree_distribution + mcmc_diagnostics, plot = FALSE)
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()
})

test_that("results$plot works for directed model with in/out degrees and continuous attributes", {
  n_units <- 15
  block <- matrix(nrow = 5, ncol = 5, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 5, block, simplify = FALSE)))

  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = TRUE, family_x = "normal", family_y = "normal")
  gt_coef <- c(1, -0.5, -0.5)

  sampler_obj <- sampler.iglm(
    n_burn_in = 2, n_simulation = 2,
    sampler_x = sampler.net.attr(n_proposals = n_units * 2),
    sampler_y = sampler.net.attr(n_proposals = n_units * 2),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 2),
    init_empty = FALSE
  )

  model_fit <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y + attribute_x,
    coef = gt_coef, sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )

  model_fit$simulate()
  model_fit$set_target(model_fit$get_samples()[[1]])
  model_fit$estimate()

  # Trace without degrees
  pdf(NULL)
  expect_silent(model_fit$results$plot(trace = TRUE))
  dev.off()

  model_fit$assess(
    formula = ~ degree_distribution + y_distribution + x_distribution,
    plot = FALSE
  )

  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()

  # Multi-model comparison with normal distributions
  assess_copy <- model_fit$results$model_assessment
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE, assess_copy))
  dev.off()

  # Custom constrained degree parameters in assess formula
  model_fit$assess(
    formula = ~ degree_distribution(x_i = 1, y_j = 0) +
      degree_distribution(x_i = 0, y_j = 1),
    plot = FALSE
  )
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()

  # Multi-model comparison with multiple custom degree terms
  assess_custom_copy <- model_fit$results$model_assessment
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE, assess_custom_copy))
  dev.off()

  # Multi-model comparison with distinct second model fit
  model_fit2 <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y + attribute_x,
    coef = c(0.2, -0.1, -0.1), sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )
  model_fit2$simulate()
  model_fit2$set_target(model_fit$get_samples()[[1]])
  model_fit2$estimate()

  res2 <- model_fit2$assess(
    formula = ~ degree_distribution + geodesic_distances_distribution + y_distribution,
    plot = FALSE
  )
  res1 <- model_fit$assess(
    formula = ~ degree_distribution + geodesic_distances_distribution + y_distribution,
    plot = FALSE
  )
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE, res2))
  dev.off()
  # Constrained degree distributions in assess formula
  model_fit$assess(
    formula = ~ degree_distribution(x_i = 1, x_j = 1) +
      degree_distribution(y_i = 1, y_j = 0),
    plot = FALSE
  )
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()
})

test_that("Continuous attributes work with degree_distribution and plot methods", {
  z <- matrix(c(
    0, 1, 1, 0, 0,
    1, 0, 1, 0, 1,
    1, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    0, 1, 0, 1, 0
  ), nrow = 5, byrow = TRUE)
  x <- c(1.2, -0.5, 0.8, -1.1, 0.4)
  y <- c(-0.3, 0.9, -0.7, 1.4, -0.1)
  data_obj <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = 5, family_x = "normal", family_y = "normal", directed = FALSE)

  pdf(NULL)
  expect_no_error(data_obj$degree_distribution(x_i = function(v) v > 0, plot = TRUE))
  expect_no_error(data_obj$degree_distribution(x_i = c(1, 2), plot = TRUE))
  dev.off()
})

test_that("Trace plot works on pure degree models", {
  n_units <- 5
  z <- matrix(0, 5, 5)
  z[1, 2] <- z[2, 1] <- z[3, 4] <- z[4, 3] <- 1
  data_obj <- iglm.data(z_network = z, n_units = 5, directed = FALSE, family_x = "binomial", family_y = "binomial")

  model_fit <- iglm(
    formula = data_obj ~ degrees,
    coef_degrees = rep(-1, 5),
    sampler = sampler.iglm(n_burn_in = 2, n_simulation = 4, init_empty = FALSE),
    control = control.iglm(max_it = 2, display_progress = FALSE)
  )
  model_fit$simulate()
  model_fit$set_target(model_fit$get_samples()[[1]])
  model_fit$estimate()

  pdf(NULL)
  expect_silent(model_fit$results$plot(trace = TRUE))
  dev.off()
})

test_that("results$plot handles asymmetric constrained in and out degree distributions properly", {
  n_units <- 8
  z_asym <- matrix(0, 8, 8)
  z_asym[1, 2] <- z_asym[1, 3] <- z_asym[1, 4] <- z_asym[1, 5] <- 1
  d_asym <- iglm.data(
    x_attribute = c(1, 0, 0, 0, 0, 0, 0, 0),
    y_attribute = c(0, 1, 1, 1, 1, 0, 0, 0),
    z_network = z_asym,
    n_units = 8,
    directed = TRUE
  )

  fit <- iglm(
    formula = d_asym ~ edges(mode = "global"),
    sampler = sampler.iglm(n_burn_in = 2, n_simulation = 4, init_empty = FALSE),
    control = control.iglm(max_it = 2, display_progress = FALSE)
  )
  fit$simulate()

  asss <- fit$assess(formula = ~ degree_distribution(x_i = 1, y_j = 1), plot = FALSE)
  asss_name <- names(asss$observed)[1]
  expect_equal(names(asss$observed[[asss_name]]$out_degree), as.character(0:4))
  expect_equal(names(asss$observed[[asss_name]]$in_degree), as.character(0:1))

  pdf(NULL)
  expect_silent(fit$results$plot(model_assessment = TRUE))
  dev.off()
})

test_that("Constrained degree labels include x and y constraints", {
  lab_def_bin <- build_constrained_xlab("Indegree", x_i = 1, y_j = 1, family_x = "binomial", family_y = "binomial")
  expect_true(grepl("x\\[i\\] == 1", paste(deparse(lab_def_bin), collapse = " ")))
  expect_true(grepl("y\\[j\\] == 1", paste(deparse(lab_def_bin), collapse = " ")))

  lab_def_norm <- build_constrained_xlab("Indegree", x_i = 1, y_j = 1, family_x = "normal", family_y = "normal")
  expect_true(grepl("x\\[i\\] > bar\\(x\\)", paste(deparse(lab_def_norm), collapse = " ")))
  expect_true(grepl("y\\[j\\] > bar\\(y\\)", paste(deparse(lab_def_norm), collapse = " ")))

  lab_assess_custom <- get_assessment_constraint_xlab("Indegree", "degree_distribution_x_i_0,y_j_1", "degree_distribution", family_x = "binomial", family_y = "binomial")
  expect_true(grepl("x\\[i\\] == 0", paste(deparse(lab_assess_custom), collapse = " ")))
  expect_true(grepl("y\\[j\\] == 1", paste(deparse(lab_assess_custom), collapse = " ")))
})

test_that("Multi-model comparison plot handles geodesic distance distributions with Inf and different supports", {
  n_units <- 10
  neighborhood <- matrix(1, nrow = n_units, ncol = n_units)

  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = FALSE)
  sampler_obj <- sampler.iglm(n_burn_in = 2, n_simulation = 3, init_empty = FALSE)

  mod1 <- iglm(
    formula = xyz_obj ~ edges(mode = "local"),
    coef = c(-1), sampler = sampler_obj
  )
  mod1$simulate(display_progress = FALSE)
  mod1$assess(formula = ~ geodesic_distances_distribution, plot = FALSE)

  # Create a second assessment result with different columns/supports including Inf
  assess2 <- mod1$results$model_assessment
  assess2$simulated <- lapply(assess2$simulated, function(s) {
    s$geodesic_distances_distribution <- c("1" = 0.2, "2" = 0.3, "3" = 0.2, "4" = 0.1, "Inf" = 0.2)
    s
  })

  pdf(NULL)
  expect_silent(mod1$results$plot(model_assessment = TRUE, assess2))
  dev.off()
})

test_that("extract_assessment_matrix aligns varying supports with zero-filling", {
  # Discrete distributions with differing supports and Inf
  sim_list_discrete <- list(
    list(geo = c("1" = 0.5, "2" = 0.3, "Inf" = 0.2)),
    list(geo = c("1" = 0.4, "2" = 0.2, "3" = 0.2, "Inf" = 0.2)),
    list(geo = c("2" = 0.5, "4" = 0.1, "Inf" = 0.4))
  )
  mat_discrete <- extract_assessment_matrix(sim_list_discrete, "geo")
  expect_equal(colnames(mat_discrete), c("1", "2", "3", "4", "Inf"))
  expect_equal(nrow(mat_discrete), 3)
  expect_equal(as.numeric(mat_discrete[1, "3"]), 0)
  expect_equal(as.numeric(mat_discrete[3, "1"]), 0)
  expect_equal(as.numeric(mat_discrete[2, "3"]), 0.2)
})

test_that("Single-model assessment plot handles geodesic distributions when simulation support differs from observed", {
  obs_geo <- c("1" = 0.6, "2" = 0.2, "Inf" = 0.2)
  sim_geo <- matrix(
    c(0.4, 0.3, 0.1, 0.2,
      0.5, 0.2, 0.1, 0.2),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("1", "2", "3", "Inf"))
  )
  x_pos <- seq_along(obs_geo)
  x_labs <- names(obs_geo)

  pdf(NULL)
  expect_silent(
    plot_assessment_single(
      observed = obs_geo, sim_matrix = sim_geo, xlab = "Geodesic Distance",
      x_positions = x_pos, x_at = x_pos, x_labels = x_labs
    )
  )
  dev.off()
})

test_that("Normal y_distribution assessment preserves continuous densities without zero-filling artifacts", {
  x_grid <- seq(-3, 3, length.out = 100)
  obs_density <- dnorm(x_grid)
  names(obs_density) <- x_grid
  sim_densities <- matrix(
    rep(dnorm(x_grid, sd = 1.1), each = 5),
    nrow = 5, ncol = 100, byrow = FALSE,
    dimnames = list(NULL, as.character(x_grid))
  )

  pdf(NULL)
  expect_silent(
    plot_assessment_single(
      observed = obs_density, sim_matrix = sim_densities, xlab = "Distribution of Y",
      ylab = "Density", x_positions = x_grid, x_at = pretty(x_grid), x_labels = pretty(x_grid),
      use_envelope = TRUE
    )
  )
  expect_silent(
    plot_assessment_multi(
      observed = obs_density, sim_main = sim_densities, sim_dots = list(sim_densities),
      model_names = c("Model 1", "Model 2"), colors = c("blue", "red"),
      xlab = "Distribution of Y", ylab = "Density", x_positions = x_grid,
      x_at = pretty(x_grid), x_labels = pretty(x_grid), use_envelope = TRUE
    )
  )
  dev.off()
})

test_that("Log-likelihood plot handles very negative values without error and restores margins", {
  res <- results(size_coef = 2, size_coef_degrees = 0)
  res$update(
    llh = c(-1543210.45, -1200000, -800000, -500000, -400000),
    coefficients_path = matrix(1:10, ncol = 2),
    estimated = TRUE
  )

  pdf(NULL)
  mar_before <- par("mar")
  expect_silent(res$plot(trace = TRUE))
  mar_after <- par("mar")
  expect_equal(mar_before, mar_after)
  dev.off()
})
