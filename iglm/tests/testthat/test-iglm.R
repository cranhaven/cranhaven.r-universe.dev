test_that("Define a iglm object, simulate, estimate, assess", {
  n_actor <- 100
  block <- matrix(nrow = 50, ncol = 50, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_actor / 50, block, simplify = FALSE)))

  overlapping_degree <- 0.5
  neighborhood <- matrix(nrow = n_actor, ncol = n_actor, data = 0)
  block <- matrix(nrow = 5, ncol = 5, data = 0)
  size_neighborhood <- 5
  size_overlap <- ceiling(size_neighborhood * overlapping_degree)

  end <- floor((n_actor - size_neighborhood) / size_overlap)
  for (i in 0:end) {
    neighborhood[(1 + size_overlap * i):(size_neighborhood + size_overlap * i), (1 + size_overlap * i):(size_neighborhood + size_overlap * i)] <- 1
  }
  neighborhood[(n_actor - size_neighborhood + 1):(n_actor), (n_actor - size_neighborhood + 1):(n_actor)] <- 1
  family_x <- "binomial"
  family_y <- "binomial"

  xyz_obj_new <- iglm.data(neighborhood = neighborhood, directed = FALSE, family_x = family_x, family_y = family_y)
  gt_coef <- c(3, -1, -1)
  gt_coef_pop <- c(rnorm(n = n_actor, -2, 1))

  sampler_new <- sampler.iglm(
    n_burn_in = 10, n_simulation = 1,
    sampler_x = sampler.net.attr(n_proposals = n_actor * 10),
    sampler_y = sampler.net.attr(n_proposals = n_actor * 10),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )

  expect_equal(inherits(sampler_new, "sampler.iglm"), expected = TRUE)

  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x + degrees,
    coef = gt_coef, coef_degrees = gt_coef_pop, sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )


  tmp_name <- paste0(tempfile(), ".rds")
  # debugonce(model_tmp_new$save)
  model_tmp_new$save(file = tmp_name)
  model_tmp_loaded <- iglm(file = tmp_name)

  expect_equal(inherits(model_tmp_loaded, "iglm.object"), expected = TRUE)
  expect_equal(length(model_tmp_loaded$results$samples), expected = 0)
  expect_equal(model_tmp_loaded$results$stats, expected = NULL)

  expect_equal(inherits(model_tmp_new, "iglm.object"), expected = TRUE)
  expect_equal(length(model_tmp_new$results$samples), expected = 0)
  expect_equal(model_tmp_new$results$stats, expected = NULL)

  model_tmp_new$simulate()

  expect_equal(length(model_tmp_new$results$samples), expected = 1)
  expect_equal(nrow(model_tmp_new$results$stats), expected = 1)
  expect_equal(model_tmp_new$iglm.data$mean_z(), expected = 0)


  expect_equal(inherits(model_tmp_loaded, "iglm.object"), expected = TRUE)
  expect_equal(length(model_tmp_loaded$results$samples), expected = 0)
  expect_equal(model_tmp_loaded$results$stats, expected = NULL)

  samples <- model_tmp_new$get_samples()
  model_tmp_new$set_target(samples[[1]])
  expect_equal(model_tmp_new$iglm.data$mean_z(),
    expected = nrow(samples[[1]]$z_network) / (n_actor * (n_actor - 1) / 2)
  )
  # debugonce(model_tmp_new$estimate)
  expect_error(model_tmp_new$estimate())


  sampler_est <- sampler.iglm(
    n_burn_in = 1, n_simulation = 10,
    sampler_x = sampler.net.attr(n_proposals = n_actor * 10),
    sampler_y = sampler.net.attr(n_proposals = n_actor * 10),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )

  model_tmp_new$set_sampler(sampler_est)
  expect_equal(model_tmp_new$sampler$n_burn_in, 1)
  # debugonce(model_tmp_new$estimate)
  model_tmp_new$estimate()
  expect_no_warning(model_tmp_new$estimate())
  # expect_equal(as.vector(round(model_tmp_new$coef)), round(gt_coef))
  expect_equal(length(model_tmp_new$results$model_assessment$observed), 0)
  model_tmp_new$assess(formula = ~degree_distribution, plot = FALSE)
  expect_equal(length(model_tmp_new$results$model_assessment$observed), 1)

  model_tmp_new$save(file = tmp_name)
  model_tmp_loaded <- iglm(file = tmp_name)
  model_tmp_new$results$model_assessment$observed

  # expect_equal(as.vector(round(model_tmp_loaded$coef)), round(gt_coef))
  expect_equal(length(model_tmp_loaded$results$model_assessment$observed), 1)

  file.remove(tmp_name)
})

test_that("iglm throws error when covariate object does not exist", {
  n_actor <- 4
  z <- matrix(0, n_actor, n_actor)
  x <- c(0, 1, 1, 0)
  y <- c(1, 0, 1, 0)
  data_obj <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = 4, family_x = "binomial", family_y = "binomial")

  expect_error(
    iglm(
      formula = data_obj ~ cov_x(data = non_existent_covariate_var),
      coef = c(1),
      sampler = sampler.iglm(n_burn_in = 2, n_simulation = 1, init_empty = FALSE)
    ),
    pattern = "Could not evaluate argument 'data' in term 'cov_x': object 'non_existent_covariate_var' not found"
  )

  expect_error(
    iglm(
      formula = data_obj ~ cov_x(non_existent_covariate_var),
      coef = c(1),
      sampler = sampler.iglm(n_burn_in = 2, n_simulation = 1, init_empty = FALSE)
    ),
    pattern = "Could not evaluate argument '..1' in term 'cov_x': object 'non_existent_covariate_var' not found"
  )

  expect_error(
    iglm(
      formula = data_obj ~ cov_x(data = stop("custom error message")),
      coef = c(1),
      sampler = sampler.iglm(n_burn_in = 2, n_simulation = 1, init_empty = FALSE)
    ),
    pattern = "Could not evaluate argument 'data' in term 'cov_x': custom error message"
  )

  # Finite coefficient checks
  expect_error(
    iglm(
      formula = data_obj ~ edges(mode = "local"),
      coef = c(NA),
      sampler = sampler.iglm(n_burn_in = 2, n_simulation = 1, init_empty = FALSE)
    ),
    pattern = "coef.*must contain finite numeric values"
  )

  # Empty formula RHS check
  expect_error(
    iglm(
      formula = data_obj ~ 1,
      coef = c(1)
    ),
    pattern = "Formula must contain at least one term"
  )

  # Control parameter bounds & type checks
  expect_error(
    control.iglm(max_it = 10.5),
    pattern = "`max_it` must be a positive integer"
  )
  expect_error(
    control.iglm(max_it = Inf),
    pattern = "`max_it` must be a positive integer"
  )
  expect_error(
    control.iglm(tol = Inf),
    pattern = "`tol` must be a positive number"
  )
  expect_error(
    control.iglm(offset_nonoverlap = Inf),
    pattern = "`offset_nonoverlap` must be a single numeric value"
  )

  # Pure degree model check
  m_degrees_only <- iglm(
    formula = data_obj ~ degrees,
    sampler = sampler.iglm(n_burn_in = 2, n_simulation = 2, init_empty = FALSE),
    control = control.iglm(max_it = 2, display_progress = FALSE, var_method = "Godambe")
  )
  expect_equal(inherits(m_degrees_only, "iglm.object"), TRUE)
  expect_no_error(m_degrees_only$estimate())
  expect_equal(dim(m_degrees_only$results$var), c(0, 0))

  # Pure degree model with Mean-value variance method
  m_degrees_updated <- iglm(
    formula = data_obj ~ degrees,
    sampler = sampler.iglm(n_burn_in = 2, n_simulation = 2, init_empty = FALSE),
    control = control.iglm(max_it = 2, display_progress = FALSE, var_method = "Mean-value")
  )
  expect_no_error(m_degrees_updated$estimate())
  expect_equal(dim(m_degrees_updated$results$var), c(0, 0))

  # Test assess removes non-distribution terms like geodesic_distances, dsp, geo, geodist, esp
  expect_warning(
    m_degrees_updated$assess(formula = ~ degree_distribution + geodesic_distances, plot = FALSE),
    pattern = "Unrecognized terms deleted: geodesic_distances"
  )
  expect_warning(
    m_degrees_updated$assess(formula = ~ degree_distribution + dsp + geo + geodist + esp, plot = FALSE),
    pattern = "Unrecognized terms deleted: dsp, geo, geodist, esp"
  )
})




