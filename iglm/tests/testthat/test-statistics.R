test_that("Test some sufficient statistics for undirected networks", {
  n_units <- 100
  block <- matrix(nrow = 50, ncol = 50, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 50, block, simplify = FALSE)))

  overlapping_degree <- 0.5
  neighborhood <- matrix(nrow = n_units, ncol = n_units, data = 0)
  block <- matrix(nrow = 5, ncol = 5, data = 0)
  size_neighborhood <- 5
  size_overlap <- ceiling(size_neighborhood * overlapping_degree)

  end <- floor((n_units - size_neighborhood) / size_overlap)
  for (i in 0:end) {
    neighborhood[(1 + size_overlap * i):(size_neighborhood + size_overlap * i), (1 + size_overlap * i):(size_neighborhood + size_overlap * i)] <- 1
  }
  neighborhood[(n_units - size_neighborhood + 1):(n_units), (n_units - size_neighborhood + 1):(n_units)] <- 1

  family_x <- "normal"
  family_y <- "normal"

  xyz_obj_new <- iglm.data(
    neighborhood = neighborhood, directed = FALSE,
    family_x = family_x, family_y = family_y, scale_y = 2, scale_x = 3
  )
  gt_coef <- c(3, -1, -1)
  gt_coef_pop <- c(rnorm(n = n_units, -2, 1))

  sampler_new <- sampler.iglm(
    n_burn_in = 1, n_simulation = 5,
    sampler_x = sampler.net.attr(n_proposals = n_units * 100),
    sampler_y = sampler.net.attr(n_proposals = n_units * 100),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )


  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x + degrees,
    coef = gt_coef, coef_degrees = gt_coef_pop, sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  model_tmp_new$print()
  model_tmp_new$simulate()


  count_values_iglm <- statistics(model_tmp_new$results$samples[[1]] ~
    spillover_xx_scaled(mode = "local") +
    spillover_yy_scaled(mode = "local") +
    spillover_xy_scaled(mode = "local") +
    spillover_yx_scaled(mode = "local"))
  # Count the statistics by hand
  tmp <- model_tmp_new$get_samples()
  z_network <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  # Undirected network
  z_network[tmp[[1]]$z_network] <- 1
  z_network[cbind(tmp[[1]]$z_network[, 2], tmp[[1]]$z_network[, 1])] <- 1

  overlap <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  overlap[tmp[[1]]$overlap] <- 1
  val_xx <- c("spillover_xx_scaled(mode = 'local')" = 0)
  val_yy <- c("spillover_yy_scaled(mode = 'local')" = 0)
  val_xy <- c("spillover_xy_scaled(mode = 'local')" = 0)
  val_yx <- c("spillover_yx_scaled(mode = 'local')" = 0)
  network_nb <- z_network * overlap
  
  x_scaled <- tmp[[1]]$x_attribute / xyz_obj_new$scale_x
  y_scaled <- tmp[[1]]$y_attribute / xyz_obj_new$scale_y
  for (i in 1:tmp[[1]]$n_units) {
    if (sum(network_nb[i, ]) == 0) {
      next
    }
    val_xx <- val_xx + sum(x_scaled[i] * x_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_yy <- val_yy + sum(y_scaled[i] * y_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_xy <- val_xy + (sum(x_scaled[i] * y_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ]))
    val_yx <- val_yx + sum(y_scaled[i] * x_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
  }
  expect_equal(count_values_iglm[1], val_xx)
  expect_equal(count_values_iglm[2], val_yy)
  expect_equal(count_values_iglm[3], val_xy)
  expect_equal(count_values_iglm[4], val_yx)
  
  
  sampler_new <- sampler.iglm(
    n_burn_in = 1, n_simulation = 10,
    sampler_x = sampler.net.attr(n_proposals = n_units * 100),
    sampler_y = sampler.net.attr(n_proposals = n_units * 100),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )

  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x +
      spillover_xx_scaled(mode = "local") +
      spillover_xy_scaled(mode = "local") +
      spillover_yx_scaled(mode = "local") +
      spillover_yy_scaled(mode = "local") +
      spillover_xx_scaled(mode = "global") +
      spillover_xy_scaled(mode = "global") +
      spillover_yx_scaled(mode = "global") +
      spillover_yy_scaled(mode = "global") + degrees,
    coef = c(gt_coef, 0, 0, 0, 0, 0, 0, 0, 0), coef_degrees = gt_coef_pop, sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  
  model_tmp_new$simulate()

  expect_all_true(as.vector(model_tmp_new$results$stats[, 1] == statistics(model_tmp_new$results$samples ~ edges(mode = "local"))))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 2]) - statistics(model_tmp_new$results$samples ~ attribute_y) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 3]) - statistics(model_tmp_new$results$samples ~ attribute_x) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 4]) - statistics(model_tmp_new$results$samples ~  spillover_xx_scaled(mode = "local") )) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 5]) - statistics(model_tmp_new$results$samples ~ spillover_xy_scaled(mode = "local"))) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 6]) - statistics(model_tmp_new$results$samples ~ spillover_yx_scaled(mode = "local") )) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 7]) - statistics(model_tmp_new$results$samples ~ spillover_yy_scaled(mode = "local") )) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 8]) - statistics(model_tmp_new$results$samples ~ spillover_xx_scaled(mode = "global"))) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 9]) - statistics(model_tmp_new$results$samples ~ spillover_xy_scaled(mode = "global"))) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 10]) - statistics(model_tmp_new$results$samples ~ spillover_yx_scaled(mode = "global"))) < 0.1))
  expect_all_true(as.vector((as.numeric(model_tmp_new$results$stats[, 11]) - statistics(model_tmp_new$results$samples ~ spillover_yy_scaled(mode = "global"))) < 0.1))
  
  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x +
      spillover_xx +
      spillover_xy +
      spillover_yy + degrees,
    coef = c(gt_coef, 0, 0, 0), coef_degrees = gt_coef_pop, 
    sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  model_tmp_new$simulate()
  
  
  
  # sum(model_tmp_new$results$samples[[1]]$y_attribute)
  expect_all_true(as.vector(model_tmp_new$results$stats[, 1] == statistics(model_tmp_new$results$samples ~ edges(mode = "local"))))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 2]) - statistics(model_tmp_new$results$samples ~ attribute_y) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 3]) - statistics(model_tmp_new$results$samples ~ attribute_x) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 4]) - statistics(model_tmp_new$results$samples ~ spillover_xx) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 5]) - statistics(model_tmp_new$results$samples ~ spillover_xy) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 6]) - statistics(model_tmp_new$results$samples ~ spillover_yy ) < 0.1))
  
})



test_that("Test some sufficient statistics for directed networks", {
  n_units <- 100
  block <- matrix(nrow = 50, ncol = 50, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 50, block, simplify = FALSE)))

  overlapping_degree <- 0.5
  neighborhood <- matrix(nrow = n_units, ncol = n_units, data = 0)
  block <- matrix(nrow = 5, ncol = 5, data = 0)
  size_neighborhood <- 5
  size_overlap <- ceiling(size_neighborhood * overlapping_degree)

  end <- floor((n_units - size_neighborhood) / size_overlap)
  for (i in 0:end) {
    neighborhood[(1 + size_overlap * i):(size_neighborhood + size_overlap * i), (1 + size_overlap * i):(size_neighborhood + size_overlap * i)] <- 1
  }
  neighborhood[(n_units - size_neighborhood + 1):(n_units), (n_units - size_neighborhood + 1):(n_units)] <- 1
  family_x <- "normal"
  family_y <- "normal"

  xyz_obj_new <- iglm.data(
    neighborhood = neighborhood, directed = TRUE,
    family_x = family_x, family_y = family_y, scale_y = 2, scale_x = 3
  )
  gt_coef <- c(3, -1, -1)
  gt_coef_pop <- c(rnorm(n = n_units, -2, 1), rnorm(n = n_units, -2, 1))

  sampler_new <- sampler.iglm(
    n_burn_in = 1, n_simulation = 5,
    sampler_x = sampler.net.attr(n_proposals = n_units * 100),
    sampler_y = sampler.net.attr(n_proposals = n_units * 100),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )


  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x + degrees,
    coef = gt_coef, coef_degrees = gt_coef_pop, sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  model_tmp_new$print()
  model_tmp_new$simulate()


  count_values_iglm <- statistics(model_tmp_new$results$samples[[1]] ~
    spillover_xx_scaled(mode = "local") +
    spillover_yy_scaled(mode = "local") +
    spillover_xy_scaled(mode = "local") +
    spillover_yx_scaled(mode = "local"))
  # Count the statistics by hand
  tmp <- model_tmp_new$get_samples()
  z_network <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  # Directed network
  z_network[tmp[[1]]$z_network] <- 1
  # z_network[cbind(tmp[[1]]$z_network[,2], tmp[[1]]$z_network[,1])] <- 1

  overlap <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  overlap[tmp[[1]]$overlap] <- 1
  val_xx <- c("spillover_xx_scaled(mode = 'local')" = 0)
  val_yy <- c("spillover_yy_scaled(mode = 'local')" = 0)
  val_xy <- c("spillover_xy_scaled(mode = 'local')" = 0)
  val_yx <- c("spillover_yx_scaled(mode = 'local')" = 0)
  network_nb <- z_network * overlap
  x_scaled <- tmp[[1]]$x_attribute / xyz_obj_new$scale_x
  y_scaled <- tmp[[1]]$y_attribute / xyz_obj_new$scale_y
  for (i in 1:tmp[[1]]$n_units) {
    if (sum(network_nb[i, ]) == 0) {
      next
    }
    val_xx <- val_xx + sum((x_scaled[i]) * x_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_yy <- val_yy + sum((y_scaled[i]) * y_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_xy <- val_xy + sum((x_scaled[i]) * y_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_yx <- val_yx + sum((y_scaled[i]) * x_scaled[network_nb[i, ] == 1]) / sum(network_nb[i, ])
  }
  expect_equal(count_values_iglm[1], val_xx)
  expect_equal(count_values_iglm[2], val_yy)
  expect_equal(count_values_iglm[3], val_xy)
  expect_equal(count_values_iglm[4], val_yx)


  sampler_new <- sampler.iglm(
    n_burn_in = 1, n_simulation = 100,
    sampler_x = sampler.net.attr(n_proposals = n_units * 100),
    sampler_y = sampler.net.attr(n_proposals = n_units * 100),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )


  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x +
      spillover_xx_scaled(mode = "local") +
      spillover_xy_scaled(mode = "local") +
      spillover_yx_scaled(mode = "local") +
      spillover_yy_scaled(mode = "local") +
      spillover_xx_scaled(mode = "global") +
      spillover_xy_scaled(mode = "global") +
      spillover_yx_scaled(mode = "global") +
      spillover_yy_scaled(mode = "global") + degrees,
    coef = c(gt_coef, 0, 0, 0, 0, 0, 0, 0, 0), coef_degrees = gt_coef_pop, sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  model_tmp_new$simulate()

  expect_all_true(as.vector(model_tmp_new$results$stats[, 1] == statistics(model_tmp_new$results$samples ~ edges(mode = "local"))))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 2]) - statistics(model_tmp_new$results$samples ~ attribute_y) < 0.01))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 3]) - statistics(model_tmp_new$results$samples ~ attribute_x) < 0.01))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 4]) - statistics(model_tmp_new$results$samples ~  spillover_xx_scaled(mode = "local") ) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 5]) - statistics(model_tmp_new$results$samples ~ spillover_xy_scaled(mode = "local")) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 6]) - statistics(model_tmp_new$results$samples ~ spillover_yx_scaled(mode = "local") ) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 7]) - statistics(model_tmp_new$results$samples ~  spillover_yy_scaled(mode = "local")) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 8]) - statistics(model_tmp_new$results$samples ~ spillover_xx_scaled(mode = "global")) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 9]) - statistics(model_tmp_new$results$samples ~ spillover_xy_scaled(mode = "global")) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 10]) - statistics(model_tmp_new$results$samples ~  spillover_yx_scaled(mode = "global")) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 11]) - statistics(model_tmp_new$results$samples ~  spillover_yy_scaled(mode = "global") ) < 0.1))
  
  
  
  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x +
      spillover_xx +
      spillover_xy +
      spillover_yy + degrees,
    coef = c(gt_coef, 0, 0, 0), coef_degrees = gt_coef_pop, 
    sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  model_tmp_new$simulate()
  
  expect_all_true(as.vector(model_tmp_new$results$stats[, 1] == statistics(model_tmp_new$results$samples ~ edges(mode = "local"))))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 2]) - statistics(model_tmp_new$results$samples ~ attribute_y) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 3]) - statistics(model_tmp_new$results$samples ~ attribute_x) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 4]) - statistics(model_tmp_new$results$samples ~ spillover_xx ) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 5]) - statistics(model_tmp_new$results$samples ~ spillover_xy) < 0.1))
  expect_all_true(as.vector(as.numeric(model_tmp_new$results$stats[, 6]) - statistics(model_tmp_new$results$samples ~ spillover_yy ) < 0.1))
  
})


test_that("Test some sufficient statistics for directed networks", {
  n_units <- 100
  block <- matrix(nrow = 50, ncol = 50, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 50, block, simplify = FALSE)))

  overlapping_degree <- 0.5
  neighborhood <- matrix(nrow = n_units, ncol = n_units, data = 0)
  block <- matrix(nrow = 5, ncol = 5, data = 0)
  size_neighborhood <- 5
  size_overlap <- ceiling(size_neighborhood * overlapping_degree)

  end <- floor((n_units - size_neighborhood) / size_overlap)
  for (i in 0:end) {
    neighborhood[(1 + size_overlap * i):(size_neighborhood + size_overlap * i), (1 + size_overlap * i):(size_neighborhood + size_overlap * i)] <- 1
  }
  neighborhood[(n_units - size_neighborhood + 1):(n_units), (n_units - size_neighborhood + 1):(n_units)] <- 1
  family_x <- "poisson"
  family_y <- "poisson"

  xyz_obj_new <- iglm.data(neighborhood = neighborhood, directed = TRUE, family_x = family_x, family_y = family_y)
  gt_coef <- c(3, -1, -1)
  gt_coef_pop <- c(rnorm(n = n_units, -2, 1))

  sampler_new <- sampler.iglm(
    n_burn_in = 10, n_simulation = 1,
    sampler_x = sampler.net.attr(n_proposals = n_units * 10),
    sampler_y = sampler.net.attr(n_proposals = n_units * 10),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = F
  )

  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + attribute_y + attribute_x,
    coef = gt_coef, sampler = sampler_new,
    control = control.iglm(accelerated = F, max_it = 200, display_progress = F)
  )
  # debugonce(model_tmp_new$simulate)
  model_tmp_new$simulate()

  count_values_iglm <- statistics(model_tmp_new$results$samples[[1]] ~
    spillover_xx_scaled(mode = "local") +
    spillover_yy_scaled(mode = "local") +
    spillover_xy_scaled(mode = "local") +
    spillover_yx_scaled(mode = "local"))
  # Count the statistics by hand
  tmp <- model_tmp_new$get_samples()
  z_network <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  # Directed network
  z_network[tmp[[1]]$z_network] <- 1

  overlap <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  overlap[tmp[[1]]$overlap] <- 1
  val_xx <- c("spillover_xx_scaled(mode = 'local')" = 0)
  val_yy <- c("spillover_yy_scaled(mode = 'local')" = 0)
  val_xy <- c("spillover_xy_scaled(mode = 'local')" = 0)
  val_yx <- c("spillover_yx_scaled(mode = 'local')" = 0)
  network_nb <- z_network * overlap
  for (i in 1:tmp[[1]]$n_units) {
    if (sum(network_nb[i, ]) == 0) {
      next
    }
    val_xx <- val_xx + sum((tmp[[1]]$x_attribute[i]) * tmp[[1]]$x_attribute[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_yy <- val_yy + sum((tmp[[1]]$y_attribute[i]) * tmp[[1]]$y_attribute[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_xy <- val_xy + sum((tmp[[1]]$x_attribute[i]) * tmp[[1]]$y_attribute[network_nb[i, ] == 1]) / sum(network_nb[i, ])
    val_yx <- val_yx + sum((tmp[[1]]$y_attribute[i]) * tmp[[1]]$x_attribute[network_nb[i, ] == 1]) / sum(network_nb[i, ])
  }
  expect_equal(count_values_iglm[1], val_xx)
  expect_equal(count_values_iglm[2], val_yy)
  expect_equal(count_values_iglm[3], val_xy)
  expect_equal(count_values_iglm[4], val_yx)
})


test_that("Test the spillover effects", {
  n_units <- 100
  block <- matrix(nrow = 50, ncol = 50, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 50, block, simplify = FALSE)))

  overlapping_degree <- 0.5
  neighborhood <- matrix(nrow = n_units, ncol = n_units, data = 0)
  block <- matrix(nrow = 5, ncol = 5, data = 0)
  size_neighborhood <- 5
  size_overlap <- ceiling(size_neighborhood * overlapping_degree)

  end <- floor((n_units - size_neighborhood) / size_overlap)
  for (i in 0:end) {
    neighborhood[(1 + size_overlap * i):(size_neighborhood + size_overlap * i), (1 + size_overlap * i):(size_neighborhood + size_overlap * i)] <- 1
  }
  neighborhood[(n_units - size_neighborhood + 1):(n_units), (n_units - size_neighborhood + 1):(n_units)] <- 1
  family_x <- "binomial"
  family_y <- "binomial"

  xyz_obj_new <- iglm.data(neighborhood = neighborhood, directed = TRUE, family_x = family_x, family_y = family_y)
  gt_coef <- c(5, -1, -1)
  gt_coef_pop <- c(rnorm(n = n_units, -2, 1))

  sampler_new <- sampler.iglm(
    n_burn_in = 1, n_simulation = 3,
    sampler_x = sampler.net.attr(n_proposals = n_units * 10),
    sampler_y = sampler.net.attr(n_proposals = n_units * 10),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
    init_empty = T
  )

  model_tmp_new <- iglm(
    formula = xyz_obj_new ~ edges(mode = "local") + spillover_yx +
      spillover_yy,
    coef = gt_coef, sampler = sampler_new,
    control = control.iglm(accelerated = FALSE, max_it = 200, display_progress = FALSE)
  )
  # debugonce(model_tmp_new$simulate)
  model_tmp_new$simulate()
  count_values_iglm <- statistics(model_tmp_new$results$samples[[1]] ~
    spillover_xx + spillover_yy + spillover_xy + spillover_yx)

  # Count the statistics by hand
  tmp <- model_tmp_new$get_samples()
  z_network <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  # Directed network
  z_network[tmp[[1]]$z_network] <- 1

  overlap <- matrix(0, nrow = tmp[[1]]$n_units, ncol = tmp[[1]]$n_units)
  overlap[tmp[[1]]$overlap] <- 1
  val_xx <- c("spillover_xx" = 0)
  val_yy <- c("spillover_yy" = 0)
  val_xy <- c("spillover_xy" = 0)
  val_yx <- c("spillover_yx" = 0)
  network_nb <- z_network * overlap
  for (i in 1:tmp[[1]]$n_units) {
    if (sum(network_nb[i, ]) == 0) {
      next
    }
    val_xx <- val_xx + sum((tmp[[1]]$x_attribute[i]) * tmp[[1]]$x_attribute[network_nb[i, ] == 1])
    val_yy <- val_yy + sum((tmp[[1]]$y_attribute[i]) * tmp[[1]]$y_attribute[network_nb[i, ] == 1])
    val_xy <- val_xy + sum((tmp[[1]]$x_attribute[i]) * tmp[[1]]$y_attribute[network_nb[i, ] == 1])
    val_yx <- val_yx + sum((tmp[[1]]$y_attribute[i]) * tmp[[1]]$x_attribute[network_nb[i, ] == 1])
  }
  expect_equal(count_values_iglm[1], val_xx)
  expect_equal(count_values_iglm[2], val_yy)
  expect_equal(count_values_iglm[2], model_tmp_new$results$stats[1, 3])

  expect_equal(count_values_iglm[3], val_xy)
  expect_equal(count_values_iglm[4], val_yx)
  expect_equal(count_values_iglm[4], model_tmp_new$results$stats[1, 2])
})

test_that("Test gwdegree, gwodegree, gwidegree vs hand calculation", {
  n_units <- 8
  z <- matrix(c(
    0, 1, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0,
    1, 1, 0, 1, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 1, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 0,
    0, 0, 0, 1, 1, 0, 1, 0,
    0, 0, 0, 0, 0, 1, 0, 1,
    0, 0, 0, 0, 0, 0, 1, 0
  ), nrow = 8, byrow = TRUE)
  x <- c(1, 1, 0, 0, 1, 0, 1, 0)
  y <- c(0, 1, 1, 0, 0, 1, 0, 1)
  neighborhood <- matrix(1, nrow = 8, ncol = 8)
  decay <- 0.5
  expo_min <- 1 - exp(-decay)

  data_dir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, neighborhood = neighborhood, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = TRUE)

  # Hand calculations for directed statistics
  # 1. Global out-degree:
  out_degs <- rowSums(z)
  hand_gwodegree_global <- sum(sapply(out_degs, function(d) {
    if (d == 0) 0 else sum(expo_min^(0:(d - 1)))
  }))

  # 2. Global in-degree:
  in_degs <- colSums(z)
  hand_gwidegree_global <- sum(sapply(in_degs, function(d) {
    if (d == 0) 0 else sum(expo_min^(0:(d - 1)))
  }))

  s_dir <- statistics(data_dir ~ gwodegree(mode = "global", decay = decay) +
                                gwidegree(mode = "global", decay = decay) +
                                gwodegree(mode = "local", decay = decay) +
                                gwidegree(mode = "local", decay = decay))

  expect_equal(unname(s_dir[1]), hand_gwodegree_global)
  expect_equal(unname(s_dir[2]), hand_gwidegree_global)
  expect_equal(unname(s_dir[3]), hand_gwodegree_global) # full neighborhood so local == global
  expect_equal(unname(s_dir[4]), hand_gwidegree_global)

  # Hand calculations for undirected statistics
  z_undir <- ((z + t(z)) > 0) * 1
  diag(z_undir) <- 0
  degs_undir <- rowSums(z_undir)
  hand_gwdegree_global <- sum(sapply(degs_undir, function(d) {
    if (d == 0) 0 else sum(expo_min^(0:(d - 1)))
  }))

  data_undir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z_undir, neighborhood = neighborhood, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = FALSE)

  s_undir <- statistics(data_undir ~ gwdegree(mode = "global", decay = decay) +
                                     gwdegree(mode = "local", decay = decay))

  expect_equal(unname(s_undir[1]), hand_gwdegree_global)
  expect_equal(unname(s_undir[2]), hand_gwdegree_global)
})

test_that("Test that tracked global statistics during simulation match exact statistics at all points", {
  n_units <- 10
  neighborhood <- matrix(1, nrow = n_units, ncol = n_units)
  set.seed(42)
  x <- c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0)
  y <- c(0, 1, 1, 0, 0, 1, 0, 1, 1, 0)
  z <- matrix(rbinom(n_units * n_units, 1, 0.2), nrow = n_units)
  diag(z) <- 0

  data_dir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, neighborhood = neighborhood, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = TRUE)

  sampler_obj <- sampler.iglm(
    n_burn_in = 10, n_simulation = 5,
    sampler_x = sampler.net.attr(n_proposals = 0),
    sampler_y = sampler.net.attr(n_proposals = 0),
    sampler_z = sampler.net.attr(n_proposals = 50),
    init_empty = FALSE
  )

  formula_dir <- data_dir ~ edges(mode = "local") +
    gwodegree(mode = "global", decay = 0.5) +
    gwidegree(mode = "global", decay = 0.5)

  model_fit <- iglm(
    formula = formula_dir,
    coef = c(-1.5, 0.3, 0.3), sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )
  model_fit$simulate()

  samples <- model_fit$get_samples()
  stats_matrix <- model_fit$results$stats

  for (s in seq_along(samples)) {
    sample_stats <- statistics(samples[[s]] ~ edges(mode = "local") +
                                 gwodegree(mode = "global", decay = 0.5) +
                                 gwidegree(mode = "global", decay = 0.5))
    expect_equal(unname(stats_matrix[s, ]), unname(sample_stats), tolerance = 1e-10)
  }

  # Test undirected simulation tracking
  z_undir <- matrix(rbinom(n_units * n_units, 1, 0.2), nrow = n_units)
  z_undir <- ((z_undir + t(z_undir)) > 0) * 1
  diag(z_undir) <- 0

  data_undir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z_undir, neighborhood = neighborhood, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = FALSE)

  formula_undir <- data_undir ~ edges(mode = "local") +
    gwdegree(mode = "global", decay = 0.5)

  model_undir <- iglm(
    formula = formula_undir,
    coef = c(-1.5, 0.3), sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )
  model_undir$simulate()

  samples_undir <- model_undir$get_samples()
  stats_undir_mat <- model_undir$results$stats

  for (s in seq_along(samples_undir)) {
    sample_undir_stats <- statistics(samples_undir[[s]] ~ edges(mode = "local") +
                                       gwdegree(mode = "global", decay = 0.5))
    expect_equal(unname(stats_undir_mat[s, ]), unname(sample_undir_stats), tolerance = 1e-10)
  }
})

test_that("Test iglm.data degree and spillover distributions with continuous attributes binarized at the mean", {
  set.seed(42)
  n_units <- 8
  z <- matrix(c(
    0, 1, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0,
    1, 1, 0, 1, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 1, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 0,
    0, 0, 0, 1, 1, 0, 1, 0,
    0, 0, 0, 0, 0, 1, 0, 1,
    0, 0, 0, 0, 0, 0, 1, 0
  ), nrow = 8, byrow = TRUE)
  
  # Continuous normal attributes
  x_cont <- c(1.5, 0.8, -0.4, -1.2, 2.1, -0.9, 0.3, -0.7) # mean = 0.1875
  y_cont <- c(-0.5, 1.2, 0.9, -1.1, -0.3, 1.8, -0.8, 0.7) # mean = 0.2375
  
  # Binarized equivalents (> mean)
  x_bin <- as.numeric(x_cont > mean(x_cont))
  y_bin <- as.numeric(y_cont > mean(y_cont))
  
  neighborhood <- matrix(1, nrow = 8, ncol = 8)
  
  data_cont <- iglm.data(x_attribute = x_cont, y_attribute = y_cont, z_network = z, neighborhood = neighborhood, n_units = n_units, family_x = "normal", family_y = "normal", directed = TRUE)
  data_bin <- iglm.data(x_attribute = x_bin, y_attribute = y_bin, z_network = z, neighborhood = neighborhood, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = TRUE)
  
  # Test iglm_data degree and degree_distribution with mode = "local"
  deg_cont <- data_cont$degree(x_i = 1, y_j = 0, mode = "local")
  deg_bin <- data_bin$degree(x_i = 1, y_j = 0, mode = "local")
  expect_equal(deg_cont, deg_bin)
  
  deg_dist_cont <- data_cont$degree_distribution(x_i = 1, y_j = 0, mode = "local", plot = FALSE)
  deg_dist_bin <- data_bin$degree_distribution(x_i = 1, y_j = 0, mode = "local", plot = FALSE)
  expect_equal(deg_dist_cont, deg_dist_bin)
})

test_that("Comprehensive 3-way test: Hand calculations vs Standalone global stats vs MCMC continuous updates (Directed)", {
  set.seed(123)
  n_units <- 8
  neighborhood <- matrix(1, nrow = n_units, ncol = n_units) # full neighborhood
  
  # Initial attributes and network
  x <- c(1, 0, 1, 1, 0, 0, 1, 0)
  y <- c(0, 1, 1, 0, 1, 0, 0, 1)
  z <- matrix(rbinom(n_units * n_units, 1, 0.25), nrow = n_units)
  diag(z) <- 0
  
  decay <- 0.6
  expo_min <- 1 - exp(-decay)
  
  data_obj <- iglm.data(
    x_attribute = x, y_attribute = y, z_network = z,
    neighborhood = neighborhood, n_units = n_units,
    family_x = "binomial", family_y = "binomial", directed = TRUE
  )
  
  form <- data_obj ~ edges(mode = "global") +
                     mutual(mode = "global") +
                     attribute_x +
                     attribute_y +
                     attribute_xy +
                     inedges_x(mode = "global") +
                     outedges_y(mode = "global") +
                     edges_x_match(mode = "global") +
                     edges_y_match(mode = "global") +
                     spillover_xy(mode = "local") +
                     spillover_yy(mode = "local") +
                     gwodegree(mode = "global", decay = decay) +
                     gwidegree(mode = "global", decay = decay)
  
  # 1. Hand calculation function for any state (z_mat, x_vec, y_vec)
  calc_hand_stats <- function(z_mat, x_vec, y_vec) {
    # edges: sum_{i != j} z_{ij}
    stat_edges <- sum(z_mat)
    # mutual: sum_{i < j} z_{ij} * z_{ji}
    stat_mutual <- sum(z_mat * t(z_mat)) / 2
    # attribute_x, attribute_y, attribute_xy
    stat_x <- sum(x_vec)
    stat_y <- sum(y_vec)
    stat_xy <- sum(x_vec * y_vec)
    # inedges_x: sum_{i != j} z_{ij} * x_j (receiver x)
    stat_inedges_x <- sum(colSums(z_mat) * x_vec)
    # outedges_y: sum_{i != j} z_{ij} * y_i (sender y)
    stat_outedges_y <- sum(rowSums(z_mat) * y_vec)
    # edges_x_match: sum_{i != j} I(x_i == x_j) * z_{ij}
    match_x <- outer(x_vec, x_vec, function(a, b) (a == b) * 1)
    diag(match_x) <- 0
    stat_edges_x_match <- sum(match_x * z_mat)
    # edges_y_match: sum_{i != j} I(y_i == y_j) * z_{ij}
    match_y <- outer(y_vec, y_vec, function(a, b) (a == b) * 1)
    diag(match_y) <- 0
    stat_edges_y_match <- sum(match_y * z_mat)
    # spillover_xy (local with full neighborhood): sum_{i != j} x_i * y_j * z_{ij}
    stat_spillover_xy <- sum(outer(x_vec, y_vec) * z_mat)
    # spillover_yy (local with full neighborhood): sum_{i != j} y_i * y_j * z_{ij}
    stat_spillover_yy <- sum(outer(y_vec, y_vec) * z_mat)
    # gwodegree
    out_degs <- rowSums(z_mat)
    stat_gwodegree <- sum(sapply(out_degs, function(d) if (d == 0) 0 else sum(expo_min^(0:(d - 1)))))
    # gwidegree
    in_degs <- colSums(z_mat)
    stat_gwidegree <- sum(sapply(in_degs, function(d) if (d == 0) 0 else sum(expo_min^(0:(d - 1)))))
    
    c(
      stat_edges, stat_mutual, stat_x, stat_y, stat_xy,
      stat_inedges_x, stat_outedges_y, stat_edges_x_match, stat_edges_y_match,
      stat_spillover_xy, stat_spillover_yy, stat_gwodegree, stat_gwidegree
    )
  }
  
  # A. Test initial state: Hand vs Standalone global statistics
  initial_hand <- calc_hand_stats(z, x, y)
  initial_standalone <- statistics(form)
  expect_equal(unname(initial_standalone), initial_hand, tolerance = 1e-10)
  
  # B. Test during MCMC simulation with active network updates
  sampler_obj <- sampler.iglm(
    n_burn_in = 20, n_simulation = 10,
    sampler_x = sampler.net.attr(n_proposals = 0),
    sampler_y = sampler.net.attr(n_proposals = 0),
    sampler_z = sampler.net.attr(n_proposals = 40),
    init_empty = FALSE
  )
  
  coefs <- rep(0.1, length(initial_hand))
  model_fit <- iglm(
    formula = form, coef = coefs, sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )
  model_fit$simulate()
  
  samples <- model_fit$get_samples()
  tracked_stats_mat <- model_fit$results$stats
  
  # Verify every sample across simulation matches standalone stats and hand calculation
  for (s in seq_along(samples)) {
    s_obj <- samples[[s]]
    s_z <- matrix(0, nrow = n_units, ncol = n_units)
    if (nrow(s_obj$z_network) > 0) {
      s_z[s_obj$z_network] <- 1
    }
    s_x <- s_obj$x_attribute
    s_y <- s_obj$y_attribute
    
    hand_val <- calc_hand_stats(s_z, s_x, s_y)
    standalone_val <- statistics(s_obj ~ edges(mode = "global") +
                                         mutual(mode = "global") +
                                         attribute_x +
                                         attribute_y +
                                         attribute_xy +
                                         inedges_x(mode = "global") +
                                         outedges_y(mode = "global") +
                                         edges_x_match(mode = "global") +
                                         edges_y_match(mode = "global") +
                                         spillover_xy(mode = "local") +
                                         spillover_yy(mode = "local") +
                                         gwodegree(mode = "global", decay = decay) +
                                         gwidegree(mode = "global", decay = decay))
    tracked_val <- tracked_stats_mat[s, ]
    
    expect_equal(unname(tracked_val), hand_val, tolerance = 1e-10)
    expect_equal(unname(standalone_val), hand_val, tolerance = 1e-10)
    expect_equal(unname(tracked_val), unname(standalone_val), tolerance = 1e-10)
  }
})

test_that("Comprehensive 3-way test: Hand calculations vs Standalone global stats vs MCMC continuous updates (Undirected with continuous attributes)", {
  set.seed(456)
  n_units <- 6
  neighborhood <- matrix(1, nrow = n_units, ncol = n_units)
  
  # Continuous attributes
  x_cont <- c(1.2, -0.5, 0.8, -1.1, 0.4, -0.2)
  y_cont <- c(-0.3, 0.9, -0.7, 1.4, -0.1, 0.5)
  z <- matrix(rbinom(n_units * n_units, 1, 0.3), nrow = n_units)
  z <- ((z + t(z)) > 0) * 1
  diag(z) <- 0
  
  decay <- 0.4
  expo_min <- 1 - exp(-decay)
  
  data_obj <- iglm.data(
    x_attribute = x_cont, y_attribute = y_cont, z_network = z,
    neighborhood = neighborhood, n_units = n_units,
    family_x = "normal", family_y = "normal", directed = FALSE
  )
  
  form <- data_obj ~ edges(mode = "local") +
                     attribute_x +
                     attribute_y +
                     attribute_xy +
                     gwdegree(mode = "global", decay = decay)
  
  # Hand calculation for undirected continuous data
  calc_hand_undir <- function(z_mat, x_vec, y_vec) {
    # In undirected networks, edges counts number of undirected edges:
    stat_edges <- sum(z_mat) / 2
    stat_x <- sum(x_vec)
    stat_y <- sum(y_vec)
    stat_xy <- sum(x_vec * y_vec)
    
    # gwdegree
    degs <- rowSums(z_mat)
    stat_gwdegree <- sum(sapply(degs, function(d) if (d == 0) 0 else sum(expo_min^(0:(d - 1)))))
    
    c(stat_edges, stat_x, stat_y, stat_xy, stat_gwdegree)
  }
  
  # A. Initial state comparison
  initial_hand <- calc_hand_undir(z, x_cont, y_cont)
  initial_standalone <- statistics(form)
  expect_equal(unname(initial_standalone), initial_hand, tolerance = 1e-10)
  
  # B. Simulation tracking (with network simulation)
  sampler_obj <- sampler.iglm(
    n_burn_in = 15, n_simulation = 8,
    sampler_x = sampler.net.attr(n_proposals = 0),
    sampler_y = sampler.net.attr(n_proposals = 0),
    sampler_z = sampler.net.attr(n_proposals = 30),
    init_empty = FALSE
  )
  
  coefs <- rep(0.05, length(initial_hand))
  model_fit <- iglm(
    formula = form, coef = coefs, sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )
  model_fit$simulate()
  
  samples <- model_fit$get_samples()
  tracked_stats_mat <- model_fit$results$stats
  
  for (s in seq_along(samples)) {
    s_obj <- samples[[s]]
    s_z <- matrix(0, nrow = n_units, ncol = n_units)
    if (nrow(s_obj$z_network) > 0) {
      s_z[s_obj$z_network] <- 1
      s_z[s_obj$z_network[, c(2, 1), drop = FALSE]] <- 1
    }
    s_x <- s_obj$x_attribute
    s_y <- s_obj$y_attribute
    
    hand_val <- calc_hand_undir(s_z, s_x, s_y)
    standalone_val <- statistics(s_obj ~ edges(mode = "local") +
                                         attribute_x +
                                         attribute_y +
                                         attribute_xy +
                                         gwdegree(mode = "global", decay = decay))
    tracked_val <- tracked_stats_mat[s, ]
    
    expect_equal(unname(tracked_val), hand_val, tolerance = 1e-10)
    expect_equal(unname(standalone_val), hand_val, tolerance = 1e-10)
    expect_equal(unname(tracked_val), unname(standalone_val), tolerance = 1e-10)
  }
})

test_that("statistics() supports canonical_names and switches between labeled and canonical names", {
  data(copenhagen)
  formula.norm <- copenhagen ~ attribute_y + attribute_xy + edges(mode = "alocal") + transitive + spillover_xy + spillover_yy + degrees

  # Labeled names by default (canonical_names = FALSE)
  stats_labeled <- statistics(formula.norm, canonical_names = FALSE)
  expected_labeled <- c(
    "attribute_duration",
    "attribute_gender_duration",
    "edges(mode = 'alocal')",
    "transitive",
    "spillover_gender_duration",
    "spillover_duration_duration"
  )
  expect_equal(names(stats_labeled), expected_labeled)

  # Default should be canonical_names = FALSE
  stats_default <- statistics(formula.norm)
  expect_equal(names(stats_default), expected_labeled)

  # Canonical names (canonical_names = TRUE)
  stats_canonical <- statistics(formula.norm, canonical_names = TRUE)
  expected_canonical <- c(
    "attribute_y",
    "attribute_xy",
    "edges(mode = 'alocal')",
    "transitive",
    "spillover_xy",
    "spillover_yy"
  )
  expect_equal(names(stats_canonical), expected_canonical)
  expect_equal(unname(stats_labeled), unname(stats_canonical))

  # Validation of canonical_names parameter
  expect_error(statistics(formula.norm, canonical_names = NA), "`canonical_names` must be a single non-missing logical value")
  expect_error(statistics(formula.norm, canonical_names = c(TRUE, FALSE)), "`canonical_names` must be a single non-missing logical value")
  expect_error(statistics(formula.norm, canonical_names = "TRUE"), "`canonical_names` must be a single non-missing logical value")
})




