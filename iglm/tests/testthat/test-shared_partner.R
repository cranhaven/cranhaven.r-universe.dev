test_that("dyadwise_shared_partner and edgewise_shared_partner work with mode in undirected networks", {
  # 4-node network: edges (1,2), (1,3), (2,3), (3,4)
  z <- matrix(c(
    0, 1, 1, 0,
    1, 0, 1, 0,
    1, 1, 0, 1,
    0, 0, 1, 0
  ), nrow = 4, byrow = TRUE)

  # Neighborhood: block 1: {1, 2, 3}, block 2: {4}
  nb <- matrix(c(
    1, 1, 1, 0,
    1, 1, 1, 0,
    1, 1, 1, 0,
    0, 0, 0, 1
  ), nrow = 4, byrow = TRUE)

  data_obj <- iglm.data(
    z_network = z,
    neighborhood = nb,
    directed = FALSE,
    family_x = "binomial",
    family_y = "binomial"
  )

  # Hand-calculated dyadwise shared partners (upper triangular):
  # (1,2) -> common neighbor 3 -> 1
  # (1,3) -> common neighbor 2 -> 1
  # (1,4) -> common neighbor 3 -> 1
  # (2,3) -> common neighbor 1 -> 1
  # (2,4) -> common neighbor 3 -> 1
  # (3,4) -> common neighbor none -> 0

  # Global dyadwise shared partners (default)
  dsp_global <- as.matrix(data_obj$dyadwise_shared_partner(mode = "global"))
  expect_equal(dsp_global[1, 2], 1)
  expect_equal(dsp_global[1, 3], 1)
  expect_equal(dsp_global[1, 4], 1)
  expect_equal(dsp_global[2, 3], 1)
  expect_equal(dsp_global[2, 4], 1)
  expect_equal(dsp_global[3, 4], 0)
  expect_true(is.na(dsp_global[1, 1]))
  expect_true(is.na(dsp_global[2, 1]))

  # Local dyadwise shared partners (only pairs within {1, 2, 3} are kept)
  dsp_local <- as.matrix(data_obj$dyadwise_shared_partner(mode = "local"))
  expect_equal(dsp_local[1, 2], 1)
  expect_equal(dsp_local[1, 3], 1)
  expect_equal(dsp_local[2, 3], 1)
  expect_equal(dsp_local[1, 4], 0)
  expect_equal(dsp_local[2, 4], 0)
  expect_equal(dsp_local[3, 4], 0)

  # Dyadwise distributions
  dist_global <- data_obj$dyadwise_shared_partner_distribution(mode = "global", value_range = c(0, 1), prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(dist_global["0"]), 1)
  expect_equal(as.numeric(dist_global["1"]), 5)

  dist_local <- data_obj$dyadwise_shared_partner_distribution(mode = "local", value_range = c(0, 1), prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(dist_local["0"]), 0)
  expect_equal(as.numeric(dist_local["1"]), 3)

  # Global edgewise shared partners: edges are (1,2), (1,3), (2,3), (3,4) -> counts 1, 1, 1, 0
  esp_global <- data_obj$edgewise_shared_partner(mode = "global")
  expect_equal(sort(esp_global), c(0, 1, 1, 1))

  # Local edgewise shared partners: only edges (1,2), (1,3), (2,3) -> counts 1, 1, 1
  esp_local <- data_obj$edgewise_shared_partner(mode = "local")
  expect_equal(sort(esp_local), c(1, 1, 1))

  # Edgewise distributions
  edist_global <- data_obj$edgewise_shared_partner_distribution(mode = "global", value_range = c(0, 1), prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(edist_global["0"]), 1)
  expect_equal(as.numeric(edist_global["1"]), 3)

  edist_local <- data_obj$edgewise_shared_partner_distribution(mode = "local", value_range = c(0, 1), prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(edist_local["0"]), 0)
  expect_equal(as.numeric(edist_local["1"]), 3)
})

test_that("dyadwise_shared_partner and edgewise_shared_partner work with mode in directed networks", {
  # 4-node directed network: edges 1->2, 1->3, 2->3, 3->4
  z <- matrix(c(
    0, 1, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    0, 0, 0, 0
  ), nrow = 4, byrow = TRUE)

  # Neighborhood: block 1: {1, 2, 3}, block 2: {4}
  nb <- matrix(c(
    1, 1, 1, 0,
    1, 1, 1, 0,
    1, 1, 1, 0,
    0, 0, 0, 1
  ), nrow = 4, byrow = TRUE)

  data_obj <- iglm.data(
    z_network = z,
    neighborhood = nb,
    directed = TRUE,
    family_x = "binomial",
    family_y = "binomial"
  )

  # 1. OTP (Outgoing Two-Path: z_{i,h} * z_{j,h})
  # (1,2) and (2,1) both send to 3 -> count 1
  # All other pairs -> 0
  otp_global <- as.matrix(data_obj$dyadwise_shared_partner(type = "OTP", mode = "global"))
  expect_equal(otp_global[1, 2], 1)
  expect_equal(otp_global[2, 1], 1)
  expect_equal(otp_global[1, 3], 0)
  expect_true(is.na(otp_global[1, 1]))

  otp_local <- as.matrix(data_obj$dyadwise_shared_partner(type = "OTP", mode = "local"))
  expect_equal(otp_local[1, 2], 1)
  expect_equal(otp_local[2, 1], 1)
  expect_equal(otp_local[1, 3], 0)
  expect_equal(otp_local[3, 1], 0)
  expect_equal(otp_local[2, 3], 0)
  expect_equal(otp_local[3, 2], 0)
  expect_equal(otp_local[1, 4], 0)
  expect_equal(otp_local[4, 1], 0)

  otp_dist_local <- data_obj$dyadwise_shared_partner_distribution(type = "OTP", mode = "local", prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(otp_dist_local["0"]), 4)
  expect_equal(as.numeric(otp_dist_local["1"]), 2)

  # 2. ISP (Ingoing Shared Partner / Two-Path: z_{i,h} * z_{h,j} -> path i -> h -> j)
  # (1,3) via 2: 1->2->3 (count 1)
  # (1,4) via 3: 1->3->4 (count 1)
  # (2,4) via 3: 2->3->4 (count 1)
  isp_global <- as.matrix(data_obj$dyadwise_shared_partner(type = "ISP", mode = "global"))
  expect_equal(isp_global[1, 3], 1)
  expect_equal(isp_global[1, 4], 1)
  expect_equal(isp_global[2, 4], 1)
  expect_equal(isp_global[1, 2], 0)

  isp_local <- as.matrix(data_obj$dyadwise_shared_partner(type = "ISP", mode = "local"))
  expect_equal(isp_local[1, 3], 1)
  expect_equal(isp_local[1, 2], 0)
  expect_equal(isp_local[2, 1], 0)
  expect_equal(isp_local[3, 1], 0)
  expect_equal(isp_local[2, 3], 0)
  expect_equal(isp_local[3, 2], 0)
  expect_equal(isp_local[1, 4], 0)
  expect_equal(isp_local[2, 4], 0)

  isp_dist_local <- data_obj$dyadwise_shared_partner_distribution(type = "ISP", mode = "local", prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(isp_dist_local["0"]), 5)
  expect_equal(as.numeric(isp_dist_local["1"]), 1)

  # 3. ITP (Incoming Two-Path: z_{h,i} * z_{h,j} -> h sends to both i and j)
  # 1 sends to 2 and 3 -> (2,3) = 1, (3,2) = 1
  itp_global <- as.matrix(data_obj$dyadwise_shared_partner(type = "ITP", mode = "global"))
  expect_equal(itp_global[2, 3], 1)
  expect_equal(itp_global[3, 2], 1)
  expect_equal(itp_global[1, 2], 0)

  itp_local <- as.matrix(data_obj$dyadwise_shared_partner(type = "ITP", mode = "local"))
  expect_equal(itp_local[2, 3], 1)
  expect_equal(itp_local[3, 2], 1)
  expect_equal(itp_local[1, 2], 0)
  expect_equal(itp_local[2, 1], 0)
  expect_equal(itp_local[1, 3], 0)
  expect_equal(itp_local[3, 1], 0)
  expect_equal(itp_local[1, 4], 0)

  itp_dist_local <- data_obj$dyadwise_shared_partner_distribution(type = "ITP", mode = "local", prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(itp_dist_local["0"]), 4)
  expect_equal(as.numeric(itp_dist_local["1"]), 2)

  # 4. OSP (Outgoing Shared Partner: z_{h,i} * z_{j,h} -> j -> h -> i)
  # (3,1) via 2: 1->2->3 (count 1)
  # (4,1) via 3: 1->3->4 (count 1)
  # (4,2) via 3: 2->3->4 (count 1)
  osp_global <- as.matrix(data_obj$dyadwise_shared_partner(type = "OSP", mode = "global"))
  expect_equal(osp_global[3, 1], 1)
  expect_equal(osp_global[4, 1], 1)
  expect_equal(osp_global[4, 2], 1)

  osp_local <- as.matrix(data_obj$dyadwise_shared_partner(type = "OSP", mode = "local"))
  expect_equal(osp_local[3, 1], 1)
  expect_equal(osp_local[1, 3], 0)
  expect_equal(osp_local[4, 1], 0)

  osp_dist_local <- data_obj$dyadwise_shared_partner_distribution(type = "OSP", mode = "local", prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(osp_dist_local["0"]), 5)
  expect_equal(as.numeric(osp_dist_local["1"]), 1)

  # Directed edgewise shared partners:
  # Edges: 1->2, 1->3, 2->3 (in overlap), 3->4 (outside overlap)
  # OTP on local edges:
  # (1,2) count 1; (1,3) count 0; (2,3) count 0
  esp_otp_local <- data_obj$edgewise_shared_partner(type = "OTP", mode = "local")
  expect_equal(sort(esp_otp_local), c(0, 0, 1))

  edist_otp_local <- data_obj$edgewise_shared_partner_distribution(type = "OTP", mode = "local", prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(edist_otp_local["0"]), 2)
  expect_equal(as.numeric(edist_otp_local["1"]), 1)
})

test_that("dyadwise and edgewise shared partner functions validate inputs properly", {
  n_units <- 4
  nb <- matrix(1, 4, 4)
  data_obj <- iglm.data(neighborhood = nb, directed = FALSE, family_x = "binomial", family_y = "binomial")

  # Invalid mode argument
  expect_error(data_obj$dyadwise_shared_partner(mode = "invalid"), "'mode' must be either 'global' or 'local'.")
  expect_error(data_obj$dyadwise_shared_partner(mode = TRUE), "'mode' must be either 'global' or 'local'.")
  expect_error(data_obj$dyadwise_shared_partner(mode = 123), "'mode' must be either 'global' or 'local'.")

  expect_error(data_obj$edgewise_shared_partner(mode = "invalid"), "'mode' must be either 'global' or 'local'.")
  expect_error(data_obj$edgewise_shared_partner(mode = 123), "'mode' must be either 'global' or 'local'.")

  expect_error(data_obj$dyadwise_shared_partner_distribution(mode = "foo"), "'mode' must be either 'global' or 'local'.")
  expect_error(data_obj$edgewise_shared_partner_distribution(mode = "foo"), "'mode' must be either 'global' or 'local'.")

  # Invalid type
  expect_error(data_obj$dyadwise_shared_partner(type = "INVALID"), "type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
  expect_error(data_obj$edgewise_shared_partner(type = "INVALID"), "type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
  expect_error(data_obj$dyadwise_shared_partner_distribution(type = "INVALID"), "type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")
  expect_error(data_obj$edgewise_shared_partner_distribution(type = "INVALID"), "type must be one of 'OTP', 'ISP', 'OSP', 'ITP', 'ALL', or 'symm'.")

  # Directed types on undirected network
  expect_error(data_obj$dyadwise_shared_partner(type = "OTP"), "Type 'OTP' is only for directed networks.")
  expect_error(data_obj$edgewise_shared_partner(type = "ISP"), "Type 'ISP' is only for directed networks.")
  expect_error(data_obj$dyadwise_shared_partner_distribution(type = "OSP"), "Type 'OSP' is only for directed networks.")
  expect_error(data_obj$edgewise_shared_partner_distribution(type = "ITP"), "Type 'ITP' is only for directed networks.")

  # Undirected type (symm) on directed network
  data_dir <- iglm.data(neighborhood = nb, directed = TRUE, family_x = "binomial", family_y = "binomial")
  expect_error(data_dir$dyadwise_shared_partner(type = "symm"), "Type 'symm' is only for undirected networks.")
  expect_error(data_dir$edgewise_shared_partner(type = "symm"), "Type 'symm' is only for undirected networks.")
  expect_error(data_dir$dyadwise_shared_partner_distribution(type = "symm"), "Type 'symm' is only for undirected networks.")
  expect_error(data_dir$edgewise_shared_partner_distribution(type = "symm"), "Type 'symm' is only for undirected networks.")

  # Invalid value_range
  expect_error(data_obj$dyadwise_shared_partner_distribution(value_range = c(-1, 2)), "'value_range' values must be non-negative.")
  expect_error(data_obj$edgewise_shared_partner_distribution(value_range = c(-1, 2)), "'value_range' values must be non-negative.")
  expect_error(data_obj$dyadwise_shared_partner_distribution(value_range = 1:3), "'value_range' must be a numeric vector of length 2.")
  expect_error(data_obj$edgewise_shared_partner_distribution(value_range = 1:3), "'value_range' must be a numeric vector of length 2.")
})

test_that("assess and results$plot work with mode in shared partner distributions", {
  n_units <- 15
  block <- matrix(nrow = 5, ncol = 5, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 5, block, simplify = FALSE)))

  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = FALSE, family_x = "binomial", family_y = "binomial")
  gt_coef <- c(1, -0.5, -0.5)

  sampler_obj <- sampler.iglm(
    n_burn_in = 2, n_simulation = 2,
    sampler_x = sampler.net.attr(n_proposals = n_units * 2),
    sampler_y = sampler.net.attr(n_proposals = n_units * 2),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 2)
  )

  model_fit <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y + attribute_x,
    coef = gt_coef, sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )

  model_fit$simulate()
  model_fit$set_target(model_fit$get_samples()[[1]])
  model_fit$estimate()

  # Run assess with mode in dyadwise and edgewise shared partner distributions
  assessment <- model_fit$assess(
    formula = ~ dyadwise_shared_partner_distribution(mode = "local") +
      edgewise_shared_partner_distribution(mode = "local") +
      dyadwise_shared_partner_distribution(mode = "global") +
      edgewise_shared_partner_distribution(mode = "global"),
    plot = FALSE
  )

  expect_true(inherits(assessment, "iglm_model_assessment"))
  expect_true("dyadwise_shared_partner_distribution_mode_local" %in% names(assessment$observed))
  expect_true("edgewise_shared_partner_distribution_mode_local" %in% names(assessment$observed))
  expect_true("dyadwise_shared_partner_distribution_mode_global" %in% names(assessment$observed))
  expect_true("edgewise_shared_partner_distribution_mode_global" %in% names(assessment$observed))

  # Test plotting without errors
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()
})

test_that("geodesic_distances and geodesic_distances_distribution work with mode", {
  # 6-node network:
  # Block 1: {1, 2, 3} with path 1-2-3 (distances: d(1,2)=1, d(2,3)=1, d(1,3)=2)
  # Block 2: {4, 5, 6} with path 4-5-6 (distances: d(4,5)=1, d(5,6)=1, d(4,6)=2)
  # Cross-block bridge: 3-4 (connects block 1 and block 2)
  z <- matrix(0, nrow = 6, ncol = 6)
  z[1, 2] <- z[2, 1] <- 1
  z[2, 3] <- z[3, 2] <- 1
  z[3, 4] <- z[4, 3] <- 1
  z[4, 5] <- z[5, 4] <- 1
  z[5, 6] <- z[6, 5] <- 1

  # Block neighborhood: {1,2,3} and {4,5,6}
  nb <- matrix(0, nrow = 6, ncol = 6)
  nb[1:3, 1:3] <- 1
  nb[4:6, 4:6] <- 1

  data_obj <- iglm.data(z_network = z, neighborhood = nb, directed = FALSE)

  # Global geodesic distances
  d_global <- as.matrix(data_obj$geodesic_distances(mode = "global"))
  expect_equal(d_global[1, 2], 1)
  expect_equal(d_global[1, 3], 2)
  expect_equal(d_global[1, 6], 5) # 1-2-3-4-5-6 has length 5

  # Local geodesic distances (only pairs within block 1 or block 2)
  d_local <- as.matrix(data_obj$geodesic_distances(mode = "local"))
  expect_equal(d_local[1, 2], 1)
  expect_equal(d_local[1, 3], 2)
  expect_equal(d_local[4, 5], 1)
  expect_equal(d_local[4, 6], 2)
  # Cross-block distance (1,6) is not in overlap, so should be NA
  expect_true(is.na(d_local[1, 6]))
  expect_true(is.na(d_local[3, 4]))

  # Distribution mode = "global"
  dist_global <- data_obj$geodesic_distances_distribution(mode = "global", prob = FALSE, plot = FALSE)
  # Total pairs = 6 * 5 / 2 = 15 pairs
  expect_equal(sum(dist_global), 15)

  # Distribution mode = "local"
  # Overlap pairs: (1,2), (1,3), (2,3) and (4,5), (4,6), (5,6) -> 6 pairs total
  dist_local <- data_obj$geodesic_distances_distribution(mode = "local", prob = FALSE, plot = FALSE)
  expect_equal(sum(dist_local), 6)
  # Distances: d(1,2)=1, d(2,3)=1, d(4,5)=1, d(5,6)=1 -> 4 of length 1; d(1,3)=2, d(4,6)=2 -> 2 of length 2
  expect_equal(as.numeric(dist_local["1"]), 4)
  expect_equal(as.numeric(dist_local["2"]), 2)
})

test_that("short aliases esp, dsp, and geo work identically to full method names", {
  n_units <- 10
  z <- matrix(c(1, 2, 2, 3, 3, 4, 1, 3), ncol = 2, byrow = TRUE)
  nb <- matrix(1, nrow = n_units, ncol = n_units)

  data_obj <- iglm.data(z_network = z, neighborhood = nb, directed = FALSE, n_units = n_units)

  # esp vs edgewise_shared_partner
  expect_equal(data_obj$esp(), data_obj$edgewise_shared_partner())
  expect_equal(data_obj$esp(mode = "local"), data_obj$edgewise_shared_partner(mode = "local"))
  expect_equal(data_obj$esp_dist(plot = FALSE), data_obj$edgewise_shared_partner_distribution(plot = FALSE))
  expect_equal(data_obj$esp_dist(mode = "local", plot = FALSE), data_obj$edgewise_shared_partner_distribution(mode = "local", plot = FALSE))

  # dsp vs dyadwise_shared_partner
  expect_equal(as.matrix(data_obj$dsp()), as.matrix(data_obj$dyadwise_shared_partner()))
  expect_equal(as.matrix(data_obj$dsp(mode = "local")), as.matrix(data_obj$dyadwise_shared_partner(mode = "local")))
  expect_equal(data_obj$dsp_dist(plot = FALSE), data_obj$dyadwise_shared_partner_distribution(plot = FALSE))
  expect_equal(data_obj$dsp_dist(mode = "local", plot = FALSE), data_obj$dyadwise_shared_partner_distribution(mode = "local", plot = FALSE))

  # geo vs geodesic_distances
  expect_equal(as.matrix(data_obj$geo()), as.matrix(data_obj$geodesic_distances()))
  expect_equal(as.matrix(data_obj$geo(mode = "local")), as.matrix(data_obj$geodesic_distances(mode = "local")))
  expect_equal(data_obj$geo_dist(plot = FALSE), data_obj$geodesic_distances_distribution(plot = FALSE))
  expect_equal(data_obj$geo_dist(mode = "local", plot = FALSE), data_obj$geodesic_distances_distribution(mode = "local", plot = FALSE))

  # deg vs degree
  expect_equal(data_obj$deg(), data_obj$degree())
  expect_equal(data_obj$deg_dist(plot = FALSE), data_obj$degree_distribution(plot = FALSE))

  # x_dist and y_dist
  expect_equal(data_obj$x_dist(plot = FALSE), data_obj$x_distribution(plot = FALSE))
  expect_equal(data_obj$y_dist(plot = FALSE), data_obj$y_distribution(plot = FALSE))

  # mode = "local" on degree and degree_distribution
  deg_local <- data_obj$degree(mode = "local")
  expect_true(is.numeric(deg_local$degree_seq) || is.list(deg_local))
  deg_dist_local <- data_obj$degree_distribution(mode = "local", plot = FALSE)
  expect_true(inherits(deg_dist_local, "table") || is.list(deg_dist_local))
})

test_that("assess and results$plot work with esp, dsp, geo, and deg aliases and local mode", {
  n_units <- 15
  block <- matrix(nrow = 5, ncol = 5, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 5, block, simplify = FALSE)))

  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = FALSE, family_x = "binomial", family_y = "binomial")
  sampler_obj <- sampler.iglm(
    n_burn_in = 2, n_simulation = 2,
    sampler_x = sampler.net.attr(n_proposals = n_units * 2),
    sampler_y = sampler.net.attr(n_proposals = n_units * 2),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 2)
  )

  model_fit <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y + attribute_x,
    coef = c(1, -0.5, -0.5), sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )

  model_fit$simulate()
  model_fit$set_target(model_fit$get_samples()[[1]])
  model_fit$estimate()

  assessment <- model_fit$assess(
    formula = ~ esp_dist(mode = "local") + dsp_dist(mode = "local") + geo_dist(mode = "local") +
      geo_dist(mode = "global") + deg_dist(mode = "local") + deg_dist(mode = "global") +
      x_dist() + y_dist(),
    plot = FALSE
  )

  expect_true(inherits(assessment, "iglm_model_assessment"))
  expect_true("esp_dist_mode_local" %in% names(assessment$observed))
  expect_true("dsp_dist_mode_local" %in% names(assessment$observed))
  expect_true("geo_dist_mode_local" %in% names(assessment$observed))
  expect_true("geo_dist_mode_global" %in% names(assessment$observed))
  expect_true("deg_dist_mode_local" %in% names(assessment$observed))
  expect_true("deg_dist_mode_global" %in% names(assessment$observed))
  expect_true("x_dist" %in% names(assessment$observed))
  expect_true("y_dist" %in% names(assessment$observed))

  # Test plotting
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()
})

test_that("esp, dsp, and geodist work with x_i, x_j, y_i, y_j constraints in undirected networks", {
  n_units <- 5
  # Edges: (1, 2), (1, 3), (2, 3), (3, 4), (4, 5)
  # Shared partners:
  # (1, 2) share 3 -> 1 shared partner
  # (1, 3) share 2 -> 1 shared partner
  # (2, 3) share 1 -> 1 shared partner
  # (3, 4) share none -> 0
  # (4, 5) share none -> 0
  z <- matrix(c(
    1, 2,
    1, 3,
    2, 3,
    3, 4,
    4, 5
  ), ncol = 2, byrow = TRUE)
  x <- c(1, 1, 0, 0, 1)
  y <- c(0, 0, 1, 1, 0)
  data_obj <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, directed = FALSE)

  # Both x_i and x_j constraints: both endpoints must have x = 1
  # Qualifying edges: only (1, 2) since x[1]=1, x[2]=1
  esp_x1_x1 <- data_obj$esp(x_i = 1, x_j = 1)
  expect_equal(esp_x1_x1, 1)
  esp_dist_x1_x1 <- data_obj$esp_dist(x_i = 1, x_j = 1, plot = FALSE)
  expect_equal(as.numeric(esp_dist_x1_x1["1"]), 1)

  # Single constraint: at least one endpoint has x = 1
  # Qualifying edges: (1, 2) [both 1], (1, 3) [1 has x=1], (2, 3) [2 has x=1], (4, 5) [5 has x=1]
  # (3, 4) has x[3]=0, x[4]=0 -> excluded
  esp_x1 <- data_obj$esp(x_i = 1)
  expect_equal(length(esp_x1), 4)

  # Bilateral cross-attribute constraint: x_i = 1, y_j = 1
  # Qualifying edges: one endpoint has x=1, the other has y=1
  # Nodes with x=1: {1, 2, 5}. Nodes with y=1: {3, 4}.
  # Edges connecting {1, 2, 5} and {3, 4}:
  # (1, 3) connects 1 and 3 -> qualifies! (shared partners: 1)
  # (2, 3) connects 2 and 3 -> qualifies! (shared partners: 1)
  # (4, 5) connects 4 and 5 -> qualifies! (shared partners: 0)
  esp_xy <- data_obj$esp(x_i = 1, y_j = 1)
  expect_equal(sort(esp_xy), c(0, 1, 1))

  esp_dist_xy <- data_obj$esp_dist(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(esp_dist_xy["0"]), 1)
  expect_equal(as.numeric(esp_dist_xy["1"]), 2)

  # Dyadwise shared partners (dsp and dsp_dist)
  dsp_xy <- data_obj$dsp(x_i = 1, y_j = 1)
  expect_true(is.na(dsp_xy[1, 2])) # both have x=1, neither has y=1
  expect_false(is.na(dsp_xy[1, 3])) # node 1 (x=1) and node 3 (y=1)
  expect_false(is.na(dsp_xy[4, 5])) # node 5 (x=1) and node 4 (y=1)

  dsp_dist_xy <- data_obj$dsp_dist(x_i = 1, y_j = 1, plot = FALSE)
  expect_true(inherits(dsp_dist_xy, "table"))

  # Geodesic distances (geodist, geo, geodist_dist, geo_dist)
  geo_xy <- data_obj$geodist(x_i = 1, y_j = 1)
  expect_equal(as.matrix(geo_xy), as.matrix(data_obj$geo(x_i = 1, y_j = 1)))
  # Shortest paths from {1, 2, 5} to {3, 4}:
  # (1, 3): dist 1
  # (1, 4): 1-3-4 -> dist 2
  # (2, 3): dist 1
  # (2, 4): 2-3-4 -> dist 2
  # (5, 3): 5-4-3 -> dist 2
  # (5, 4): dist 1
  geo_dist_xy <- data_obj$geo_dist(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(geo_dist_xy["1"]), 3)
  expect_equal(as.numeric(geo_dist_xy["2"]), 3)

  # Plotting verification with constraints
  pdf(NULL)
  expect_no_error(data_obj$esp_dist(x_i = 1, y_j = 1, plot = TRUE))
  expect_no_error(data_obj$dsp_dist(x_i = 1, y_j = 1, plot = TRUE))
  expect_no_error(data_obj$geo_dist(x_i = 1, y_j = 1, plot = TRUE))
  dev.off()
})

test_that("esp, dsp, and geodist work with x_i, x_j, y_i, y_j constraints in directed networks", {
  n_units <- 4
  # Directed network: 1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4
  z_dir <- matrix(c(
    1, 2,
    1, 3,
    2, 3,
    3, 4
  ), ncol = 2, byrow = TRUE)
  x <- c(1, 0, 1, 0)
  y <- c(0, 1, 0, 1)

  data_dir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z_dir, n_units = n_units, directed = TRUE)

  # Constraint: x_i = 1 (senders: {1, 3}), y_j = 1 (receivers: {2, 4})
  # Directed edges from {1, 3} to {2, 4}:
  # 1 -> 2 (qualifies! x[1]=1, y[2]=1)
  # 3 -> 4 (qualifies! x[3]=1, y[4]=1)
  # 1 -> 3 (receiver has y[3]=0, excluded)
  # 2 -> 3 (sender has x[2]=0, excluded)
  esp_dir <- data_dir$esp(type = "ALL", x_i = 1, y_j = 1)
  expect_equal(length(esp_dir), 2)

  esp_dist_dir <- data_dir$esp_dist(type = "ALL", x_i = 1, y_j = 1, plot = FALSE)
  expect_true(inherits(esp_dist_dir, "table"))

  # DSP in directed
  dsp_dir <- data_dir$dsp(type = "OTP", x_i = 1, y_j = 1)
  expect_false(is.na(dsp_dir[1, 2]))
  expect_false(is.na(dsp_dir[3, 4]))
  expect_true(is.na(dsp_dir[2, 1])) # 2 is not in sender {1, 3}

  # Geodist in directed
  geo_dir <- data_dir$geodist(x_i = 1, y_j = 1)
  expect_false(is.na(geo_dir[1, 2]))
  expect_true(is.na(geo_dir[2, 1]))

  # Function constraint
  esp_fn <- data_dir$esp(type = "ALL", x_i = function(v) v == 1, y_j = function(v) v == 1)
  expect_equal(esp_fn, esp_dir)
})

test_that("assess and results$plot work with constrained esp_dist, dsp_dist, and geo_dist", {
  n_units <- 15
  block <- matrix(nrow = 5, ncol = 5, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(n_units / 5, block, simplify = FALSE)))

  xyz_obj <- iglm.data(neighborhood = neighborhood, directed = FALSE, family_x = "binomial", family_y = "binomial")
  sampler_obj <- sampler.iglm(
    n_burn_in = 2, n_simulation = 2,
    sampler_x = sampler.net.attr(n_proposals = n_units * 2),
    sampler_y = sampler.net.attr(n_proposals = n_units * 2),
    sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 2)
  )

  model_fit <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + attribute_y + attribute_x,
    coef = c(1, -0.5, -0.5), sampler = sampler_obj,
    control = control.iglm(accelerated = FALSE, max_it = 2, display_progress = FALSE)
  )

  model_fit$simulate()
  model_fit$set_target(model_fit$get_samples()[[1]])
  model_fit$estimate()

  # Assess with constrained terms
  assessment <- model_fit$assess(
    formula = ~ esp_dist(x_i = 1, y_j = 0) +
      dsp_dist(x_i = 0, y_j = 1) +
      geo_dist(x_i = 1, y_j = 1),
    plot = FALSE
  )

  expect_true(inherits(assessment, "iglm_model_assessment"))
  expect_true("esp_dist_x_i_1,y_j_0" %in% names(assessment$observed))
  expect_true("dsp_dist_x_i_0,y_j_1" %in% names(assessment$observed))
  expect_true("geo_dist_x_i_1,y_j_1" %in% names(assessment$observed))

  # Test assessment plotting with constrained terms
  pdf(NULL)
  expect_silent(model_fit$results$plot(model_assessment = TRUE))
  dev.off()
})

test_that("hand-coded tests verify degree distribution and all other distributions unconstrained and constrained", {
  # Undirected 6-node network:
  # Edges: (1,2), (1,3), (2,3), (3,4), (4,5), (5,6)
  n_units <- 6
  z <- matrix(0, nrow = 6, ncol = 6)
  edges <- matrix(c(
    1, 2,
    1, 3,
    2, 3,
    3, 4,
    4, 5,
    5, 6
  ), ncol = 2, byrow = TRUE)
  for (k in 1:nrow(edges)) {
    z[edges[k, 1], edges[k, 2]] <- 1
    z[edges[k, 2], edges[k, 1]] <- 1
  }
  x <- c(1, 1, 0, 0, 1, 0)
  y <- c(0, 1, 1, 0, 0, 1)
  nb <- matrix(0, 6, 6)
  nb[1:3, 1:3] <- 1
  nb[4:6, 4:6] <- 1

  d <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, neighborhood = nb,
                 n_units = n_units, family_x = "binomial", family_y = "binomial", directed = FALSE)

  # 1. Degree distribution & deg_dist
  # Unconstrained: degrees are (2, 2, 3, 2, 2, 1) -> 0: 0, 1: 1, 2: 4, 3: 1
  expect_equal(as.numeric(d$degree_distribution(prob = FALSE, plot = FALSE)), c(0, 1, 4, 1))
  expect_equal(as.numeric(d$deg_dist(prob = FALSE, plot = FALSE)), c(0, 1, 4, 1))
  expect_equal(as.numeric(d$degree_distribution(prob = TRUE, plot = FALSE)), c(0, 1/6, 4/6, 1/6))
  # Single constraint x_i = 1 (senders {1, 2, 5}): degrees (2, 2, 2)
  expect_equal(as.numeric(d$degree_distribution(x_i = 1, prob = FALSE, plot = FALSE)), c(0, 0, 3))
  # Bilateral constraint x_i = 1, y_j = 1:
  # Out-degrees for {1, 2, 5} to {2, 3, 6}: 1->2, 2->1, 5->1 -> counts: 0: 0, 1: 2, 2: 1
  # In-degrees for {2, 3, 6} from {1, 2, 5}: 2->1, 3->2, 6->1 -> counts: 0: 0, 1: 2, 2: 1
  deg_bi <- d$degree_distribution(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(deg_bi$out_degree), c(0, 2, 1))
  expect_equal(as.numeric(deg_bi$in_degree), c(0, 2, 1))
  # Local mode: block 1 {1,2,3}, block 2 {4,5,6}; local degrees: (2, 2, 2, 1, 2, 1)
  expect_equal(as.numeric(d$degree_distribution(mode = "local", prob = FALSE, plot = FALSE)), c(0, 2, 4))

  # 2. Edgewise shared partner distribution (esp_dist)
  # Unconstrained edges: (1,2)->1, (1,3)->1, (2,3)->1, (3,4)->0, (4,5)->0, (5,6)->0
  expect_equal(as.numeric(d$esp_dist(prob = FALSE, plot = FALSE)), c(3, 3))
  expect_equal(as.numeric(d$edgewise_shared_partner_distribution(prob = FALSE, plot = FALSE)), c(3, 3))
  expect_equal(as.numeric(d$esp_dist(prob = TRUE, plot = FALSE)), c(0.5, 0.5))
  # Constrained x_i = 1, y_j = 1: qualifying edges (1,2)->1, (1,3)->1, (2,3)->1, (5,6)->0
  expect_equal(as.numeric(d$esp_dist(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)), c(1, 3))
  expect_equal(as.numeric(d$esp_dist(x_i = 1, y_j = 1, prob = TRUE, plot = FALSE)), c(0.25, 0.75))
  # Local mode: 5 local edges: (1,2)->1, (1,3)->1, (2,3)->1, (4,5)->0, (5,6)->0
  expect_equal(as.numeric(d$esp_dist(mode = "local", prob = FALSE, plot = FALSE)), c(2, 3))

  # 3. Dyadwise shared partner distribution (dsp_dist)
  # Unconstrained: 15 dyads -> 8 with 0 shared partners, 7 with 1 shared partner
  expect_equal(as.numeric(d$dsp_dist(prob = FALSE, plot = FALSE)), c(8, 7))
  expect_equal(as.numeric(d$dyadwise_shared_partner_distribution(prob = FALSE, plot = FALSE)), c(8, 7))
  expect_equal(as.numeric(d$dsp_dist(prob = TRUE, plot = FALSE)), c(8/15, 7/15))
  # Constrained x_i = 1, y_j = 1: 8 qualifying dyads -> 4 with 0, 4 with 1
  expect_equal(as.numeric(d$dsp_dist(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)), c(4, 4))
  expect_equal(as.numeric(d$dsp_dist(x_i = 1, y_j = 1, prob = TRUE, plot = FALSE)), c(0.5, 0.5))
  # Local mode: 6 local pairs -> 2 with 0, 4 with 1
  expect_equal(as.numeric(d$dsp_dist(mode = "local", prob = FALSE, plot = FALSE)), c(2, 4))

  # 4. Geodesic distances distribution (geo_dist)
  # Unconstrained: 15 pairs -> dist 1: 6, dist 2: 4, dist 3: 3, dist 4: 2, Inf: 0
  expect_equal(as.numeric(d$geo_dist(prob = FALSE, plot = FALSE)), c(6, 4, 3, 2, 0))
  expect_equal(as.numeric(d$geodesic_distances_distribution(prob = FALSE, plot = FALSE)), c(6, 4, 3, 2, 0))
  expect_equal(as.numeric(d$geo_dist(prob = TRUE, plot = FALSE)), c(6/15, 4/15, 3/15, 2/15, 0))
  # Constrained x_i = 1, y_j = 1: 8 qualifying pairs -> dist 1: 4, dist 2: 1, dist 3: 1, dist 4: 2, Inf: 0
  expect_equal(as.numeric(d$geo_dist(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)), c(4, 1, 1, 2, 0))
  expect_equal(as.numeric(d$geo_dist(x_i = 1, y_j = 1, prob = TRUE, plot = FALSE)), c(0.5, 0.125, 0.125, 0.25, 0))

  # 5. X distribution (x_dist)
  expect_equal(as.numeric(d$x_dist(prob = FALSE, plot = FALSE)), c(3, 3))
  expect_equal(as.numeric(d$x_distribution(prob = FALSE, plot = FALSE)), c(3, 3))
  expect_equal(as.numeric(d$x_dist(prob = TRUE, plot = FALSE)), c(0.5, 0.5))

  # 6. Y distribution (y_dist)
  expect_equal(as.numeric(d$y_dist(prob = FALSE, plot = FALSE)), c(3, 3))
  expect_equal(as.numeric(d$y_distribution(prob = FALSE, plot = FALSE)), c(3, 3))
  expect_equal(as.numeric(d$y_dist(prob = TRUE, plot = FALSE)), c(0.5, 0.5))

  # Continuous attributes (normal)
  d_cont <- iglm.data(x_attribute = c(1.0, 2.0, 3.0), y_attribute = c(-1.0, 0.0, 1.0),
                      n_units = 3, family_x = "normal", family_y = "normal")
  x_dens <- d_cont$x_dist(plot = FALSE)
  expect_true(is.numeric(x_dens))
  expect_true(length(x_dens) > 0)
  y_dens <- d_cont$y_dist(plot = FALSE)
  expect_true(is.numeric(y_dens))
  expect_true(length(y_dens) > 0)

  # Directed 4-node network:
  # 1->2, 1->3, 2->3, 3->4
  n_unit_dir <- 4
  z_dir <- matrix(c(
    0, 1, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    0, 0, 0, 0
  ), nrow = 4, byrow = TRUE)
  x_dir <- c(1, 0, 1, 0)
  y_dir <- c(0, 1, 0, 1)

  d_dir <- iglm.data(x_attribute = x_dir, y_attribute = y_dir, z_network = z_dir,
                     n_units = n_unit_dir, family_x = "binomial", family_y = "binomial", directed = TRUE)

  # Directed Degree:
  # Out-degrees: 1: 2, 2: 1, 3: 1, 4: 0 -> counts: 0: 1, 1: 2, 2: 1
  # In-degrees: 1: 0, 2: 1, 3: 2, 4: 1 -> counts: 0: 1, 1: 2, 2: 1
  deg_dir_un <- d_dir$degree_distribution(prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(deg_dir_un$out_degree), c(1, 2, 1))
  expect_equal(as.numeric(deg_dir_un$in_degree), c(1, 2, 1))
  # Constrained x_i = 1, y_j = 1: out: (1, 1) -> 0: 0, 1: 2; in: (1, 1) -> 0: 0, 1: 2
  deg_dir_c <- d_dir$degree_distribution(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)
  expect_equal(as.numeric(deg_dir_c$out_degree), c(0, 2))
  expect_equal(as.numeric(deg_dir_c$in_degree), c(0, 2))

  # Directed ESP (OTP):
  # Edges: 1->2 (OTP 1), 1->3 (OTP 0), 2->3 (OTP 0), 3->4 (OTP 0) -> counts: 0: 3, 1: 1
  expect_equal(as.numeric(d_dir$esp_dist(type = "OTP", prob = FALSE, plot = FALSE)), c(3, 1))
  # Constrained x_i = 1, y_j = 1: edges 1->2 (OTP 1), 3->4 (OTP 0) -> counts: 0: 1, 1: 1
  expect_equal(as.numeric(d_dir$esp_dist(type = "OTP", x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)), c(1, 1))

  # Directed DSP (OTP):
  # 12 directed dyads -> 10 with 0, 2 with 1: (1,2) and (2,1)
  expect_equal(as.numeric(d_dir$dsp_dist(type = "OTP", prob = FALSE, plot = FALSE)), c(10, 2))
  # Constrained x_i = 1, y_j = 1: 4 dyads: (1,2)->1, (1,4)->0, (3,2)->0, (3,4)->0 -> 0: 3, 1: 1
  expect_equal(as.numeric(d_dir$dsp_dist(type = "OTP", x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)), c(3, 1))

  # Directed Geodist (shortest paths on symmetrized network):
  # 12 pairs -> dist 1: 8, dist 2: 4, Inf: 0
  expect_equal(as.numeric(d_dir$geo_dist(prob = FALSE, plot = FALSE)), c(8, 4, 0))
  # Constrained x_i = 1, y_j = 1: 4 pairs: (1,2)->1, (1,4)->2, (3,2)->1, (3,4)->1 -> dist 1: 3, dist 2: 1, Inf: 0
  expect_equal(as.numeric(d_dir$geo_dist(x_i = 1, y_j = 1, prob = FALSE, plot = FALSE)), c(3, 1, 0))
})



