test_that("Define a iglm.data object and check all functions", {
  # debugonce(iglm.data)
  tmp <- iglm.data(
    neighborhood = matrix(c(
      0, 1, 1, 0,
      1, 0, 0, 1,
      1, 0, 0, 1,
      0, 1, 1, 0
    ), nrow = 4, byrow = TRUE),
    directed = FALSE,
    family_x = "binomial",
    family_y = "binomial"
  )

  expect_equal(inherits(tmp, "iglm.data"), expected = TRUE)
  expect_equal(tmp$degree()$degree_seq, expected = c(0, 0, 0, 0))
  expect_equal(tmp$mean_x(), expected = 0)
  expect_equal(tmp$mean_y(), expected = 0)
  expect_equal(tmp$mean_z(), expected = 0)
  tmp_name <- paste(tempfile(), ".RDS")
  tmp$save(file = tmp_name)
  rm(tmp)

  loaded_tmp <- iglm.data(file = tmp_name)
  expect_equal(inherits(loaded_tmp, "iglm.data"), expected = TRUE)
  expect_equal(loaded_tmp$degree()$degree_seq, expected = c(0, 0, 0, 0))
  expect_equal(loaded_tmp$mean_x(), expected = 0)
  expect_equal(loaded_tmp$mean_y(), expected = 0)
  expect_equal(loaded_tmp$mean_z(), expected = 0)
  rm(loaded_tmp)
  tmp <- iglm.data(
    z_network = matrix(c(
      0, 1, 1, 0,
      1, 0, 0, 1,
      1, 0, 0, 1,
      0, 1, 1, 0
    ), nrow = 4, byrow = TRUE),
    directed = FALSE,
    n_units = 4, x_attribute = c(0, 0, 1, 0),
    y_attribute = c(0, 1, 0, 1),
    family_x = "binomial",
    family_y = "binomial"
  )
  # debugonce(tmp$degree)
  expect_equal(tmp$degree()$degree_seq, expected = c(2, 2, 2, 2))
  expect_equal(tmp$mean_z(), expected = 4 / 6)
  expect_equal(tmp$mean_x(), expected = 1 / 4)
  expect_equal(tmp$mean_y(), expected = 2 / 4)
  expect_equal(nrow(tmp$overlap) == 12,
    expected = nrow(tmp$neighborhood) == 12
  )
  tmp$save(file = tmp_name)
  rm(tmp)
  loaded_tmp <- iglm.data(file = tmp_name)

  expect_equal(loaded_tmp$degree()$degree_seq, expected = c(2, 2, 2, 2))
  expect_equal(loaded_tmp$mean_z(), expected = 4 / 6)
  expect_equal(loaded_tmp$mean_x(), expected = 1 / 4)
  expect_equal(loaded_tmp$mean_y(), expected = 2 / 4)
  expect_equal(nrow(loaded_tmp$overlap) == 12,
    expected = nrow(loaded_tmp$neighborhood) == 12
  )

  file.remove(tmp_name)
})

test_that("Define a directed iglm.data object and check all functions", {
  tmp <- iglm.data(
    neighborhood = matrix(c(
      0, 1, 1, 0,
      1, 0, 0, 1,
      1, 0, 0, 1,
      0, 1, 1, 0
    ), nrow = 4, byrow = TRUE),
    directed = TRUE,
    family_x = "binomial",
    family_y = "binomial"
  )

  expect_equal(inherits(tmp, "iglm.data"), expected = TRUE)
  expect_equal(tmp$degree()$in_degree_seq, expected = c(0, 0, 0, 0))
  expect_equal(tmp$degree()$out_degree_seq, expected = c(0, 0, 0, 0))
  expect_equal(tmp$mean_x(), expected = 0)
  expect_equal(tmp$mean_y(), expected = 0)
  expect_equal(tmp$mean_z(), expected = 0)

  tmp_name <- paste(tempfile(), ".RDS")
  tmp$save(file = tmp_name)
  rm(tmp)

  loaded_tmp <- iglm.data(file = tmp_name)
  expect_equal(inherits(loaded_tmp, "iglm.data"), expected = TRUE)
  expect_equal(loaded_tmp$degree()$in_degree_seq, expected = c(0, 0, 0, 0))
  expect_equal(loaded_tmp$degree()$out_degree_seq, expected = c(0, 0, 0, 0))
  expect_equal(loaded_tmp$mean_x(), expected = 0)
  expect_equal(loaded_tmp$mean_y(), expected = 0)
  expect_equal(loaded_tmp$mean_z(), expected = 0)
  rm(loaded_tmp)

  tmp <- iglm.data(
    z_network = matrix(c(
      0, 1, 1, 0,
      0, 0, 0, 1,
      0, 0, 0, 1,
      0, 1, 0, 0
    ), nrow = 4, byrow = TRUE),
    directed = TRUE,
    n_units = 4, x_attribute = c(0, 0, 1, 0),
    y_attribute = c(0, 1, 0, 1),
    family_x = "binomial",
    family_y = "binomial"
  )

  expect_equal(tmp$mean_z(), expected = 5 / 12)
  expect_equal(tmp$mean_x(), expected = 1 / 4)
  expect_equal(tmp$mean_y(), expected = 2 / 4)
  network_tmp <- matrix(c(
    0, 1, 1, 0,
    0, 0, 0, 1,
    0, 0, 0, 1,
    0, 1, 0, 0
  ), nrow = 4, byrow = TRUE)
  expect_equal(tmp$degree()$in_degree_seq, expected = colSums(network_tmp))
  expect_equal(tmp$degree()$out_degree_seq, expected = rowSums(network_tmp))
  expect_equal(nrow(tmp$overlap) == 12,
    expected = nrow(tmp$neighborhood) == 12
  )
  tmp$save(file = tmp_name)
  rm(tmp)
  loaded_tmp <- iglm.data(file = tmp_name)
  expect_equal(loaded_tmp$mean_z(), expected = 5 / 12)
  expect_equal(loaded_tmp$mean_x(), expected = 1 / 4)
  expect_equal(loaded_tmp$mean_y(), expected = 2 / 4)
  expect_equal(loaded_tmp$degree()$in_degree_seq, expected = colSums(network_tmp))
  expect_equal(loaded_tmp$degree()$out_degree_seq, expected = rowSums(network_tmp))
  expect_equal(nrow(loaded_tmp$overlap) == 12,
    expected = nrow(loaded_tmp$neighborhood) == 12
  )
  file.remove(tmp_name)
})

test_that("iglm.data validation throws error when attributes or networks contain NA", {
  x_na <- c(0, 1, NA, 0)
  x_clean <- c(0, 1, 0, 0)
  y_na <- c(1, NA, 1, 0)
  y_clean <- c(1, 0, 1, 0)
  z_clean <- matrix(0, 4, 4)
  z_na <- matrix(c(0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 4)

  # Check x_attribute NA
  expect_error(
    iglm.data(x_attribute = x_na, y_attribute = y_clean, z_network = z_clean, n_units = 4, family_x = "normal", family_y = "binomial"),
    pattern = "'x_attribute' contains missing \\(NA/NaN\\) values."
  )

  # Check y_attribute NA
  expect_error(
    iglm.data(x_attribute = x_clean, y_attribute = y_na, z_network = z_clean, n_units = 4, family_x = "normal", family_y = "binomial"),
    pattern = "'y_attribute' contains missing \\(NA/NaN\\) values."
  )

  # Check z_network matrix NA
  expect_error(
    iglm.data(x_attribute = x_clean, y_attribute = y_clean, z_network = z_na, n_units = 4, family_x = "normal", family_y = "binomial"),
    pattern = "'z_network' contains missing \\(NA/NaN\\) values."
  )

  # Check neighborhood matrix NA
  expect_error(
    iglm.data(x_attribute = x_clean, y_attribute = y_clean, z_network = z_clean, neighborhood = z_na, n_units = 4, family_x = "normal", family_y = "binomial"),
    pattern = "'neighborhood' contains missing \\(NA/NaN\\) values."
  )
})

test_that("degree_distribution works with custom x_i, x_j, y_i, y_j parameters", {
  n_units <- 6
  z <- matrix(c(
    0, 1, 1, 0, 0, 0,
    1, 0, 1, 0, 0, 0,
    1, 1, 0, 1, 0, 0,
    0, 0, 1, 0, 1, 1,
    0, 0, 0, 1, 0, 1,
    0, 0, 0, 1, 1, 0
  ), nrow = 6, byrow = TRUE)

  x <- c(1, 1, 0, 0, 1, 0)
  y <- c(0, 1, 1, 0, 0, 1)

  data_dir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = TRUE)
  data_undir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = FALSE)

  # Directed default vs constrained
  deg_dir_def <- data_dir$degree_distribution(plot = FALSE)
  expect_true(is.list(deg_dir_def))
  expect_true("in_degree" %in% names(deg_dir_def))
  expect_true("out_degree" %in% names(deg_dir_def))

  deg_dir_sub <- data_dir$degree_distribution(x_i = 1, y_j = 1, plot = FALSE)
  expect_true(is.list(deg_dir_sub))

  # Undirected default vs constrained
  deg_undir_def <- data_undir$degree_distribution(plot = FALSE)
  expect_true(inherits(deg_undir_def, "table"))

  # Single constraint on undirected network returns table
  deg_undir_xi <- data_undir$degree_distribution(x_i = 1, plot = FALSE)
  expect_true(inherits(deg_undir_xi, "table"))

  # Bilateral (sender + receiver) constraint on undirected network returns list with in and out degrees
  deg_undir_sub <- data_undir$degree_distribution(x_i = 1, y_j = 1, plot = FALSE)
  expect_true(is.list(deg_undir_sub))
  expect_true("out_degree" %in% names(deg_undir_sub))
  expect_true("in_degree" %in% names(deg_undir_sub))

  # Vectorized filtering function
  deg_pred_vec <- data_dir$degree_distribution(x_i = function(x) x == 1, y_j = function(y) y == 1, plot = FALSE)
  expect_equal(deg_pred_vec, deg_dir_sub)

  # Scalar / unvectorized filtering function
  deg_pred_scalar <- data_dir$degree_distribution(x_i = function(x) if (x == 1) TRUE else FALSE, y_j = function(y) if (y == 1) TRUE else FALSE, plot = FALSE)
  expect_equal(deg_pred_scalar, deg_dir_sub)

  # Plotting directed and constrained degree distributions
  pdf(NULL)
  expect_no_error(data_dir$degree_distribution(x_i = 1, y_j = 1, plot = TRUE))
  expect_no_error(data_dir$degree_distribution(plot = TRUE))
  dev.off()
})

test_that("Positional arguments work for distribution methods", {
  n_units <- 5
  z <- matrix(c(
    0, 1, 1, 0, 0,
    1, 0, 1, 0, 0,
    1, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    0, 0, 0, 1, 0
  ), nrow = 5, byrow = TRUE)
  x <- c(1, 0, 1, 0, 1)
  y <- c(0, 1, 0, 1, 0)
  data_obj <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = TRUE)

  expect_no_error(data_obj$edgewise_shared_partner_distribution("OTP", c(0, 3), FALSE, FALSE))
  expect_no_error(data_obj$dyadwise_shared_partner_distribution("OTP", c(0, 3), FALSE, FALSE))
  expect_no_error(data_obj$degree_distribution(c(0, 4), FALSE, FALSE))
})

test_that("Undirected constrained degree accounts for canonical edge orientation", {
  n_units <- 4
  # Only edge is between actor 1 and actor 4 (stored as 1 -> 4)
  z <- matrix(0, nrow = 4, ncol = 4)
  z[1, 4] <- z[4, 1] <- 1
  x <- c(1, 0, 0, 0) # actor 1 has x=1
  y <- c(0, 0, 0, 1) # actor 4 has y=1
  data_obj <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = FALSE)

  # Degree of actor 1 connecting to y=1 receivers (actor 4)
  deg_1 <- data_obj$degree(x_i = 1, y_j = 1)
  expect_equal(unname(deg_1$out_degree_seq), 1)
  expect_equal(unname(deg_1$in_degree_seq), 1)

  # Degree of actor 4 (x=0, y=1) connecting to x=1 receivers (actor 1)
  deg_4 <- data_obj$degree(y_i = 1, x_j = 1)
  expect_equal(unname(deg_4$out_degree_seq), 1)
  expect_equal(unname(deg_4$in_degree_seq), 1)
})

test_that("Undirected degree and degree_distribution are equivalent for x_i = 1 vs x_j = 1", {
  n_units <- 5
  z <- matrix(c(
    0, 1, 1, 0, 0,
    1, 0, 1, 0, 1,
    1, 1, 0, 1, 0,
    0, 0, 1, 0, 1,
    0, 1, 0, 1, 0
  ), nrow = 5, byrow = TRUE)
  x <- c(1, 1, 0, 0, 1) # nodes 1, 2, 5 have x=1
  y <- c(0, 1, 1, 0, 0)

  data_undir <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, family_x = "binomial", family_y = "binomial", directed = FALSE)

  # Single constraint equivalence
  deg_xi <- data_undir$degree(x_i = 1)
  deg_xj <- data_undir$degree(x_j = 1)
  expect_equal(deg_xi, deg_xj)

  # Single constraint degree distribution equivalence
  dist_xi <- data_undir$degree_distribution(x_i = 1, plot = FALSE)
  dist_xj <- data_undir$degree_distribution(x_j = 1, plot = FALSE)
  expect_equal(dist_xi, dist_xj)

  # Double constraint x_i = 1, x_j = 1 implies both ends have x = 1 (subgraph on nodes 1, 2, 5)
  # Edges among {1, 2, 5}: (1,2), (2,5) -> node 1 deg=1, node 2 deg=2, node 5 deg=1
  deg_both <- data_undir$degree(x_i = 1, x_j = 1)
  expect_equal(unname(deg_both$degree_seq), c(1, 2, 1))

  # Identical constraints on undirected network return a single degree table (not in/out list)
  dist_both <- data_undir$degree_distribution(x_i = 1, x_j = 1, plot = FALSE)
  expect_true(inherits(dist_both, "table"))
  expect_false(is.list(dist_both))

  # Single constraint counts full degrees for actors with x=1:
  # Node 1: edges to 2, 3 -> deg = 2
  # Node 2: edges to 1, 3, 5 -> deg = 3
  # Node 5: edges to 2, 4 -> deg = 2
  expect_equal(unname(deg_xi$degree_seq), c(2, 3, 2))
})

test_that("degree_distribution with mode = 'local' is equivalent to the original standalone spillover algorithm", {
  old_spillover_algo <- function(data_obj, x_i = NULL, x_j = NULL, y_i = NULL, y_j = NULL,
                                 prob = TRUE, value_range = NULL) {
    binarize_filter <- function(attr_vec, spec, type = "binomial") {
      if (is.null(spec)) {
        return(rep(TRUE, length(attr_vec)))
      }
      if (is.function(spec)) {
        return(as.logical(spec(attr_vec)))
      }
      if (type == "binomial") {
        attr_vec %in% spec
      } else {
        m <- mean(attr_vec)
        res <- rep(FALSE, length(attr_vec))
        if (1 %in% spec) res <- res | (attr_vec > m)
        if (0 %in% spec) res <- res | (attr_vec <= m)
        other_spec <- spec[!spec %in% c(0, 1)]
        if (length(other_spec) > 0) res <- res | (attr_vec %in% other_spec)
        res
      }
    }

    x_attr <- data_obj$x_attribute
    y_attr <- data_obj$y_attribute
    family_x <- data_obj$family_x
    family_y <- data_obj$family_y
    n_units <- data_obj$n_units
    z_net <- data_obj$z_network
    overlap <- data_obj$overlap

    if (!is.null(x_i) || !is.null(y_i)) {
      cond_i <- rep(TRUE, n_units)
      if (!is.null(x_i)) cond_i <- cond_i & binarize_filter(x_attr, x_i, family_x)
      if (!is.null(y_i)) cond_i <- cond_i & binarize_filter(y_attr, y_i, family_y)
      actors_sender <- which(cond_i)
    } else {
      actors_sender <- which(x_attr > mean(x_attr))
    }

    if (!is.null(x_j) || !is.null(y_j)) {
      cond_j <- rep(TRUE, n_units)
      if (!is.null(x_j)) cond_j <- cond_j & binarize_filter(x_attr, x_j, family_x)
      if (!is.null(y_j)) cond_j <- cond_j & binarize_filter(y_attr, y_j, family_y)
      actors_receiver <- which(cond_j)
    } else {
      actors_receiver <- which(y_attr > mean(y_attr))
    }

    if (length(actors_sender) == 0 || length(actors_receiver) == 0) {
      if (is.null(value_range)) value_range <- c(0, 1)
      tmp1 <- if (length(actors_sender) > 0) rep(0, length(actors_sender)) else numeric(0)
      tmp2 <- if (length(actors_receiver) > 0) rep(0, length(actors_receiver)) else numeric(0)
      out_degree_x_y <- table(factor(tmp1, levels = seq(from = value_range[1], to = value_range[2])))
      in_degree_x_y <- table(factor(tmp2, levels = seq(from = value_range[1], to = value_range[2])))
      if (sum(out_degree_x_y) > 0) out_degree_x_y <- out_degree_x_y / (sum(out_degree_x_y) * prob + (!prob))
      if (sum(in_degree_x_y) > 0) in_degree_x_y <- in_degree_x_y / (sum(in_degree_x_y) * prob + (!prob))
      return(list(out_spillover_degree = out_degree_x_y, in_spillover_degree = in_degree_x_y))
    }

    adj_mat_x_y <- matrix(
      data = NA, nrow = length(actors_sender),
      ncol = length(actors_receiver),
      dimnames = list(actors_sender, actors_receiver)
    )

    overlap_tmp <- if (!is.null(overlap) && nrow(overlap) > 0) {
      matrix(
        overlap[(overlap[, 1] %in% actors_sender) & (overlap[, 2] %in% actors_receiver), ],
        ncol = 2
      )
    } else {
      matrix(integer(0), ncol = 2)
    }
    if (nrow(overlap_tmp) > 0) {
      row_idx <- match(overlap_tmp[, 1], rownames(adj_mat_x_y))
      col_idx <- match(overlap_tmp[, 2], colnames(adj_mat_x_y))
      valid <- !is.na(row_idx) & !is.na(col_idx)
      if (any(valid)) adj_mat_x_y[cbind(row_idx[valid], col_idx[valid])] <- 0
    }
    has_valid_overlap_row <- rowSums(!is.na(adj_mat_x_y)) > 0
    has_valid_overlap_col <- colSums(!is.na(adj_mat_x_y)) > 0
    adj_mat_x_y <- adj_mat_x_y[has_valid_overlap_row, has_valid_overlap_col, drop = FALSE]

    z_net_all <- if (!data_obj$directed && ncol(z_net) == 2 && nrow(z_net) > 0) {
      rbind(z_net, z_net[, c(2, 1), drop = FALSE])
    } else {
      z_net
    }

    edges_x_y <- matrix(
      z_net_all[(z_net_all[, 1] %in% actors_sender) & (z_net_all[, 2] %in% actors_receiver), ],
      ncol = 2
    )
    if (nrow(edges_x_y) > 0 && nrow(adj_mat_x_y) > 0 && ncol(adj_mat_x_y) > 0) {
      which_overlap <- check_overlap(edges_x_y, overlap)
      edges_x_y_overlap <- matrix(edges_x_y[which_overlap, ], ncol = 2)
      if (nrow(edges_x_y_overlap) > 0) {
        row_idx <- match(edges_x_y_overlap[, 1], rownames(adj_mat_x_y))
        col_idx <- match(edges_x_y_overlap[, 2], colnames(adj_mat_x_y))
        valid <- !is.na(row_idx) & !is.na(col_idx)
        if (any(valid)) adj_mat_x_y[cbind(row_idx[valid], col_idx[valid])] <- 1
      }
    }

    tmp1 <- if (nrow(adj_mat_x_y) > 0) rowSums(adj_mat_x_y, na.rm = TRUE) else numeric(0)
    tmp2 <- if (ncol(adj_mat_x_y) > 0) colSums(adj_mat_x_y, na.rm = TRUE) else numeric(0)

    range_out <- if (is.list(value_range)) value_range$out_spillover_degree else (if (!is.null(value_range)) value_range else range(unique(c(tmp1, 0))))
    range_in <- if (is.list(value_range)) value_range$in_spillover_degree else (if (!is.null(value_range)) value_range else range(unique(c(tmp2, 0))))

    out_degree_x_y <- table(factor(tmp1, levels = seq(from = range_out[1], to = range_out[2])))
    in_degree_x_y <- table(factor(tmp2, levels = seq(from = range_in[1], to = range_in[2])))
    if (sum(out_degree_x_y) > 0) out_degree_x_y <- out_degree_x_y / (sum(out_degree_x_y) * prob + (!prob))
    if (sum(in_degree_x_y) > 0) in_degree_x_y <- in_degree_x_y / (sum(in_degree_x_y) * prob + (!prob))

    list(out_spillover_degree = out_degree_x_y, in_spillover_degree = in_degree_x_y)
  }

  set.seed(42)
  n_units <- 15
  block <- matrix(nrow = 5, ncol = 5, data = 1)
  neighborhood <- as.matrix(Matrix::bdiag(replicate(3, block, simplify = FALSE)))
  z_dir <- matrix(rbinom(225, 1, 0.3), 15, 15)
  diag(z_dir) <- 0
  x <- rbinom(15, 1, 0.5)
  y <- rbinom(15, 1, 0.5)

  # Directed network
  d_dir <- iglm.data(
    x_attribute = x, y_attribute = y, z_network = z_dir,
    neighborhood = neighborhood, directed = TRUE,
    family_x = "binomial", family_y = "binomial"
  )

  old_dir <- old_spillover_algo(d_dir, x_i = 1, y_j = 1)
  deg_dir <- d_dir$degree_distribution(x_i = 1, y_j = 1, mode = "local", plot = FALSE)

  expect_equal(as.numeric(old_dir$out_spillover_degree), as.numeric(deg_dir$out_degree))
  expect_equal(as.numeric(old_dir$in_spillover_degree), as.numeric(deg_dir$in_degree))
  expect_equal(names(old_dir$out_spillover_degree), names(deg_dir$out_degree))
  expect_equal(names(old_dir$in_spillover_degree), names(deg_dir$in_degree))

  # Undirected network
  z_undir <- matrix(0, 15, 15)
  for (i in 1:14) {
    for (j in (i + 1):15) {
      val <- rbinom(1, 1, 0.3)
      z_undir[i, j] <- z_undir[j, i] <- val
    }
  }
  d_undir <- iglm.data(
    x_attribute = x, y_attribute = y, z_network = z_undir,
    neighborhood = neighborhood, directed = FALSE,
    family_x = "binomial", family_y = "binomial"
  )

  old_undir <- old_spillover_algo(d_undir, x_i = 1, y_j = 1)
  deg_undir <- d_undir$degree_distribution(x_i = 1, y_j = 1, mode = "local", plot = FALSE)

  expect_equal(as.numeric(old_undir$out_spillover_degree), as.numeric(deg_undir$out_degree))
  expect_equal(as.numeric(old_undir$in_spillover_degree), as.numeric(deg_undir$in_degree))
  expect_equal(names(old_undir$out_spillover_degree), names(deg_undir$out_degree))
  expect_equal(names(old_undir$in_spillover_degree), names(deg_undir$in_degree))
})

test_that("iglm.data supports custom label_x, label_y, and label_z", {
  n_units <- 5
  x <- c(0, 1, 0, 1, 1)
  y <- c(1, 1, 0, 0, 1)
  z <- matrix(c(1, 2, 2, 3, 3, 4), ncol = 2, byrow = TRUE)

  # Default labels
  d_default <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units)
  expect_equal(d_default$label_x, "x")
  expect_equal(d_default$label_y, "y")
  expect_equal(d_default$label_z, "z")

  # Custom labels via constructor
  d_custom <- iglm.data(
    x_attribute = x, y_attribute = y, z_network = z, n_units = n_units,
    label_x = "republican", label_y = "turnout", label_z = "friendship"
  )
  expect_equal(d_custom$label_x, "republican")
  expect_equal(d_custom$label_y, "turnout")
  expect_equal(d_custom$label_z, "friendship")

  # Setters and active bindings
  d_custom$label_x <- "party"
  expect_equal(d_custom$label_x, "party")
  d_custom$set_label_y("vote")
  expect_equal(d_custom$label_y, "vote")
  d_custom$set_label_z("advice")
  expect_equal(d_custom$label_z, "advice")

  # Error on invalid labels
  expect_error(d_custom$set_label_x(""), "single non-empty character string")
  expect_error(d_custom$set_label_y(123), "single non-empty character string")
  expect_error(d_custom$set_label_z(NA_character_), "single non-empty character string")

  # Explicit labels vs defaults
  d_def <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units)
  expect_equal(d_def$label_x, "x")
  expect_equal(d_def$label_y, "y")
  expect_equal(d_def$label_z, "z")

  d_arg <- iglm.data(x_attribute = x, y_attribute = y, z_network = z, n_units = n_units, label_x = "pol_orient", label_y = "participation")
  expect_equal(d_arg$label_x, "pol_orient")
  expect_equal(d_arg$label_y, "participation")

  # Print shows labels
  out_print <- capture.output(d_custom$print())
  expect_true(any(grepl("x_attribute \\[party\\]", out_print)))
  expect_true(any(grepl("y_attribute \\[vote\\]", out_print)))
  expect_true(any(grepl("connections \\[advice\\]", out_print)))

  # Normal attribute print output shows mean and sd only (no redundant scale)
  x_norm <- rnorm(n_units)
  d_norm <- iglm.data(x_attribute = x_norm, y_attribute = y, z_network = z, n_units = n_units, family_x = "normal")
  out_norm <- capture.output(d_norm$print())
  expect_true(any(grepl("normal mean = .*sd = ", out_norm)))
  expect_false(any(grepl("scale =", out_norm)))

  # copenhagen dataset labels
  data(copenhagen)
  expect_equal(copenhagen$label_x, "gender")
  expect_equal(copenhagen$label_y, "duration")
  expect_equal(copenhagen$label_z, "friendship")
})

test_that("iglm.data supports backward compatibility with n_actor argument and field", {
  n_nodes <- 6
  neighborhood <- matrix(1, nrow = n_nodes, ncol = n_nodes)

  # Passing n_actor emits warning and sets n_units
  expect_warning(
    d_old <- iglm.data(neighborhood = neighborhood, directed = FALSE, n_actor = n_nodes),
    regexp = "'n_actor' is deprecated; please use 'n_units' instead."
  )
  expect_equal(d_old$n_units, n_nodes)

  # Reading d$n_actor emits warning and returns n_units
  expect_warning(
    actor_count <- d_old$n_actor,
    regexp = "`n_actor` is deprecated, please use `n_units`."
  )
  expect_equal(actor_count, n_nodes)

  # Reading an older serialized file with n_actor works
  saved_state <- d_old$gather()
  names(saved_state)[names(saved_state) == "n_units"] <- "n_actor"
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)
  saveRDS(saved_state, file = temp_file)

  d_restored <- iglm.data(file = temp_file)
  expect_equal(d_restored$n_units, n_nodes)
})

test_that("iglm.data supports family_x and family_y with strict validation", {
  n_nodes <- 4
  neighborhood <- matrix(1, nrow = n_nodes, ncol = n_nodes)

  # Standard character strings
  d_str <- iglm.data(
    neighborhood = neighborhood, directed = FALSE,
    family_x = "binomial", family_y = "normal",
    x_attribute = c(0, 1, 0, 1), y_attribute = rnorm(n_nodes)
  )
  expect_equal(d_str$family_x, "binomial")
  expect_equal(d_str$family_y, "normal")

  d_pois <- iglm.data(
    neighborhood = neighborhood, directed = FALSE,
    family_x = "poisson", family_y = "binomial",
    x_attribute = c(1, 2, 0, 3), y_attribute = c(0, 1, 0, 1)
  )
  expect_equal(d_pois$family_x, "poisson")
  expect_equal(d_pois$family_y, "binomial")

  # "gaussian" and other undefined families throw validation errors
  expect_error(
    iglm.data(
      neighborhood = neighborhood, directed = FALSE,
      family_x = "gaussian", family_y = "normal",
      x_attribute = rnorm(n_nodes), y_attribute = rnorm(n_nodes)
    ),
    "family_x must be one of 'binomial', 'poisson', or 'normal'."
  )
  expect_error(
    iglm.data(
      neighborhood = neighborhood, directed = FALSE,
      family_x = "normal", family_y = "gaussian",
      x_attribute = rnorm(n_nodes), y_attribute = rnorm(n_nodes)
    ),
    "family_y must be one of 'binomial', 'poisson', or 'normal'."
  )
  expect_error(
    iglm.data(
      neighborhood = neighborhood, directed = FALSE,
      family_x = "undefined_family", family_y = "normal",
      x_attribute = rnorm(n_nodes), y_attribute = rnorm(n_nodes)
    ),
    "family_x must be one of 'binomial', 'poisson', or 'normal'."
  )

  # type_x and type_y do not exist (no backward compatibility)
  expect_null(d_str$type_x)
  expect_null(d_str$type_y)
})

