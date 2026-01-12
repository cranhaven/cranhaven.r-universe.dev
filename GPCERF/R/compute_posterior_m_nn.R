#' @title
#' Calculate posterior means for nnGP model
#'
#' @description
#' Calculates the posterior mean of a point on the CERF based on the nnGP model.
#' This function also returns the weights assigned to all nearest neighbors when
#' calculating the posterior mean.
#'
#' @param hyperparam A set of hyperparameters in the GP model.
#' @param w  A scaler representing the exposure level for the point of interest
#'  on the CERF.
#' @param gps_w The GPS for all samples when their exposure levels are set
#'  at \code{w}.
#' @param obs_ord A matrix of two columns. First column is the observed
#' exposure levels of all samples; second is the GPS at the observed exposure
#' levels. The rows are in ascending order for the first column.
#' @param y_obs_ord A vector of observed outcome values. The vector is ordered
#' as \code{obs_ord}.
#' @param kernel_fn The covariance function of the GP.
#' @param n_neighbor The number of nearest neighbors on one side.
#' @param block_size Number of samples included in a computation block.
#' Mainly used to balance the speed and memory requirement.
#' Larger \code{block_size} is faster, but requires more memory.
#'
#' @return
#' TODO: The first column is the selected index and the second column is weight.
#' A two-column matrix. The first column is the weights assigned to each
#' nearest neighbor. The second column is the corresponding observed outcome
#' value. The weight in the last row of this matrix is NA and the observed
#' outcome value is the estimated posterior mean of the CERF at point \code{w},
#' which is the weighted sum of all observed outcome values of the neighbors.
#'
#' @keywords internal
#'
compute_posterior_m_nn <- function(hyperparam,
                                   w,
                                   gps_w,
                                   obs_ord,
                                   y_obs_ord,
                                   kernel_fn = function(x) exp(-x ^ 2),
                                   n_neighbor = 10,
                                   block_size = 1e4) {


  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]


  n <- base::length(gps_w)

  if (w >= obs_ord[nrow(obs_ord), 1]) {
    idx_select <- seq(nrow(obs_ord) - n_neighbor + 1, nrow(obs_ord), 1)
  } else {
    # which.max returns the index of first TRUE value.
    idx_anchor <- which.max(obs_ord[, 1] >= w)
    idx_start <- max(1, idx_anchor - n_neighbor)
    idx_end <- min(nrow(obs_ord), idx_anchor + n_neighbor)
    if (idx_end == nrow(obs_ord)) {
      idx_select <- seq(idx_end - n_neighbor * 2 + 1, idx_end, 1)
    } else {
      idx_select <- seq(idx_start, idx_start + n_neighbor * 2 - 1, 1)
    }
  }

  used_obs <- t(t(obs_ord[idx_select, ]) * (1 / sqrt(c(beta, alpha))))
  cov_used_inv <- compute_inverse(g_sigma * kernel_fn(as.matrix(dist(used_obs)))
                                  + diag(nrow(used_obs)))
  used_y <- y_obs_ord[idx_select]

  w_obs <- t(t(cbind(w, gps_w)) * (1 / sqrt(c(beta, alpha))))
  id_all <- split(1:n, ceiling(seq_along(1:n) / block_size))

  all_weights <- sapply(id_all, function(id_ind) {
    cov_cross <- g_sigma * kernel_fn(spatstat.geom::crossdist(w_obs[id_ind, 1],
                                                              w_obs[id_ind, 2],
                                                              used_obs[, 1],
                                                              used_obs[, 2]))
    #weight
    c(arma_mm(cov_used_inv, Rfast::colsums(cov_cross)))
  })
  weights <- Rfast::rowsums(all_weights) / n
  weights[weights < 0] <- 0
  if (sum(weights) > 0) {
    weights <- weights / sum(weights)
  }

  est <- c(used_y %*% weights)

  return(cbind(c(idx_select, NA), c(weights, est)))
}
