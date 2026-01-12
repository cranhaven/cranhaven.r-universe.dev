#' @title
#' Calculate posterior standard deviations for nnGP model
#'
#' @description
#' Calculates the posterior standard deviation of a point on the CERF based on
#' the nnGP model.
#'
#' @param hyperparam The values of hyperparameters in the GP model.
#' @param w  The exposure level for the point of interest on the CERF.
#' @param gps_w The GPS for all samples when their exposure levels are set
#' at \code{w}.
#' @param obs_ord A matrix of two columns. The first column is the observed
#' exposure levels of all samples; the second is the GPS at the observed
#' exposure levels. The rows are in ascending order for the first column.
#' @param sigma2 A scaler representing \code{sigma^2}.
#' @param kernel_fn The covariance function of the GP.
#' @param n_neighbor Number of nearest neighbors on one side.
#' @param block_size Number of samples included in a computation block.
#' Mainly used to balance the speed and memory requirement.
#' Larger \code{block_size} is faster, but requires more memory.
#'
#' @return
#' The posterior standard deviation of the estimated CERF at \code{w}.
#'
#' @keywords internal
#'
compute_posterior_sd_nn <-  function(hyperparam,
                                     w,
                                     gps_w,
                                     obs_ord,
                                     sigma2,
                                     kernel_fn = function(x) exp(-x ^ 2),
                                     n_neighbor = 10,
                                     block_size = 1e4) {

  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]


  n <- length(gps_w)
  # Compute number of blocks
  n_block <- base::ceiling(n / block_size)

  if (w >= obs_ord[nrow(obs_ord), 1]) {
    idx_all <- seq(nrow(obs_ord) - n_neighbor + 1, nrow(obs_ord), 1)
  } else {
    idx_anchor <- which.max(obs_ord[, 1] >= w)
    idx_start <- max(1, idx_anchor - n_neighbor)
    idx_end <- min(nrow(obs_ord), idx_anchor + n_neighbor)
    if (idx_end == nrow(obs_ord)) {
      idx_all <- seq(idx_end - n_neighbor * 2 + 1, idx_end, 1)
    } else {
      idx_all <- seq(idx_start, idx_start + n_neighbor * 2 - 1, 1)
    }
  }

  obs_use <- t(t(obs_ord[idx_all, ]) * (1 / sqrt(c(beta, alpha))))
  cov_use_inv <- compute_inverse(sigma2 *
                   (g_sigma * kernel_fn(as.matrix(dist(obs_use))) +
                              diag(nrow(obs_use))))
  obs_new <- t(t(cbind(w, gps_w)) * (1 / sqrt(c(beta, alpha))))

  id_all <- split(1:n, ceiling(seq_along(1:n) / n_block))
  #within variance
  sigma_sq1 <- (sum(sapply(id_all, function(id_ind) {
    dist_block <- abs(Rfast::Outer(obs_new[id_ind, 2], obs_new[, 2], "-"))
    Sigma_block <- sigma2 * g_sigma * kernel_fn(dist_block)
    sum(Sigma_block)
  })) + sigma2 * n) / n ^ 2

  #cross variance
  #also use block to free up memories
  cross_cov_colS <- Rfast::rowsums(sapply(id_all, function(id_ind) {
    cross_cov <- sigma2 * g_sigma *
                  kernel_fn(spatstat.geom::crossdist(obs_new[id_ind, 1],
                                                     obs_new[id_ind, 2],
                                                     obs_use[, 1],
                                                     obs_use[, 2]))
    Rfast::colsums(cross_cov)
  }))

  cross_cov_mult <- c(arma_mm(cov_use_inv, cross_cov_colS))

  sigma_sq2 <- c(cross_cov_colS %*% cross_cov_mult) / n ^ 2
  posterior_sd <- sqrt(sigma_sq1 - sigma_sq2)

  logger::log_debug("w: {w}, sigma_sq1: {sigma_sq1}, sigma_sq2: {sigma_sq2},",
                    "sigma2: {sigma2}")

  return(posterior_sd)
}
