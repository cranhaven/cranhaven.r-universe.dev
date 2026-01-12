#' @title
#' Calculate derivatives of CERF for nnGP
#'
#' @description
#' Calculates the posterior mean of the derivative of CERF at a given
#' exposure level with nnGP.
#'
#' @param w A scalar of exposure level of interest.
#' @param w_obs A vector of observed exposure levels of all samples.
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FALSE
#' @param y_obs A vector of observed outcome values.
#' @param hyperparam A vector of hyper-parameters in the GP model.
#' @param n_neighbor The number of nearest neighbors on one side.
#' @param block_size The number of samples included in a computation block.
#' Mainly used to balance the speed and memory requirement. Larger
#' \code{block_size} is faster, but requires more memory.
#' @param kernel_fn The covariance function. The input is the square of
#' Euclidean distance.
#' @param kernel_deriv_fn The partial derivative of the covariance function.
#' The input is the square of Euclidean distance.
#'
#' @return
#' A scalar of estimated derivative of CERF at \code{w} in nnGP.
#'
#' @keywords internal
#'
compute_deriv_nn <- function(w,
                             w_obs,
                             gps_m,
                             y_obs,
                             hyperparam,
                             n_neighbor,
                             block_size,
                             kernel_fn = function(x) exp(-x),
                             kernel_deriv_fn = function(x) -exp(-x)) {


  # Get hyperparameters
  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]


  # Get gps and helper functions
  gps <- gps_m$gps$gps
  e_gps_pred <- gps_m$gps$e_gps_pred
  e_gps_std <- gps_m$gps$e_gps_std
  dnorm_log <- gps_m$used_params$dnorm_log

  gps_w <- dnorm(w, mean = e_gps_pred, sd = e_gps_std, log = dnorm_log)

  n <- length(gps_w)
  n_block <- ceiling(n / block_size)
  obs_raw <- cbind(w_obs, gps)
  obs_ord <- obs_raw[order(obs_raw[, 1]), ]
  y_obs_ord <- y_obs[order(obs_raw[, 1])]


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
  y_use <- y_obs_ord[idx_all]

  obs_new <- t(t(cbind(w, gps_w)) * (1 / sqrt(c(beta, alpha))))
  id_all <- split(1:n, ceiling(seq_along(1:n) / n_block))
  # sigma refers to capital Sigma in the paper.
  sigma_obs <- g_sigma * kernel_fn(as.matrix(dist(obs_use)) ^ 2) +
               diag(nrow(obs_use))
  sigma_obs_inv <- chol2inv(chol(sigma_obs))

  all_weights <- sapply(id_all, function(id_ind) {

    # TODO: change index to column name.
    cross_dist <- spatstat.geom::crossdist(obs_new[id_ind, 1],
                                           obs_new[id_ind, 2],
                                           obs_use[, 1], obs_use[, 2])


    sigma_cross <- g_sigma * (1 / beta) *
                               (2 * outer(rep(w, length(id_ind)) * (1 / beta),
                                              obs_use[, 1], "-")) *
                                              kernel_deriv_fn(cross_dist ^ 2)
    #mean
    wght <- sigma_cross %*% sigma_obs_inv
    colSums(wght)
  })
  weights <- rowSums(all_weights) / n

  return(weights %*% y_use)
}
