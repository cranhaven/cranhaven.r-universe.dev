# AMFCC -------------------------------------------------------------------
#'
#'
#' @title Phase I of the Adaptive Multivariate Functional Control Chart (AMFCC).
#' @description This function implements the design phase (Phase I) of the
#' Adaptive Multivariate Functional Control Chart.
#'
#' @param data_tra a data frame with the training data with the following columns:
#'
#' * var: vector of the variable indexes.
#'
#' * curve: vector of the curve indexes.
#'
#' * timeindex: vector of the time indexes corresponding to
#' given elements of \code{grid}.
#'
#' * x: concatenated vector of the observed curves.
#'
#' @param data_tun
#' a data frame with the tuning data with the same structure as \code{data_tra}.
#' If NULL, \code{data_tun} is set to \code{data_tra}.
#' @param grid The vector of time points where the curves are sampled.
#' @param q The dimension of the set of B-spline functions.
#' @param par_seq_list a list with two elements.
#' The first element is a sequence of values for the regularization
#' parameter \eqn{\lambda} and the second element is a sequence of
#' percentages of the total variability  to select \eqn{L}.
#' @param alpha_diagn Type I error probability for the diagnostic.
#' @param alpha_mon Type I error probability for the monitoring.
#' @param ncores number of cores to use for parallel computing
#'
#' @return   A list containing the following arguments:
#'
#'  * \code{statistics_IC}: A matrix with the values of the Hotelling T^2-type
#'    statistics for each observation and parameter combination.
#'
#'  * \code{p_values_combined}: A list with two elements containing the
#'    monitoring statistics obtained with the Fisher omnibus and Tippett combining
#'    functions.
#'
#'  * \code{CL}: The control limits for the monitoring statistics obtained with
#'    the Fisher omnibus and Tippett combining functions.
#'
#'  * \code{contributions_IC}: A list where each element corresponds to a variable
#'     and is a matrix with the contributions to the Hotelling \eqn{T^2}-type
#'     statistics for each observation and parameter combination.
#'
#'  * \code{p_values_combined_cont}: A list where each element corresponds to a
#'    variable and is a list of two elements containing the contribution to the
#'    monitoring statistics obtained with the Fisher omnibus and Tippett
#'    combining functions.
#'
#'  * \code{CL_cont}: The control limits for the contribution to the monitoring
#'  statistics obtained with the Fisher omnibus and Tippett combining functions.
#'
#'  * \code{par_seq_list}: The list of the sequences of the tuning parameters.
#'
#'  * \code{q}: The dimension of the set of B-spline functions.
#'
#'  * \code{basis}: The basis functions used for the functional data representation.
#'
#'  * \code{grid}: The vector of time points where the curves are sampled.
#'
#'  * \code{comb_list_tot}: The matrix with all the parameter combinations.
#'
#'  * \code{mod_pca_list}: The list of the MFPCA models for each value of
#'    \code{lambda_s}.
#' @export
#'
#' @examples
#' \donttest{
#' library(funcharts)
#' N <- 10
#' l_grid <- 10
#' p <- 2
#' grid <- seq(0, 1, l = l_grid)
#'
#'
#' Xall_tra <- funcharts::simulate_mfd(
#'   nobs = N,
#'   p = p,
#'   ngrid = l_grid,
#'   correlation_type_x = c("Bessel", "Gaussian")
#' )
#' X_tra <-
#'   data.frame(
#'     x = c(Xall_tra$X_list[[1]], Xall_tra$X_list[[2]]),
#'     timeindex = rep(rep(1:l_grid, each = (N)), p),
#'     curve = rep(1:(N), l_grid * p),
#'     var = rep(1:p, each = l_grid * N)
#'   )
#'
#' Xall_II <- funcharts::simulate_mfd(
#'   nobs = N,
#'   p = p,
#'   ngrid = l_grid,
#'   shift_type_x = list("A", "B"),
#'   d_x = c(10, 10),
#'   correlation_type_x = c("Bessel", "Gaussian")
#' )
#'
#' X_II <-
#'   data.frame(
#'     x = c(Xall_II$X_list[[1]], Xall_II$X_list[[2]]),
#'     timeindex = rep(rep(1:l_grid, each = (N)), p),
#'     curve = rep(1:(N), l_grid * p),
#'     var = rep(1:p, each = l_grid * N)
#'   )
#'
#' # AMFCC -------------------------------------------------------------------
#' print("AMFCC")
#'
#' mod_phaseI_AMFCC <- AMFCC_PhaseI(
#'   data_tra = X_tra,
#'   data_tun =
#'     NULL,
#'   grid = grid,
#'   ncores = 1
#' )
#'
#' mod_phaseII_AMFCC <- AMFCC_PhaseII(data = X_II,
#' mod_Phase_I = mod_phaseI_AMFCC,
#' ncores = 1)
#'
#' plot(mod_phaseII_AMFCC)
#' plot(mod_phaseII_AMFCC,type='cont',ind_obs=1)
#' }
#'
#' @references
#' Centofanti, F., A. Lepore, and B. Palumbo (2025).
#' An Adaptive Multivariate Functional Control Chart.
#' Accepted for publication in \emph{Technometrics}.
AMFCC_PhaseI <- function(data_tra,
                         data_tun = NULL,
                         grid,
                         q = 30,
                         par_seq_list = list(10^seq(-7, 2, l = 10), c(0.5, 0.7, 0.8, 0.9, 0.99)),
                         alpha_diagn = 0.05,
                         alpha_mon = 0.05,
                         ncores = 1) {
  if (is.null(data_tun)) {
    data_tun <- data_tra
  }

  p_var <- max(data_tra$var)
  grid2 <- seq(0, 1, l = 100)
  N_tra <- length(unique(data_tra$curve))
  basis <- fda::create.bspline.basis(c(grid[1], grid[length(grid)]), nbasis = q)
  lambda_s_seq <- par_seq_list[[1]]
  fev_seq <- par_seq_list[[2]]
  comb_tot <- expand.grid(fev_seq, lambda_s_seq)

  ## Data smoothing training data for different lambda_s
  par_fun <- function(kkk, data) {
    ind_obs <- kkk
    out1 <- get_mfd2(data, ind_obs, basis, grid, lambda_s_seq, gamma_s = 1)
    X_mfd <- out1$X_mfd
    X_mfd_0 <- out1$X_mfd_0
    out <- list(X_mfd = X_mfd, X_mfd_0 = X_mfd_0)
    return(out)
  }



  if (.Platform$OS.type == "unix" |
      ncores == 1) {
    out <- parallel::mclapply(1:N_tra, par_fun, mc.cores = ncores, data = data_tra)
  } else {
    cl <- parallel::makeCluster(ncores)
    # parallel::clusterEvalQ(cl, {
    #   library(funcharts)
    #   library(fda)
    # })
    parallel::clusterExport(cl,
                            c("basis", "grid", "lambda_s_seq", "get_mfd2"),
                            envir = environment())
    out <- parallel::parLapply(cl, 1:N_tra, par_fun, data = data_tra)
  }

  X_mfd_list_1 <- lapply(out, "[[", 1)
  X_mfd_list_tra <- list()
  for (ii in 1:length(lambda_s_seq)) {
    coef_mat <- coef_mat_0 <- array(0, c(q, N_tra, p_var))
    for (jj in 1:N_tra) {
      coef_mat[, jj, ] <- X_mfd_list_1[[jj]]$coefs[, ii, ]
    }
    dimnames(coef_mat)[[3]] <- as.character(1:p_var)
    X_mfd_list_tra[[ii]] <- mfd(
      coef_mat,
      basis,
      fdnames = list("arg", as.character(1:N_tra), as.character(1:p_var)),
      raw = data.frame(
        arg = factor(rep(1:N_tra)),
        id = factor(rep(1:N_tra)),
        matrix(0, N_tra, p_var, dimnames = list(
          as.character(1:N_tra), c(as.character(1:(p_var)))
        ))
      ),
      id_var = "id"
    )
  }

  ## MFPCA
  mod_pca_list <- list()
  for (ii in 1:length(lambda_s_seq)) {
    X_mfd_ii <- X_mfd_list_tra[[ii]]
    mod_pca <- pca_mfd(X_mfd_ii, scale = T, nharm = 300)
    mod_pca_list[[ii]] <- mod_pca

  }

  ## Data smoothing tuning data for different lambda_s
  N_tun <- length(unique(data_tun$curve))
  if (.Platform$OS.type == "unix" | ncores == 1) {
    out <-
      parallel::mclapply(1:N_tun, par_fun, mc.cores = ncores, data = data_tun)
  } else {
    out <- parallel::parLapply(cl, 1:N_tun, par_fun, data = data_tun)
    parallel::stopCluster(cl)
  }

  X_mfd_list_1 <- lapply(out, "[[", 1)
  X_mfd_list_tun <- list()
  for (ii in 1:length(lambda_s_seq)) {
    coef_mat <- coef_mat_0 <- array(0, c(q, N_tun, p_var))
    for (jj in 1:N_tun) {
      coef_mat[, jj, ] <- X_mfd_list_1[[jj]]$coefs[, ii, ]
    }
    dimnames(coef_mat)[[3]] <- as.character(1:p_var)
    X_mfd_list_tun[[ii]] <- mfd(
      coef_mat,
      basis,
      fdnames = list("arg", as.character(1:N_tun), as.character(1:p_var)),
      raw = data.frame(
        arg = factor(rep(1:N_tun)),
        id = factor(rep(1:N_tun)),
        matrix(0, N_tun, p_var, dimnames = list(
          as.character(1:N_tun), c(as.character(1:(p_var)))
        ))
      ),
      id_var = "id"
    )

  }

  ## Computation of monitoring and contribution plot statistics
  ## for each parameter combination
  statistic <- X_mfd_std <- scores_x <- contribution <- list()
  for (ii in 1:length(lambda_s_seq)) {
    X_mfd_ii <- X_mfd_list_tun[[ii]]
    mod_pca_ii <- mod_pca_list[[ii]]
    X_mfd_std[[ii]] = scale_mfd(X_mfd_ii,
                                center = mod_pca_ii$center_fd,
                                scale = mod_pca_ii$scale_fd)
    inprods <- inprod_mfd(X_mfd_std[[ii]], mod_pca_ii$harmonics)
    scores_x[[ii]] <- apply(inprods, 1:2, sum)
    statistic[[ii]] <- matrix(0, N_tun, length(fev_seq))
    contribution[[ii]] <- list()
    for (ll in 1:length(fev_seq)) {
      varprop <- mod_pca_ii$values / sum(mod_pca_ii$values)
      K <- which(cumsum(varprop) >= fev_seq[ll])[1]

      scores_x_ll <- scores_x[[ii]][, 1:K]
      values_ll <- mod_pca_ii$values[1:K]
      if (K > 1) {
        Lambda_inv_ll <- diag(1 / values_ll)
        statistic[[ii]][, ll] <- rowSums((scores_x_ll %*% Lambda_inv_ll) * scores_x_ll)
      }
      if (K == 1) {
        statistic[[ii]][, ll] <- scores_x_ll^2 / values_ll
      }
      contribution[[ii]][[ll]] <-
        sapply(1:p_var, function(variable) {
          rowSums(t(t(inprods[, 1:K, variable] * scores_x_ll) / values_ll))
        })


    }
  }
  statistics <- t(do.call(cbind, statistic))
  scores_x_list <- scores_x
  contribution_list <- contribution
  X_mfd_list_tun_std <- X_mfd_std

  contribution_list <- list()
  for (ii in 1:p_var) {
    contribution_list[[ii]] <- matrix(0, dim(comb_tot)[1], N_tun)
    rr <- 1
    for (jj in 1:length(lambda_s_seq)) {
      for (kk in 1:length(fev_seq)) {
        contribution_list[[ii]][rr, ] <- contribution[[jj]][[kk]][, ii]
        rr <- rr + 1
      }
    }
  }

  ## p-values computation with the Fisher omnibus and Tippett combining functions
  p_values_IC_tot <- sapply(1:dim(comb_tot)[1], function(ii)
    p_value_fun(statistics[ii, ], statistics[ii, ]))
  p_values_combined_tot_fis <-
    combined_pvalues(p_values = p_values_IC_tot, type = "Fisher")
  p_values_combined_tot_tip <-
    combined_pvalues(p_values_IC_tot, type = "Tippett")
  p_values_combined <- list()
  p_values_combined[[1]] <- p_values_combined_tot_fis
  p_values_combined[[2]] <- p_values_combined_tot_tip

  ## Control limits calculation for monitoring
  CL_out <- get_CL(p_values_combined, 1 / alpha_mon)
  CL <- CL_out$CL
  ind_out <- CL_out$ind_out

  ## Control limits calculation for diagnosis
  p_values_cont <- p_value_com_cont_rr <- list()
  p_values_comb_cont_fis <- p_values_comb_cont_tip <- matrix(0, N_tun, p_var)
  CL_cont_mat <- matrix(0, 2, p_var)
  for (rr in 1:p_var) {
    p_values_cont[[rr]] <-
      sapply(1:dim(comb_tot)[1], function(ii)
        p_value_fun(contribution_list[[rr]][ii, ], contribution_list[[rr]][ii, ]))
    p_values_comb_cont_fis[, rr] <-
      combined_pvalues(p_values = p_values_cont[[rr]], type = "Fisher")
    p_values_comb_cont_tip[, rr] <-
      combined_pvalues(p_values_cont[[rr]], type = "Tippett")
    p_value_com_cont_rr[[rr]] <-
      list(p_values_comb_cont_fis[, rr], p_values_comb_cont_tip[, rr])
    CL_cont_mat[, rr] <-
      get_CL(p_value_com_cont_rr[[rr]], 1 / alpha_diagn)$CL
  }

  out <- list(
    statistics_IC = statistics,
    p_values_combined = p_values_combined,
    CL = CL,
    contributions_IC = contribution_list,
    p_values_combined_cont = p_value_com_cont_rr,
    CL_cont = CL_cont_mat,
    par_seq_list = par_seq_list,
    q = q,
    basis = basis,
    grid = grid,
    comb_list_tot = comb_tot,
    mod_pca_list = mod_pca_list
  )
  class(out) <- "AMFCC_PhaseI"
  return(out)
}



#' @title Phase II of the Adaptive Multivariate Functional Control Chart (AMFCC).
#' @description This function implements the monitoring phase (Phase II) of the
#' Adaptive Multivariate Functional Control Chart.
#'
#' @param data a data frame with the testing data with the following columns:
#'
#'           var: vector of the variable indexes.
#'
#'           curve: vector of the curve indexes.
#'
#'           timeindex: vector of the time indexes corresponding to given elements of \code{grid}.
#'
#'           x: concatenated vector of the observed curves.
#'
#' @param mod_Phase_I a list with the output of the Phase I.
#' @param ncores number of cores to use for parallel computing
#'
#' @returns
#'
#' A list containing the following arguments:
#'
#'  * \code{ARL}: The average run length (ARL) for the monitoring statistics
#'    obtained with the Fisher omnibus and Tippett combining functions.
#'
#'  * \code{ARL_cont}: The average run length for the contribution to the
#'    monitoring statistics obtained with the Fisher omnibus and Tippett combining
#'    functions.
#'
#'  * \code{statistics}: A matrix with the values of the Hotelling T^2-type
#'    statistics for each observation and parameter combination.
#'
#'  * \code{contributions}: A list where each element is a matrix with the
#'    contributions to the Hotelling T^2-type statistics for each observation and
#'    parameter combination.
#'
#'  * \code{p_values_combined}: A list with two elements containing the monitoring
#'    statistics obtained with the Fisher omnibus and Tippett combining functions.
#'
#'  * \code{p_values_combined_cont}: A list where each element is a list of two
#'    elements containing the contribution to the monitoring statistics obtained
#'    with the Fisher omnibus and Tippett combining functions.
#'
#'  * \code{CL}: The control limits for the monitoring statistics obtained with
#'    the Fisher omnibus and Tippett combining functions.
#'
#'  * \code{CL_cont}: The control limits for the contribution to the monitoring
#'    statistics obtained with the Fisher omnibus and Tippett combining functions.
#'
#' @export
#'
#' @examples
#' library(funcharts)
#' N <- 10
#' l_grid <- 10
#' p <- 2
#' grid <- seq(0, 1, l = l_grid)
#'
#'
#' Xall_tra <- funcharts::simulate_mfd(
#'   nobs = N,
#'   p = p,
#'   ngrid = l_grid,
#'   correlation_type_x = c("Bessel", "Gaussian")
#' )
#' X_tra <-
#'   data.frame(
#'     x = c(Xall_tra$X_list[[1]], Xall_tra$X_list[[2]]),
#'     timeindex = rep(rep(1:l_grid, each = (N)), p),
#'     curve = rep(1:(N), l_grid * p),
#'     var = rep(1:p, each = l_grid * N)
#'   )
#'
#' Xall_II <- funcharts::simulate_mfd(
#'   nobs = N,
#'   p = p,
#'   ngrid = l_grid,
#'   shift_type_x = list("A", "B"),
#'   d_x = c(10, 10),
#'   correlation_type_x = c("Bessel", "Gaussian")
#' )
#'
#' X_II <-
#'   data.frame(
#'     x = c(Xall_II$X_list[[1]], Xall_II$X_list[[2]]),
#'     timeindex = rep(rep(1:l_grid, each = (N)), p),
#'     curve = rep(1:(N), l_grid * p),
#'     var = rep(1:p, each = l_grid * N)
#'   )
#'
#' # AMFCC -------------------------------------------------------------------
#' print("AMFCC")
#'
#' mod_phaseI_AMFCC <- AMFCC_PhaseI(
#'   data_tra = X_tra,
#'   data_tun =
#'     NULL,
#'   grid = grid,
#'   ncores = 1
#' )
#'
#' mod_phaseII_AMFCC <- AMFCC_PhaseII(data = X_II,
#' mod_Phase_I = mod_phaseI_AMFCC,
#' ncores = 1)
#'
#' plot(mod_phaseII_AMFCC)
#' plot(mod_phaseII_AMFCC,type='cont',ind_obs=1)
#'
#' @references
#' Centofanti, F., A. Lepore, and B. Palumbo (2025).
#' An Adaptive Multivariate Functional Control Chart.
#' Accepted for publication in \emph{Technometrics}.
AMFCC_PhaseII <- function(data = NULL,
                          mod_Phase_I,
                          ncores = 1) {
  statistics_IC_tot <- mod_Phase_I$statistics_IC
  CL_vec <- mod_Phase_I$CL
  par_seq_list <- mod_Phase_I$par_seq_list
  q <- mod_Phase_I$q
  comb_tot <- mod_Phase_I$comb_list_tot
  mod_pca <- mod_Phase_I$mod_pca
  grid <- mod_Phase_I$grid
  mat_stat_interp_list <- mod_Phase_I$mat_stat_interp_list
  statistic_ii_or_IC <- mod_Phase_I$statistic_ii_or_IC
  mod_pca_list <- mod_Phase_I$mod_pca_list
  CL_cont_mat <- mod_Phase_I$CL_cont
  contribution_list_IC <- mod_Phase_I$contributions_IC
  basis <- mod_Phase_I$basis
  p_var <- max(data$var)
  gamma_s <- 1
  fev_seq <- par_seq_list[[2]]
  lambda_s_seq <- par_seq_list[[1]]

  ## Data smoothing of the Phase II data for different lambda_s
  N <- length(unique(data$curve))
  par_fun <- function(kkk) {
    ind_obs <- kkk
    out <- get_mfd2(data, ind_obs, basis, grid, lambda_s_seq, gamma_s = 1)
    X_mfd <- out$X_mfd
    X_mfd_0 <- out$X_mfd_0
    out <- list(X_mfd = X_mfd, X_mfd_0 = X_mfd_0)
    return(out)
  }

  if (.Platform$OS.type == "unix" | ncores == 1) {
    out <-
      parallel::mclapply(1:N, par_fun, mc.cores = ncores)
  } else {
    cl <- parallel::makeCluster(ncores)
    # parallel::clusterEvalQ(cl, {
    #   library(funcharts)
    #   library(fda)
    # })
    parallel::clusterExport(cl, c("basis", "grid", "lambda_s_seq"), envir = environment())
    out <- parallel::parLapply(cl, 1:N, par_fun)
    parallel::stopCluster(cl)
  }

  X_mfd_list_1 <- lapply(out, "[[", 1)
  X_mfd_list <- list()
  for (ii in 1:length(lambda_s_seq)) {
    coef_mat <- coef_mat_0 <- array(0, c(q, N, p_var))
    for (jj in 1:N) {
      coef_mat[, jj, ] <- X_mfd_list_1[[jj]]$coefs[, ii, ]
    }
    dimnames(coef_mat)[[3]] <- as.character(1:p_var)
    X_mfd_list[[ii]] <- mfd(
      coef_mat,
      basis,
      fdnames = list("arg", as.character(1:N), as.character(1:p_var)),
      raw = data.frame(
        arg = factor(rep(1:N)),
        id = factor(rep(1:N)),
        matrix(0, N, p_var, dimnames = list(as.character(1:N), c(
          as.character(1:(p_var))
        )))
      ),
      id_var = "id"
    )
  }

  ## Computation of monitoring and contribution plot statistics
  ## for each parameter combination
  statistic <- X_mfd_std <- scores_x <- contribution <- list()
  for (ii in 1:length(lambda_s_seq)) {
    X_mfd_ii <- X_mfd_list[[ii]]
    mod_pca_ii <- mod_pca_list[[ii]]
    X_mfd_std[[ii]] <-
      scale_mfd(X_mfd_ii,
                center = mod_pca_ii$center_fd,
                scale = mod_pca_ii$scale_fd)
    inprods = inprod_mfd(X_mfd_std[[ii]], mod_pca_ii$harmonics)
    scores_x[[ii]] <- apply(inprods, 1:2, sum)
    statistic[[ii]] <- matrix(0, N, length(fev_seq))
    contribution[[ii]] <- list()
    for (ll in 1:length(fev_seq)) {
      varprop <- mod_pca_ii$values / sum(mod_pca_ii$values)
      K <- which(cumsum(varprop) >= fev_seq[ll])[1]
      scores_x_ll <- scores_x[[ii]][, 1:K]
      values_ll <- mod_pca_ii$values[1:K]

      if (K > 1) {
        Lambda_inv_ll <- diag(1 / values_ll)
        statistic[[ii]][, ll] <-
          rowSums((scores_x_ll %*% Lambda_inv_ll) * scores_x_ll)
      }
      if (K == 1) {
        statistic[[ii]][, ll] <- scores_x_ll^2 / values_ll
      }
      contribution[[ii]][[ll]] <-
        sapply(1:p_var, function(variable) {
          rowSums(t(t(inprods[, 1:K, variable] * scores_x_ll) / values_ll))
        })

    }

  }
  statistics <- t(do.call(cbind, statistic))
  scores_x_list <- scores_x
  X_mfd_list_tun_std <- X_mfd_std
  contribution_list <- list()
  for (ii in 1:p_var) {
    contribution_list[[ii]] <- matrix(0, dim(comb_tot)[1], N)
    rr <- 1
    for (jj in 1:length(lambda_s_seq)) {
      for (kk in 1:length(fev_seq)) {
        contribution_list[[ii]][rr, ] <- contribution[[jj]][[kk]][, ii]
        rr <- rr + 1
      }
    }
  }

  ## p-values computation with the Fisher omnibus and Tippett combining functions
  p_values <-
    sapply(1:dim(comb_tot)[1], function(ii)
      p_value_fun(statistics[ii, ], statistics_IC_tot[ii, ]))
  p_values_combined_tot_fis <-
    combined_pvalues(p_values = p_values, type = "Fisher")
  p_values_combined_tot_tip <-
    combined_pvalues(p_values, type = "Tippett")
  p_values_combined <-
    rbind(p_values_combined_tot_fis, p_values_combined_tot_tip)

  p_values_combined_out <-
    list(p_values_combined_tot_fis, p_values_combined_tot_tip)

  ## ARL calculation
  RL_vec <-
    sapply(1:length(CL_vec), function(ii)
      ARL(p_values_combined[ii, ], CL_vec[ii]))

  ind_out <-
    sapply(1:length(CL_vec), function(ii)
      which(p_values_combined[ii, ] >= CL_vec[ii]))
  ARL <- c(RL_vec)
  p_values_cont <- p_value_com_cont_rr <- ind_out <- list()
  p_values_comb_cont_fis <-
    p_values_comb_cont_tip <- matrix(0, N, p_var)
  RL_mat <- matrix(0, 2, p_var)
  for (rr in 1:p_var) {
    p_values_cont[[rr]] <-
      sapply(1:dim(comb_tot)[1], function(ii)
        p_value_fun(contribution_list[[rr]][ii, ], contribution_list_IC[[rr]][ii, ]))
    p_values_comb_cont_fis[, rr] <-
      combined_pvalues(p_values = p_values_cont[[rr]], type = "Fisher")
    p_values_comb_cont_tip[, rr] <-
      combined_pvalues(p_values_cont[[rr]], type = "Tippett")
    p_value_com_cont_rr[[rr]] <-
      list(p_values_comb_cont_fis[, rr], p_values_comb_cont_tip[, rr])
    RL_mat[, rr] <-
      sapply(1:dim(CL_cont_mat)[1], function(ii)
        ARL(p_value_com_cont_rr[[rr]][[ii]], CL_cont_mat[ii, rr]))
    ind_out[[rr]] <-
      sapply(1:dim(CL_cont_mat)[1], function(ii)
        which(p_value_com_cont_rr[[rr]][[ii]] >= CL_cont_mat[ii, rr]))
  }
  ARL_cont <- 1 / apply(1 / RL_mat, 1, mean)
  names(ARL) <- c("Fisher", "Tippett")
  names(ARL_cont) <- c("Fisher", "Tippett")

  out <- list(
    ARL = ARL,
    ARL_cont = ARL_cont,
    statistics = statistics,
    contributions = contribution_list,
    p_values_combined = p_values_combined_out,
    p_values_combined_cont = p_value_com_cont_rr,
    CL = CL_vec,
    CL_cont = CL_cont_mat
  )
  class(out) <- "AMFCC_PhaseII"
  return(out)
}

#' @title Plot the results of the Phase I and the Phase II of the AMFCC
#' @description This function provides plots of either the monitoring statistics
#'              or the contribution plot for a given observation.
#' @param x The output of  either `AMFCC_PhaseI` or `AMFCC_PhaseII`.
#' @param ... Select the \code{type} of plot to produce either the contribution
#'            plot 'cont' or the monitoring plot 'mon'. Default is 'mon'.
#'            Select the \code{combining_function} to use for the monitoring
#'            plot either 'Fisher' or 'Tippett'. Default is 'Fisher'.
#'            Set the observation index \code{ind_obs} for which producing the
#'            contribution plot.
#'
#' @return No return value, called for side effects.
#' @rdname plot.AMFCC_PhaseI
#' @method plot AMFCC_PhaseI
#' @export
#' @inherit AMFCC_PhaseI return examples
plot.AMFCC_PhaseI <- function(x, ...) {

  variables <- NULL
  contribution <- NULL
  ooc <- NULL
  xend <- NULL
  limit <- NULL

  aa <- list(...)
  mod <- x
  if (is.null(aa$combining_function)) {
    print("The Fisher omnibus combining function is considered.")
    aa$combining_function = "Fisher"
  }
  if (aa$combining_function == "Fisher")
    ind_comb <- 1
  else
    ind_comb <- 2

  if (is.null(aa$type))
    type <- 'mon'
  else
    type <- aa$type


  if (type == 'mon') {
    plot(
      mod$p_values_combined[[ind_comb]],
      ylim = c(0, range(mod$p_values_combined[[ind_comb]])[2] * 1.2),
      cex.axis = 0.7,
      cex.lab = 0.8,
      ylab = expression(paste("T"["i,F"]^"2")),
      xlab = "Observation index",
      mgp = c(1.5, 0.5, 0),
      pch = 16,
      cex = 0.3
    )
    graphics::lines(mod$p_values_combined[[ind_comb]], , lwd = 0.6)
    graphics::abline(h = mod$CL[ind_comb], lty = 2)
  }
  if (type == "cont") {
    if (is.null(aa$ind_obs))
      stop("The ind_obs must be provided for the contribution plot.")
    else
      ind_obs <- aa$ind_obs

    p <- length(mod$p_values_combined_cont)
    pvalues <- sapply(1:p, function(ii)
      mod$p_values_combined_cont[[ii]][[ind_comb]][ind_obs])
    alpha_vec <- rep(1, p)
    alpha_vec[pvalues <= mod$CL_cont[ind_comb, ]] = 0.5
    sss <- sapply(1:p, function(ii)
      as.expression(bquote("X"[.(ii)])))
    rr <- as.expression(bquote(c[ik]^T^2))
    rr <- expression(c[ik]^T[F]^2)
    df <- data.frame(
      contribution = pvalues,
      variables = paste0("X", 1:p),
      x = 1:p - 0.35,
      xend = 1:p + 0.35,
      limit = mod$CL_cont[ind_comb, ],
      ooc = pvalues > mod$CL_cont[ind_comb, ],
      alpha_vec = alpha_vec
    )
    df <- dplyr::mutate(df, variables = factor(variables, levels = paste0("X", 1:p)))
    plot <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(
        x = variables,
        y = contribution,
        fill = ooc,
        alpha = I(alpha_vec)
      ),
      width = 0.7) +
      ggplot2::theme_bw() + ggplot2::xlab("") +
      ggplot2::ggtitle("") +
      ggplot2::labs(y = rr) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
            legend.position = "none") +
      ggplot2::scale_fill_manual(values = c("FALSE" = "grey", "TRUE" = "tomato1")) +
      ggplot2::geom_segment(ggplot2::aes(
        x = x,
        xend = xend,
        y = limit,
        yend = limit
      ),
      col = "black") +
      ggplot2::scale_x_discrete(labels = sss) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.placement = "outside",
        axis.title =  ggplot2::element_text(size = 30),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 50),
        axis.title.y = ggplot2::element_text(size = 40),
        axis.title.x = ggplot2::element_text(size = 40),
        axis.text.x =  ggplot2::element_text(size = 30),
        axis.text.y =  ggplot2::element_text(size = 30),
        legend.key.width = ggplot2::unit(2, "cm")
      )
    print(plot)
  }
}
#' @rdname plot.AMFCC_PhaseI
#' @method plot AMFCC_PhaseII
#' @export
#'
plot.AMFCC_PhaseII <- function(x, ...) {
  plot.AMFCC_PhaseI(x, ...)
}

# Additional functions ----------------------------------------------------

get_mfd2 <- function(data,
                     ind_obs,
                     basis,
                     grid,
                     lambda_s_seq,
                     gamma_s) {
  q <- basis$nbasis
  p_var <- max(data$var)
  N_data <- length(unique(data$curve))
  W_der <- fda::eval.penalty(basis, 2)
  coef_mat <-
    coef_mat_0 <- array(0, c(q, length(lambda_s_seq), p_var))

  X_jj_list <- grid_i_list <- S_list <- list()
  for (jj in 1:p_var) {
    X_jj_list[[jj]] <- data$x[data$curve == ind_obs &
                                data$var == jj]
    grid_i_list[[jj]] <-
      grid[data$timeindex[data$curve == ind_obs & data$var == jj]]
    S_list[[jj]] <- fda::eval.basis(grid_i_list[[jj]], basis)
  }

  for (ii in 1:length(lambda_s_seq)) {
    lambda_s <- lambda_s_seq[ii]
    for (jj in 1:p_var) {
      X <- X_jj_list[[jj]]
      grid_i <- grid_i_list[[jj]]
      S <- S_list[[jj]]
      coef_mat_0[, ii, jj] <-
        chol2inv(chol(t(S) %*% S + lambda_s * W_der)) %*% t(S) %*% X
    }

    mat_mu_start_vec <- coef_mat_0[, ii, ]
    AA <- diag(t(mat_mu_start_vec) %*% W_der %*% mat_mu_start_vec)
    weight_s <- 1 / abs(AA)^gamma_s
    weight_s <- (weight_s / sum(weight_s)) * p_var

    for (jj in 1:p_var) {
      X <- X_jj_list[[jj]]
      grid_i <- grid_i_list[[jj]]
      S <- S_list[[jj]]
      lambda_sada <- lambda_s * weight_s[jj]
      coef_mat[, ii, jj] <-
        chol2inv(chol(t(S) %*% S + lambda_sada * W_der)) %*% t(S) %*% X
    }
  }

  dimnames(coef_mat_0)[[3]] <- as.character(1:p_var)
  X_mfd_0 <- mfd(
    coef_mat_0,
    basis,
    fdnames = list("arg", as.character(1:N_data), as.character(1:p_var)),
    raw = data.frame(
      arg = factor(rep(1:N_data)),
      id = factor(rep(1:N_data)),
      matrix(0, N_data, p_var, dimnames = list(
        as.character(1:N_data), c(as.character(1:(p_var)))
      ))
    ),
    id_var = "id"
  )
  dimnames(coef_mat)[[3]] <- as.character(1:p_var)
  X_mfd <- mfd(
    coef_mat,
    basis,
    fdnames = list("arg", as.character(1:N_data), as.character(1:p_var)),
    raw = data.frame(
      arg = factor(rep(1:N_data)),
      id = factor(rep(1:N_data)),
      matrix(0, N_data, p_var, dimnames = list(
        as.character(1:N_data), c(as.character(1:(p_var)))
      ))
    ),
    id_var = "id"
  )
  out <- list(X_mfd = X_mfd, X_mfd_0 = X_mfd_0)
  return(out)
}
p_value_fun <- function(x, x_IC) {
  p_value <- numeric()
  p_value <-
    sapply(1:length(x), function(ii)
      (length(which(x_IC > x[ii])) + 1)) / ((length(x_IC) + 1))
  p_value
}
combined_pvalues <-
  function(p_values,
           type = "Tippett",
           thre = 0.2,
           ii = NULL) {
    if (type == "Tippett") {
      p_values_comb = -2 * log(apply(p_values, 1, min, na.rm = T))
    }
    else if (type == "Fisher") {
      p_values_comb = -2 * apply(log(p_values), 1, function(x)
        sum(x, na.rm = T) / length(which(!is.na(x))))
    }
    return(p_values_comb)

  }
ARL <- function(x, h, q = 1) {
  if (is.null(dim(x))) {
    x_q <- x[q:length(x)]
    1 / (length(which(x_q >= h)) / length(x_q))
  }
  else{
    x_q <- c(x)
    1 / (length(which(x_q >= h)) / length(x_q))
  }


}
get_CL <- function(p_values_combined, ARL_0) {
  if (is.list(p_values_combined)) {
    CL <- numeric()
    ind_out <- list()
    for (ww in 1:length(p_values_combined)) {
      # CL[ww] <- quantile(p_values_combined[[ww]], 1 - (1 / ARL_0))
      CL[ww] = spatstat.univar::quantile.density(stats::density(p_values_combined[[ww]], bw = "SJ"), 1 -
                                                   (1 / ARL_0))
      x_q <- c(p_values_combined[[ww]])
      ind_out[[ww]] <- which(x_q >= CL[ww])
    }
  }
  else{
    CL <- stats::quantile(p_values_combined, 1 - (1 / ARL_0))
    x_q <- c(p_values_combined)
    ind_out <- which(x_q >= CL)
  }
  out <- list(CL = CL, ind_out = ind_out)
}
