# AMFEWMA -----------------------------------------------------------------
#' Adaptive Multivariate Functional EWMA control chart - Phase I
#'
#' This function performs Phase I of the
#' Adaptive Multivariate Functional EWMA (AMFEWMA) control chart proposed
#' by Capezza et al. (2024)
#'
#' @param mfdobj
#' An object of class \code{mfd} containing the Phase I multivariate
#' functional data set, to be used to train the multivariate functional
#' principal component analysis model.
#' @param mfdobj_tuning
#' An object of class \code{mfd} containing the Phase I multivariate
#' functional data set, to be used as tuning data set to estimate the
#' AMFEWMA control chart limit.
#' @param lambda
#' lambda parameter to be used in the score function.
#' See Equation (7) or (8) of Capezza et al. (2024).
#' If it is provided, it must be a number between zero and one.
#' If NULL, it is chosen through the selected according to the
#' optimization procedure presented in Section 2.4 of Capezza et al. (2024).
#' In this case, it is chosen among the values of
#' \code{optimization_pars$lambda_grid}.
#' Default value is NULL.
#' @param k
#' k parameter to be used in the score function.
#' See Equation (7) or (8) of Capezza et al. (2024).
#' If it is provided, it must be a number greater than zero.
#' If NULL, it is chosen through the selected according to the
#' optimization procedure presented in Section 2.4 of Capezza et al. (2024).
#' In this case, it is chosen among the values of
#' \code{optimization_pars$k_grid}.
#' Default value is NULL.
#' @param ARL0
#' The nominal in-control average run length.
#' Default value is 200.
#' @param bootstrap_pars
#' Parameters of the bootstrap procedure described in
#' Section 2.4 of Capezza et al. (2024) for the estimation of the
#' control chart limit.
#' It must be a list with two arguments.
#' \code{n_seq} is the number of bootstrap sequences to be generated.
#' \code{l_seq} is the length of each bootstrap sequence, i.e., the number
#' of observations to be sampled with replacement from the tuning set.
#' Default value is \code{list(n_seq = 200, l_seq = 2000)}.
#' @param optimization_pars
#' Parameters to be used in the optimization procedure described in Section
#' 2.4 of Capezza et al. (2024) for the selection of the parameters
#' lambda and k.
#' It must be a list of the following parameters.
#' \code{lambda_grid} contains the possible values of
#' the parameter \code{lambda}.
#' \code{k_grid} contains the possible values of the parameter \code{k}.
#' \code{epsilon} is the parameter used in Equation (10) of
#' Capezza et al. (2024).
#' When performing the parameter optimization,
#' first the parameters lambda and k are selected to minimize the ARL
#' with respect to a large shift,
#' then the same parameters are chosen to minimize the ARL
#' with respect to a small shift, given that the resulting ARL with respect
#' to the previous large shift does not increase, in percentage,
#' more than \code{epsilon}*100.
#' Default value is 0.1.
#' \code{sd_small} is a positive constant that multiplies the
#' standard deviation function to define the small shift delta_1 in Section
#' 2.4 of Capezza et al. (2024). In fact, the small shift is defined as
#' delta_1(t) = mu_0(t) + \code{sd_small} * sigma(t), where
#' mu_0(t) is the estimated in-control mean function and
#' sigma(t) is the estimated standard deviation function.
#' Default value is 0.25.
#' \code{sd_big} is a positive constant that multiplies the
#' standard deviation function to define the large shift delta_2 in Section
#' 2.4 of Capezza et al. (2024). In fact, the large shift is defined as
#' delta_2(t) = mu_0(t) + \code{sd_large} * sigma(t), where
#' mu_0(t) is the estimated in-control mean function and
#' sigma(t) is the estimated standard deviation function.
#' Default value is 2.
#' @param discrete_grid_length
#' The number of equally spaced argument values at which the \code{mfd}
#' objects are discretized.
#' Default value is 25.
#' @param score_function
#' Score function to be used in Equation (7) or (8) of Capezza et al. (2024),
#' to calculate the weighting parameter of the EWMA statistic
#' for each observation of the sequence.
#' Two values are possible.
#' If "huber", it uses the score function (7)
#' inspired by the Huber's function.
#' If "tukey", it uses the score function (8)
#' inspired by the Tukey's bisquare function.
#' @param fev
#' Number between 0 and 1 denoting the fraction
#' of variability that must be explained by the
#' principal components to be selected after
#' applying multivariate functional principal component analysis
#' on \code{mfdobj}. Default is 0.9.
#' @param n_skip
#' The upper control limit of the AMFEWMA control chart is set
#' to achieve a desired in-control ARL, evaluated after the
#' monitoring statistic has reached steady state.
#' A monitoring statistic is in a steady state
#' if the process has been in control long enough
#' for the effect of the starting value to become negligible
#' (Lucas and Saccucci, 1990).
#' In this regard, the first \code{n_skip} observations
#' are excluded from the calculation of the run length.
#' Default value is 100.
#' @references
#' Capezza, C., Capizzi, G., Centofanti, F., Lepore, A., Palumbo, B. (2025)
#' An Adaptive Multivariate Functional EWMA Control Chart.
#' \emph{Journal of Quality Technology},  57(1):1--15,
#' doi:https://doi.org/10.1080/00224065.2024.2383674.
#'
#' Lucas, J. M., Saccucci, M. S. (1990)
#' Exponentially weighted moving average control schemes:
#' properties and enhancements. \emph{Technometrics}, 32(1), 1-12.
#'
#' @return
#' A list with the following elements.
#' \code{lambda} is the selected lambda parameter.
#' \code{k} is the selected k parameter.
#' \code{mod_1} contains the estimated Phase I model. It is a list with
#' the following elements.
#'
#' * \code{mfdobj} the \code{mfdobj} object passed as input to this function,
#'
#' * \code{mfdobj_tuning} the \code{mfdobj_tuning} object
#' passed as input to this function,
#'
#' * \code{inv_sigmaY_reg}: the matrix containing the discretized
#' version of the function K^*(s,t) defined in Equation (9) of
#' Capezza et al. (2024),
#'
#' * \code{mean_mfdobj}: the estimated mean function,
#'
#' * \code{h}: the calculated upper control limit of the AMFEWMA control chart,
#'
#' * \code{ARL0}: the estimated in-control ARL, which should be close to the
#' nominal value passed as input to this function,
#'
#' * \code{lambda}: the lambda parameter selected by the optimization
#' procedure described in Section 2.4 of Capezza et al. (2024).
#'
#' * \code{k}: The function C_j(t)=k sigma_j(t) appearing in the score
#' functions (7) and (8) of Capezza et al. (2024).
#'
#' * \code{grid_points}: the grid containing the points over which
#' the functional data are discretized before computing the AMFEWMA monitoring
#' statistic and estimating all the model parameters.
#'
#' * \code{V2_mat}: the \code{n_seq}X\code{l_seq} matrix containing,
#' in each column, the AMFEWMA monitoring statistic values of each
#' bootstrap sequence.
#' This matrix is used to set the control chart limit \code{h} to
#' ensure that the desired average run length is achieved.
#'
#' * \code{n_skip}: the \code{n_skip} input parameter passed to this function,
#'
#' * \code{huber}: if the input parameter \code{score_function} is
#' \code{"huber"}, this is TRUE, else is FALSE,
#'
#' * \code{vectors}: the discretized eigenfunctions psi_l(t) of
#' the covariance function, appearing in Equation (9) of Capezza et al. (2024).
#'
#' * \code{values}: the eigenvalues rho_l of
#' the covariance function, appearing in Equation (9) of Capezza et al. (2024).
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' library(funcharts)
#' dat_I <- simulate_mfd(nobs = 200,
#'                       correlation_type_x = c("Bessel", "Bessel", "Bessel"),
#'                       sd_x = c(0.3, 0.3, 0.3))
#' dat_tun <- simulate_mfd(nobs = 200,
#'                         correlation_type_x = c("Bessel", "Bessel", "Bessel"),
#'                         sd_x = c(0.3, 0.3, 0.3))
#' dat_II <- simulate_mfd(nobs = 20,
#'                        correlation_type_x = c("Bessel", "Bessel", "Bessel"),
#'                        shift_type_x = c("C", "C", "C"),
#'                        d_x = c(2, 2, 2),
#'                        sd_x = c(0.3, 0.3, 0.3))
#' mfdobj_I <- get_mfd_list(dat_I$X_list, lambda = 1e-2)
#' mfdobj_tun <- get_mfd_list(dat_tun$X_list, lambda = 1e-2)
#' mfdobj_II <- get_mfd_list(dat_II$X_list, lambda = 1e-2)
#'
#' # p <- plot_mfd(mfdobj_I[1:100])
#' # lines_mfd(p, mfdobj_II, col = "red")
#'
#'
#' mod <- AMFEWMA_PhaseI(mfdobj = mfdobj_I,
#'                       mfdobj_tuning = mfdobj_tun,
#'                       lambda = 0.1,
#'                       k = c(1, 2))
#'
#' cc <- AMFEWMA_PhaseII(mfdobj_2 = rbind_mfd(mfdobj_I[1:100], mfdobj_II),
#'                       mod_1 = mod)
#' plot_control_charts(cc$cc, nobsI = 100)
#' }
#'
#'
AMFEWMA_PhaseI <- function(mfdobj,
                           mfdobj_tuning,
                           lambda = NULL,
                           k = NULL,
                           ARL0 = 200,
                           bootstrap_pars = list(n_seq = 200,
                                                 l_seq = 2000),
                           optimization_pars = list(
                             lambda_grid = c(0.1, 0.2, 0.3, 0.5, 1),
                             k_grid = c(1, 2, 3, 4),
                             epsilon = 0.1,
                             sd_small = 0.25,
                             sd_big = 2
                           ),
                           discrete_grid_length = 25,
                           score_function = "huber",
                           fev = 0.9,
                           n_skip = 100) {

  if (!is.null(lambda)) {
    optimization_pars$lambda_grid <- lambda
  }
  if (!is.null(k)) {
    optimization_pars$k_grid <- k
  }

  if (!is.list(optimization_pars)) {
    stop("optimization_pars must be a list.")
  }

  if (is.null(optimization_pars$lambda_grid)) {
    optimization_pars$lambda_grid <- c(0.1, 0.2, 0.3, 0.5, 1)
  }

  if (is.null(optimization_pars$k_grid)) {
    optimization_pars$k_grid <- c(1, 2, 3, 4)
  }

  if (is.null(optimization_pars$epsilon)) {
    optimization_pars$epsilon <- 0.1
  }

  if (is.null(optimization_pars$sd_small)) {
    optimization_pars$sd_small <- 0.25
  }

  if (is.null(optimization_pars$sd_big)) {
    optimization_pars$sd_big <- 2
  }

  if (!is.list(bootstrap_pars)) {
    stop("bootstrap_pars must be a list.")
  }

  if (is.null(bootstrap_pars$n_seq)) {
    bootstrap_pars$n_seq <- 200
  }

  if (is.null(bootstrap_pars$l_seq)) {
    bootstrap_pars$l_seq <- 2000
  }

  lambda_grid <- optimization_pars$lambda_grid
  k_grid <- optimization_pars$k_grid
  epsilon <- optimization_pars$epsilon
  sd_small <- optimization_pars$sd_small
  sd_big <- optimization_pars$sd_big
  n_seq <- bootstrap_pars$n_seq
  l_seq <- bootstrap_pars$l_seq

  ###########################################################################
  # Algorithm to select lambda and k ----------------------------------------
  ###########################################################################
  sd_mfdobj <- fda::sd.fd(mfdobj)

  p <- dim(mfdobj$coef)[3]
  n_basis <- dim(mfdobj$coef)[1]
  nobs_tun <- dim(mfdobj_tuning$coef)[2]

  ## basis coefficients of out-of-control observations
  ## with small shift (0.5*sd_mfdobj)
  coef_small <- array(0, dim = c(n_basis, nobs_tun, p))
  ## with large shift (2*sd_mfdobj)
  coef_big <- array(0, dim = c(n_basis, nobs_tun, p))
  sd_mfdobj_coefs <- array(sd_mfdobj$coefs, dim = c(n_basis, 1, p))
  for (hh in 1:nobs_tun) {
    mfdobj_tuning_coefs_hh <- mfdobj_tuning$coefs[, hh, , drop = FALSE]
    coef_small[, hh,] <- sd_mfdobj_coefs * sd_small + mfdobj_tuning_coefs_hh
    coef_big[, hh,] <- sd_mfdobj_coefs * sd_big + mfdobj_tuning_coefs_hh
  }
  fase2_small <- mfd(coef_small, mfdobj_tuning$basis, mfdobj_tuning$fdnames)
  fase2_big <- mfd(coef_big, mfdobj_tuning$basis, mfdobj_tuning$fdnames)



  # calculate ARL for each combination of lambda and k
  n_lambda <- length(lambda_grid)
  n_k <- length(k_grid)
  matrix_ARL_big <- matrix(0, n_k, n_lambda)
  matrix_ARL_small <- matrix(0, n_k, n_lambda)
  rownames(matrix_ARL_big) <- k_grid
  colnames(matrix_ARL_big) <- lambda_grid
  rownames(matrix_ARL_small) <- k_grid
  colnames(matrix_ARL_small) <- lambda_grid

  mod_1_AMFEWMA_list <- list()
  for (ii in 1:n_lambda) {
    mod_1_AMFEWMA_list[[ii]] <- list()
    for (jj in 1:n_k) {
      lambda <- lambda_grid[ii]
      k <- k_grid[jj]
      mod_1_AMFEWMA_list[[ii]][[jj]] <-
        AMFEWMA_PhaseI_given_pars(mfdobj = mfdobj,
                                  mfdobj_tuning = mfdobj_tuning,
                                  lambda = lambda,
                                  k = k,
                                  ARL0 = ARL0,
                                  bootstrap_pars = bootstrap_pars,
                                  discrete_grid_length = discrete_grid_length,
                                  score_function = score_function,
                                  fev = fev,
                                  n_skip = n_skip)
      mod_ii_jj <- list(mod_1 = mod_1_AMFEWMA_list[[ii]][[jj]],
                        lambda = lambda,
                        k = k)
      mod_2_AMFEWMA_big <- AMFEWMA_PhaseII(mfdobj_2 = fase2_big,
                                           mod_1 = mod_ii_jj,
                                           n_seq_2 = n_seq,
                                           l_seq_2 = l_seq)
      mod_2_AMFEWMA_small <- AMFEWMA_PhaseII(mfdobj_2 = fase2_small,
                                             mod_1 = mod_ii_jj,
                                             n_seq_2 = n_seq,
                                             l_seq_2 = l_seq)
      ARL_big <- mod_2_AMFEWMA_big$ARL_2
      ARL_small <- mod_2_AMFEWMA_small$ARL_2

      matrix_ARL_big[jj, ii] <- ARL_big
      matrix_ARL_small[jj, ii] <- ARL_small
    }
  }
  permat_small <- matrix_ARL_small
  mat_big <- matrix_ARL_big

  ## algorithm to select best lambda and k
  ARL_2_min <- min(matrix_ARL_big)
  for (ii in 1:(n_k * n_lambda)) {
    min_1 <- which(matrix_ARL_small == min(matrix_ARL_small), arr.ind = TRUE)
    ARL_2_p <- matrix_ARL_big[min_1[1, 1], min_1[1, 2]]

    if (ARL_2_p <= (1 + epsilon) * ARL_2_min) {
      lambda_opt <- lambda_grid[min_1[1, 2]]
      k_opt <- k_grid[min_1[1, 1]]
      break
    } else {
      matrix_ARL_small[min_1[1, 1], min_1[1, 2]] <- 10000
    }
  }

  # optimal parameters
  lambda <- lambda_opt
  k <- k_opt

  which_lambda_opt <- which(lambda_grid == lambda_opt)
  which_k_opt <- which(k_grid == k_opt)

  mod_1 <- mod_1_AMFEWMA_list[[which_lambda_opt]][[which_k_opt]]

  ret <- list(mod_1 = mod_1,
              lambda = lambda,
              k = k)

  return(ret)
}


AMFEWMA_PhaseI_given_pars <- function(mfdobj,
                                      mfdobj_tuning,
                                      lambda,
                                      k,
                                      ARL0 = 200,
                                      bootstrap_pars = list(n_seq = 200,
                                                            l_seq = 2000),
                                      discrete_grid_length = 25,
                                      score_function = "huber",
                                      fev = 0.9,
                                      n_skip = 100) {

  nobs <- dim(mfdobj$coefs)[2]
  nvar <- dim(mfdobj$coefs)[3]

  n_seq <- bootstrap_pars$n_seq
  l_seq <- bootstrap_pars$l_seq

  mean_mfdobj <- fda::mean.fd(mfdobj)
  sd_mfdobj <- fda::sd.fd(mfdobj)

  rn <- mfdobj$basis$rangeval
  grid_points <- seq(rn[1], rn[2], length.out = discrete_grid_length)
  sd_fd <- fda::eval.fd(grid_points, sd_mfdobj)
  sd_vec <- as.numeric(sd_fd)
  k_fun <- k * sd_vec
  basis <- mfdobj$basis

  # (TRAINING)
  mfdobj_cen <- funcharts::scale_mfd(mfdobj,
                                     center = mean_mfdobj,
                                     scale = FALSE)
  Xeval_cen <- fda::eval.fd(grid_points, mfdobj_cen)
  X <- matrix(aperm(Xeval_cen, c(2, 1, 3)), nrow = nobs)

  if (!(score_function %in% c("huber", "tukey"))) {
    stop("score_function must be \"huber\" or \"tukey\"")
  }

  if (score_function == "huber") {
    huber <- TRUE
  } else {
    huber <- FALSE
  }

  Y_array_tra <- statisticY_EWMA_cpp(X,
                                     lambda = lambda,
                                     k = k_fun,
                                     huber = huber,
                                     idx = 1:nrow(X))
  sigmaY <- Rfast::cova(Y_array_tra)

  eigen <- eigen(sigmaY, symmetric = TRUE)
  var_spiegata <- cumsum(eigen$values) / sum(eigen$values)
  n_eigenvalues <- which(var_spiegata > fev)[1]
  A <- eigen$values[1:n_eigenvalues]
  B <- eigen$vectors[, 1:n_eigenvalues, drop = FALSE]
  inv_sigmaY_reg <- B %*% diag(1 / A) %*% t(B)

  # (TUNING)
  mfdobj_tuning_cen <- funcharts::scale_mfd(mfdobj_tuning,
                                            center = mean_mfdobj,
                                            scale = FALSE)
  Xeval_tun_cen <- fda::eval.fd(grid_points, mfdobj_tuning_cen)

  nobs_tun <- dim(Xeval_tun_cen)[2]
  nvar <- dim(Xeval_tun_cen)[3]
  Xtun <- matrix(aperm(Xeval_tun_cen, c(2, 1, 3)), nrow = nobs_tun)

  par_fun_tun <- function(qq) {
    idx_tun <- sample(1:nobs_tun, l_seq, replace = TRUE)
    Y_array_tun <- statisticY_EWMA_cpp(
      Xtun,
      lambda = lambda,
      k = k_fun,
      huber = huber,
      idx = idx_tun
    )
    V2 <- colSums(t(Y_array_tun %*% B) ^ 2 / A)
    return(V2)
  }

  V2_mat <- do.call(rbind, lapply(1:n_seq, function(x) par_fun_tun(x)))

  ARL <- -100
  iter <- 0
  hmin <- 0
  hmax <- max(V2_mat)
  while (abs(ARL - ARL0) > 1e-2 & (hmax - hmin) > 1e-2 & iter < 50) {
    iter <- iter + 1
    h <- (hmin + hmax) / 2
    cond <- V2_mat[, -(1:n_skip)] > h
    if (max(V2_mat[, -(1:n_skip)]) < h) {
      ARL <- Inf
      hmax <- h
    } else {
      RL <- apply(cond, 1, function(x) which(x)[1])
      ARL <- mean(RL, na.rm = TRUE)
      if (ARL < ARL0) {
        hmin <- h
      } else {
        hmax <- h
      }
    }
  }
  ARL0 <- ARL

  ret <- list(mfdobj = mfdobj,
              mfdobj_tuning = mfdobj_tuning,
              inv_sigmaY_reg = inv_sigmaY_reg,
              mean_mfdobj = mean_mfdobj,
              h = h,
              ARL0 = ARL0,
              lambda = lambda,
              k = k_fun,
              grid_points = grid_points,
              V2_mat = V2_mat,
              n_skip = n_skip,
              huber = huber,
              vectors = B,
              values = A)
  return(ret)
}



#' Adaptive Multivariate Functional EWMA control chart - Phase II
#'
#' This function performs Phase II of the
#' Adaptive Multivariate Functional EWMA (AMFEWMA) control chart proposed
#' by Capezza et al. (2024)
#'
#' @param mfdobj_2
#' An object of class \code{mfd} containing the Phase II multivariate
#' functional data set, to be monitored with the AMFEWMA control chart.
#' @param mod_1
#' The output of the Phase I achieved through the
#' \code{\link{AMFEWMA_PhaseI}} function.
#' @param n_seq_2
#' If it is 1, the Phase II monitoring statistic is calculated on
#' the data sequence.
#' If it is an integer number larger than 1, a number \code{n_seq_2} of
#' bootstrap sequences are sampled with replacement from \code{mfdobj_2}
#' to allow uncertainty quantification on the estimation of the run length.
#' Default value is 1.
#' @param l_seq_2
#' If \code{n_seq_2} is larger than 1, this parameter sets the
#' length of each bootstrap sequence to be generated.
#' Default value is 2000 (which is ignored if the default value
#' @references
#' Capezza, C., Capizzi, G., Centofanti, F., Lepore, A., Palumbo, B. (2025)
#' An Adaptive Multivariate Functional EWMA Control Chart.
#' \emph{Journal of Quality Technology},  57(1):1--15,
#' doi:https://doi.org/10.1080/00224065.2024.2383674.
#'
#' @return
#' A list with the following elements.
#'
#' * \code{ARL_2}: the average run length estimated over the
#' bootstrap sequences. If \code{n_seq_2} is 1, it is simply the run length
#' observed over the Phase II sequence, i.e., the number of observations
#' up to the first alarm,
#'
#' * \code{RL}: the run length
#' observed over the Phase II sequence, i.e., the number of observations
#' up to the first alarm,
#'
#' * \code{V2}: a list with length \code{n_seq_2}, containing the
#' AMFEWMA monitoring statistic in Equation (8) of Capezza
#' et al. (2024), calculated in each bootstrap sequence, until the first alarm.
#'
#' * \code{cc}: a data frame with the information needed to plot the
#' AMFEWMA control chart in Phase II, with the following columns.
#' \code{id} contains the id of each multivariate functional observation,
#' \code{amfewma_monitoring_statistic} contains the AMFEWMA monitoring
#' statistic values calculated on the Phase II sequence,
#' \code{amfewma_monitoring_statistic_lim} is the upper control limit.
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' library(funcharts)
#' dat_I <- simulate_mfd(nobs = 200,
#'                       correlation_type_x = c("Bessel", "Bessel", "Bessel"),
#'                       sd_x = c(0.3, 0.3, 0.3))
#' dat_tun <- simulate_mfd(nobs = 200,
#'                         correlation_type_x = c("Bessel", "Bessel", "Bessel"),
#'                         sd_x = c(0.3, 0.3, 0.3))
#' dat_II <- simulate_mfd(nobs = 20,
#'                        correlation_type_x = c("Bessel", "Bessel", "Bessel"),
#'                        shift_type_x = c("C", "C", "C"),
#'                        d_x = c(2, 2, 2),
#'                        sd_x = c(0.3, 0.3, 0.3))
#' mfdobj_I <- get_mfd_list(dat_I$X_list, lambda = 1e-2)
#' mfdobj_tun <- get_mfd_list(dat_tun$X_list, lambda = 1e-2)
#' mfdobj_II <- get_mfd_list(dat_II$X_list, lambda = 1e-2)
#'
#' # p <- plot_mfd(mfdobj_I[1:100])
#' # lines_mfd(p, mfdobj_II, col = "red")
#'
#'
#' mod <- AMFEWMA_PhaseI(mfdobj = mfdobj_I,
#'                       mfdobj_tuning = mfdobj_tun,
#'                       lambda = 0.1,
#'                       k = c(1, 2))
#'
#' cc <- AMFEWMA_PhaseII(mfdobj_2 = rbind_mfd(mfdobj_I[1:100], mfdobj_II),
#'                       mod_1 = mod)
#' plot_control_charts(cc$cc, nobsI = 100)
#' }
#'
AMFEWMA_PhaseII <- function (mfdobj_2,
                             mod_1,
                             n_seq_2 = 1,
                             l_seq_2 = 2000) {

  mod_1 <- mod_1$mod_1

  nobs_2 <- dim(mfdobj_2$coefs)[2]
  nvar <- dim(mfdobj_2$coefs)[3]
  grid_points <- mod_1$grid_points
  mean_mfdobj <- mod_1$mean_mfdobj
  vectors <- mod_1$vectors
  values <- mod_1$values
  lambda <- mod_1$lambda
  k <- mod_1$k
  h <- mod_1$h
  huber <- mod_1$huber

  RL <- numeric(n_seq_2)

  mfdobj_2_cen <- funcharts::scale_mfd(mfdobj_2,
                                       center = mean_mfdobj,
                                       scale = FALSE)
  mfdobj_2_cen_eval <- fda::eval.fd(grid_points, mfdobj_2_cen)
  X2 <- matrix(aperm(mfdobj_2_cen_eval, c(2, 1, 3)), nrow = nobs_2)

  V2 <- list()


  for (jj in 1:n_seq_2) {
    if (n_seq_2 == 1) {
      idx2 <- 1:nobs_2
    } else {
      idx2 <- sample(1:nobs_2, l_seq_2, replace = TRUE)
    }
    output <- get_RL_cpp(
      X2 = X2,
      X_IC = matrix(),
      idx2 = idx2,
      idx_IC = numeric(),
      lambda = lambda,
      k = k,
      huber = huber,
      h = h,
      Values = values,
      Vectors = vectors
    )
    V2[[jj]] <- output$T2
    RL[jj] <- output$RL
  }

  ARL_2 <- mean(RL, na.rm = TRUE)
  if (mean(is.na(RL)) == 1) {
    ARL_2 <- 1e10
  }

  idx2 <- 1:nobs_2
  output <- get_RL_cpp(
    X2 = X2,
    X_IC = matrix(),
    idx2 = idx2,
    idx_IC = numeric(),
    lambda = lambda,
    k = k,
    huber = huber,
    h = 1e10,
    Values = values,
    Vectors = vectors
  )

  cc <- data.frame(
    id = mfdobj_2$fdnames[[2]],
    amfewma_monitoring_statistic = output$T2[, 1],
    amfewma_monitoring_statistic_lim = mod_1$h
  )

  return(list(
    ARL_2 = ARL_2,
    RL = RL,
    V2 = V2,
    cc = cc
  ))
}
