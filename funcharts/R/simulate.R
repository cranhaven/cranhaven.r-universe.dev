#' Simulate example data for funcharts
#'
#' Function used to simulate three data sets to illustrate the use
#' of \code{funcharts}.
#' It uses the function \code{\link[funcharts]{simulate_mfd}},
#' which creates a data set with three functional covariates,
#' a functional response generated as a function of the
#' three functional covariates,
#' and a scalar response generated as a function of the
#' three functional covariates.
#' This function generates three data sets, one for phase I,
#' one for tuning, i.e.,
#' to estimate the control chart limits, and one for phase II monitoring.
#' see also \code{\link[funcharts]{simulate_mfd}}.
#' @param nobs1
#' The number of observation to simulate in phase I. Default is 1000.
#' @param nobs_tun
#' The number of observation to simulate the tuning data set. Default is 1000.
#' @param nobs2
#' The number of observation to simulate in phase II. Default is 60.
#'
#' @return
#' A list with three objects, \code{datI} contains the phase I data,
#' \code{datI_tun} contains the tuning data,
#' \code{datII} contains the phase II data.
#' In the phase II data, the first group of observations are in control,
#' the second group of observations contains a moderate mean shift,
#' while the third group of observations contains a severe mean shift.
#' The shift types are described in the paper from Capezza et al. (2023).
#' @export
#'
#' @references
#'
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3):281--294. <doi:10.1080/00401706.2020.1753581>
#'
#' Capezza, C., Centofanti, F., Lepore, A., Menafoglio, A., Palumbo, B.,
#' & Vantini, S. (2023). funcharts: Control charts for multivariate
#' functional data in R. \emph{Journal of Quality Technology}, 55(5), 566--583.
#' <doi:10.1080/00224065.2023.2219012>
sim_funcharts <- function(nobs1 = 1000, nobs_tun = 1000, nobs2 = 60) {

  datI <- simulate_mfd(nobs = nobs1)
  datI_tun <- simulate_mfd(nobs = nobs_tun)
  datII_ic <- simulate_mfd(nobs = nobs2 / 3)
  datII_oc1 <- simulate_mfd(nobs = nobs2 / 3,
                            shift_type_x = c("0", "0", "A"),
                            d_x = c(0, 0, 20),
                            d_y_scalar = 1,
                            shift_type_y = "D",
                            d_y = 0.5)
  datII_oc2 <- simulate_mfd(nobs = nobs2 / 3,
                            shift_type_x = c("0", "0", "A"),
                            d_x = c(0, 0, 40),
                            d_y_scalar = 2,
                            shift_type_y = "D",
                            d_y = 1.5)
  datII <- list()
  datII$X1 <-
    rbind(datII_ic$X_list[[1]], datII_oc1$X_list[[1]], datII_oc2$X_list[[1]])
  datII$X2 <-
    rbind(datII_ic$X_list[[2]], datII_oc1$X_list[[2]], datII_oc2$X_list[[2]])
  datII$X3 <-
    rbind(datII_ic$X_list[[3]], datII_oc1$X_list[[3]], datII_oc2$X_list[[3]])
  datII$Y <- rbind(datII_ic$Y, datII_oc1$Y, datII_oc2$Y)
  datII$y_scalar <- c(datII_ic$y_scalar, datII_oc1$y_scalar, datII_oc2$y_scalar)


  datI <- list(X1 = datI$X_list[[1]],
               X2 = datI$X_list[[2]],
               X3 = datI$X_list[[3]],
               Y = datI$Y,
               y_scalar = datI$y_scalar)
  datI_tun <- list(X1 = datI_tun$X_list[[1]],
                   X2 = datI_tun$X_list[[2]],
                   X3 = datI_tun$X_list[[3]],
                   Y = datI_tun$Y,
                   y_scalar = datI_tun$y_scalar)


  return(list(datI = datI, datI_tun = datI_tun, datII = datII))
}




#' Simulate a data set for funcharts
#'
#' Function used to simulate a data set to illustrate
#' the use of \code{funcharts}.
#' By default, it creates a data set with three functional covariates,
#' a functional response generated as a function of the
#' three functional covariates
#' through a function-on-function linear model,
#' and a scalar response generated as a function of the
#' three functional covariates
#' through a scalar-on-function linear model.
#' This function covers the simulation study in Centofanti et al. (2021)
#' for the function-on-function case and also simulates data in a similar way
#' for the scalar response case.
#' It is possible to select the number of functional covariates,
#' the correlation function type for each functional covariate
#' and the functional response, moreover
#' it is possible to provide manually the mean and variance functions
#' for both functional covariates and the response.
#' In the default case, the function generates in-control data.
#' Additional arguments can be used to generate additional
#' data that are out of control,
#' with mean shifts according to the scenarios proposed
#' by Centofanti et al. (2021).
#' Each simulated observation of a functional variable consists of
#' a vector of discrete points equally spaced between 0 and 1 (by default
#' 150 points),
#' generated with noise.
#' @param nobs
#' The number of observation to simulate
#' @param p
#' The number of functional covariates to simulate. Default value is 3.
#' @param R2
#' The desired coefficient of determination in the regression
#' in both the scalar and functional response cases,
#' Default is 0.97.
#' @param shift_type_y
#' The shift type for the functional response.
#' There are five possibilities: "0" if there is no shift,
#' "A", "B", "C" or "D" for the corresponding shift types
#' shown in Centofanti et al. (2021).
#' Default is "0".
#' @param shift_type_x
#' A list of length \code{p}, indicating, for each functional covariate,
#' the shift type. For each element of the list,
#' there are five possibilities: "0" if there is no shift,
#' "A", "B", "C" or "D" for the corresponding shift types
#' shown in Centofanti et al. (2021).
#' By default, shift is not applied to any functional covariate.
#' @param correlation_type_y
#' A character vector indicating the type of correlation function for
#' the functional response.
#' See  Centofanti et al. (2021)
#' for more details. Three possible values are available,
#' namely \code{"Bessel"}, \code{"Gaussian"} and \code{"Exponential"}.
#' Default value is \code{"Bessel"}.
#' @param correlation_type_x
#' A list of \code{p} character vectors indicating
#' the type of correlation function for each
#' functional covariate.
#' See  Centofanti et al. (2021)
#' for more details. For each element of the list,
#' three possible values are available,
#' namely \code{"Bessel"}, \code{"Gaussian"} and \code{"Exponential"}.
#' Default value is \code{c("Bessel", "Gaussian", "Exponential")}.
#' @param d_y
#' A number indicating the severity of the shift type for
#' the functional response.
#' Default is 0.
#' @param d_y_scalar
#' A number indicating the severity of the shift type for
#' the scalar response.
#' Default is 0.
#' @param d_x
#' A list of \code{p} numbers, each indicating the
#' severity of the shift type for
#' the corresponding functional covariate.
#' By default, the severity is set to zero for all functional covariates.
#' @param n_comp_y
#' A positive integer number indicating how many principal components
#' obtained after the eigendecomposiiton of the covariance function
#' of the functional response variable to retain.
#' Default value is 10.
#' @param n_comp_x
#' A positive integer number indicating how many principal components
#' obtained after the eigendecomposiiton of the covariance function
#' of the multivariate functional covariates variable to retain.
#' Default value is 50.
#' @param P
#' A positive integer number indicating the number of equally spaced
#' grid points over which the covariance functions are discretized.
#' Default value is 500.
#' @param ngrid
#' A positive integer number indicating the number of equally spaced
#' grid points between zero and one
#' over which all functional observations are discretized before adding
#' noise. Default value is 150.
#' @param save_beta
#' If TRUE, the true regression coefficients of both the function-on-function
#' and the scalar-on-function models are saved.
#' Default is FALSE.
#' @param mean_y
#' The mean function of the functional response can be set manually
#' through this argument. If not NULL, it must be a vector of length
#' equal to \code{ngrid}, providing the values of the mean function of
#' the functional response discretized on \code{seq(0,1,l=ngrid)}.
#' If NULL, the mean function is generated as done in the simulation study
#' of Centofanti et al. (2021).
#' Default is NULL.
#' @param mean_x
#' The mean function of the functional covariates can be set manually
#' through this argument. If not NULL, it must be a list of vectors,
#' each with length equal to \code{ngrid},
#' providing the values of the mean function of
#' each functional covariate discretized on \code{seq(0,1,l=ngrid)}.
#' If NULL, the mean function is generated as done in the simulation study
#' of Centofanti et al. (2021).
#' Default is NULL.
#' @param variance_y
#' The variance function of the functional response can be set manually
#' through this argument. If not NULL, it must be a vector of length
#' equal to \code{ngrid}, providing the values of the variance function of
#' the functional response discretized on \code{seq(0,1,l=ngrid)}.
#' If NULL, the variance function is generated as done in the simulation study
#' of Centofanti et al. (2021).
#' Default is NULL.
#' @param variance_x
#' The variance function of the functional covariates can be set manually
#' through this argument. If not NULL, it must be a list of vectors,
#' each with length equal to \code{ngrid},
#' providing the values of the variance function of
#' each functional covariate discretized on \code{seq(0,1,l=ngrid)}.
#' If NULL, the variance function is generated as done in the simulation study
#' of Centofanti et al. (2021).
#' Default is NULL.
#' @param sd_y
#' A positive number indicating the standard deviation of the generated
#' noise with which the functional response discretized values are observed.
#' Default value is 0.3
#' @param sd_x
#' A vector of \code{p} positive numbers indicating the standard deviation
#' of the generated noise with which the
#' functional covariates discretized values are observed.
#' Default value is \code{c(0.3, 0.05, 0.3)}.
#' @param seed Deprecated: use \code{set.seed()} before calling
#' the function for reproducibility.
#'
#' @return
#' A list with the following elements:
#'
#' * \code{X_list} is a list of \code{p} matrices, each with dimension
#' \code{nobs}x\code{ngrid}, containing the simulated
#' observations of the multivariate functional covariate
#'
#' * \code{Y} is a \code{nobs}x\code{ngrid} matrix with the simulated
#' observations of the functional response
#'
#' * \code{y_scalar} is a vector of length \code{nobs} with the simulated
#' observations of the scalar response
#'
#' * \code{beta_fof}, if \code{save_beta = TRUE}, is a
#' list of \code{p} matrices, each with dimension \code{P}x\code{P}
#' with the discretized functional coefficients of the
#' function-on-function regression
#'
#' * \code{beta_sof}, if \code{save_beta = TRUE}, is a
#' list of \code{p} vectors, each with length \code{P},
#' with the discretized functional coefficients of the
#' scalar-on-function regression
#'
#'
#' @export
#'
#' @references
#'
#' Centofanti F, Lepore A, Menafoglio A, Palumbo B, Vantini S. (2021)
#' Functional Regression Control Chart.
#' \emph{Technometrics}, 63(3):281--294. <doi:10.1080/00401706.2020.1753581>
simulate_mfd <- function(nobs = 1000,
                         p = 3,
                         R2 = 0.97,
                         shift_type_y = "0",
                         shift_type_x = c("0", "0", "0"),
                         correlation_type_y = "Bessel",
                         correlation_type_x = c("Bessel",
                                                "Gaussian",
                                                "Exponential"),
                         d_y = 0,
                         d_y_scalar = 0,
                         d_x = c(0, 0, 0),
                         n_comp_y = 10,
                         n_comp_x = 50,
                         P = 500,
                         ngrid = 150,
                         save_beta = FALSE,
                         mean_y = NULL,
                         mean_x = NULL,
                         variance_y = NULL,
                         variance_x = NULL,
                         sd_y = 0.3,
                         sd_x = c(0.3, 0.05, 0.3),
                         seed) {

  if (!missing(seed)) {
    warning(paste0(
      "argument seed is deprecated; ",
      "please use set.seed() before calling simulate_mfd() instead."),
      call. = FALSE)
  }

  if (!(R2 > 0 & R2 < 1)) {
    stop("R2 must be between 0 and 1.")
  }

  if (!(toupper(shift_type_y) %in% c("A", "B", "C", "D", "0"))) {
    stop("only shift types A, B, C, D and 0 are allowed.")
  }
  if ((toupper(shift_type_y) %in% c("A", "B", "C", "D")) & d_y == 0) {
    stop("If there is a shift, the corresponding
         severity d must be greater than zero.")
  }
  if (toupper(shift_type_y) == "0" & d_y != 0) {
    stop("If there is no shift, the corresponding
         severity d must be zero.")
  }

  for (jj in seq_len(p)) {
    if (!(toupper(shift_type_x[jj]) %in% c("A", "B", "C", "D", "0"))) {
      stop("only shift types A, B, C, D and 0 are allowed.")
    }
    if (toupper(shift_type_x[jj]) %in% c("A", "B", "C", "D") & d_x[jj] == 0) {
      stop("If there is a shift, the corresponding
         severity d must be greater than zero.")
    }
    if (toupper(shift_type_x[jj]) == "0" & d_x[jj] != 0) {
      stop("If there is no shift, the corresponding severity d must be zero.")
    }
  }

  ey <- generate_cov_str(p = 1,
                         P = P,
                         correlation_type = correlation_type_y,
                         n_comp = n_comp_y)
  ey$values <- pmax(ey$values, 0)
  e <- generate_cov_str(p = p,
                        P = P,
                        correlation_type = correlation_type_x,
                        n_comp = n_comp_x)
  e$values <- pmax(e$values, 0)
  x_seq <- seq(0, 1, l = P)

  b_perfect <- sqrt(ey$values / e$values[seq_along(ey$values)])
  b_perfect_sof <- rep(1 / sqrt(sum(e$values[1:10])), 10)

  vary <- rowSums(t(t(ey$vectors^2) * ey$values))
  w <- 1 / P
  optim <- stats::uniroot(function(a) {
    b <- pmax(b_perfect - a, 0)
    var_yexp <- rowSums(t(t(ey$vectors^2) * (b^2 * e$values[1:10])))
    R2_check <- sum(var_yexp / vary) * w
    R2_check - R2
  }, lower = 0, upper = 1)
  a <- optim$root
  b <- pmax(b_perfect - a, 0)
  var_yexp <- rowSums(t(t(ey$vectors^2) * (b^2 * e$values[1:10])))
  vary <- rowSums(t(t(ey$vectors^2) * ey$values))
  R2_check <- sum(var_yexp / vary) * w
  dim_additional <- n_comp_x - length(b)
  B <- rbind(diag(b), matrix(0, nrow = dim_additional, ncol = 10))
  eigeps <- diag(diag(ey$values) - t(B) %*% diag(e$values) %*% B)

  csi_X <- stats::rnorm(n = length(e$values) * nobs,
                        mean = 0,
                        sd = sqrt(rep(e$values, each = nobs)))
  csi_X <- matrix(csi_X, nrow = nobs)
  csi_eps <- stats::rnorm(n = length(eigeps) * nobs, mean = 0,
                          sd = sqrt(rep(eigeps, each = nobs)))
  csi_eps <- matrix(csi_eps, nrow = nobs)
  csi_Y <- csi_X %*% B + csi_eps

  fun <- function(x, par, r, m, s) {
    a <- par[1]
    b <- par[2]
    c <- par[3]
    r <- par[4]
    dnorm_vec <- Vectorize(stats::dnorm, "x")
    function(x) {
      norm_dens <- dnorm_vec(x, mean = m, sd = s)
      sum_term <- if (identical(s, 0)) 0 else colSums(norm_dens)
      a * x^2 + b * x + c + r * sum_term
    }
  }

  grid <- seq(0, 1, length.out = ngrid)

  if (!is.null(mean_x)) {
    if (!is.list(mean_x)) {
      stop("mean_x must be a list of length p if not NULL")
    }
    if (length(mean_x) != p) {
      stop("mean_x must be a list of length p if not NULL")
    }
    if (!all(vapply(mean_x, length, 0) == ngrid)) {
      stop("mean_x must be a list of length p if not NULL,
           each element of the list must be a vector of length ngrid")
    }
  } else {
    par_m_x_list <- list()
    mm_x_list <- list()
    ms_x_list <- list()
    mean_x <- list()
    for (jj in seq_len(p)) {
      if (correlation_type_x[jj] == "Bessel") {
        par_m_x_list[[jj]] <- c(-20, 20, 20, 0.05)
        mm_x_list[[jj]] <- c(0.075, 0.100, 0.250, 0.350, 0.500,
                             0.650, 0.850, 0.900, 0.950)
        ms_x_list[[jj]] <- c(0.050, 0.030, 0.050, 0.050, 0.100,
                             0.050, 0.100, 0.040, 0.035)
      }
      if (correlation_type_x[jj] == "Gaussian") {
        par_m_x_list[[jj]] <- c(0, 4, 0, 0)
        mm_x_list[[jj]] <- 0
        ms_x_list[[jj]] <- 0
      }
      if (correlation_type_x[jj] == "Exponential") {
        par_m_x_list[[jj]] <- c(-10, 14,-8, 0.05)
        mm_x_list[[jj]] <- c(0.075, 0.100, 0.150, 0.225, 0.400, 0.525, 0.550,
                             0.600, 0.625, 0.650, 0.850, 0.900, 0.925)
        ms_x_list[[jj]] <- c(0.050, 0.060, 0.050, 0.040, 0.050, 0.035, 0.045,
                             0.045, 0.040, 0.030, 0.015, 0.010, 0.015)
      }

      mean_x[[jj]] <- fun(par = par_m_x_list[[jj]],
                          m = mm_x_list[[jj]],
                          s = ms_x_list[[jj]])(grid)

      if (shift_type_x[jj] == "A") mean_x[[jj]] <-
        mean_x[[jj]] + d_x[jj] * grid^2
      if (shift_type_x[jj] == "B") mean_x[[jj]] <-
        mean_x[[jj]] + d_x[jj] * grid
      if (shift_type_x[jj] == "C") mean_x[[jj]] <-
        mean_x[[jj]] + d_x[jj]
      if (shift_type_x[jj] == "D") mean_x[[jj]] <-
        mean_x[[jj]] + d_x[jj] * grid^2 + d_x[jj] * grid
    }

  }


  if (!is.null(variance_x)) {
    if (!is.list(variance_x)) {
      stop("variance_x must be a list of length p if not NULL")
    }
    if (length(variance_x) != p) {
      stop("variance_x must be a list of length p if not NULL")
    }
    if (!all(vapply(variance_x, length, 0) == ngrid)) {
      stop("variance_x must be a list of length p if not NULL,
           each element of the list must be a vector of length ngrid")
    }
    if (sum(simplify2array(variance_x) <= 0) > 0) {
      stop("variance_x must contain only positive numbers if not NULL")
    }
  } else {

    par_v_x_list <- list()
    vm_x_list <- list()
    vs_x_list <- list()
    variance_x <- list()
    for (jj in seq_len(p)) {
      if (correlation_type_x[jj] == "Bessel") {
        par_v_x_list[[jj]] <- c(0, 0, 1, 0.1)
        vm_x_list[[jj]] <- c(0.075, 0.100, 0.125, 0.150, 0.400,
                             0.650, 0.850, 0.900, 0.925)
        vs_x_list[[jj]] <- c(0.050, 0.060, 0.075, 0.075, 0.075,
                             0.075, 0.045, 0.045, 0.040)
      }
      if (correlation_type_x[jj] == "Gaussian") {
        par_v_x_list[[jj]] <- c(0, 0.02, 1, 0)
        vm_x_list[[jj]] <- 0
        vs_x_list[[jj]] <- 0
      }
      if (correlation_type_x[jj] == "Exponential") {
        par_v_x_list[[jj]] <- c(-40, 150, 30, 2)
        vm_x_list[[jj]] <- c(0.075, 0.100, 0.150, 0.225, 0.400, 0.525, 0.550,
                             0.600, 0.625, 0.650, 0.850, 0.900, 0.925)
        vs_x_list[[jj]] <- c(0.050, 0.060, 0.050, 0.040, 0.050, 0.035, 0.045,
                             0.045, 0.040, 0.030, 0.015, 0.010, 0.015)
      }

      variance_x[[jj]] <- fun(par = par_v_x_list[[jj]],
                              m = vm_x_list[[jj]],
                              s = vs_x_list[[jj]])(grid)
    }
  }


  if (!is.null(mean_y)) {
    if (!is.numeric(mean_y)) {
      stop("mean_y must be a vector of length ngrid")
    }
    if (length(mean_y) != ngrid) {
      stop("mean_y must be a vector of length ngrid")
    }
  } else {

    par_m_y <- c(0, 30, 0, 0)
    mm_y <- 0
    ms_y <- 0

    mean_y <- fun(par = par_m_y, m = mm_y, s = ms_y)(grid)

    if (shift_type_y == "A") mean_y <- mean_y + d_y * grid^2
    if (shift_type_y == "B") mean_y <- mean_y + d_y * grid
    if (shift_type_y == "C") mean_y <- mean_y + d_y
    if (shift_type_y == "D") mean_y <- mean_y + d_y * grid^2 + d_y * grid

  }



  if (!is.null(variance_y)) {
    if (!is.numeric(variance_y)) {
      stop("variance_y must be a vector of length ngrid")
    }
    if (length(variance_y) != ngrid) {
      stop("variance_y must be a vector of length ngrid")
    }
    if (sum(variance_y <= 0) > 0) {
      stop("variance_y must be a vector with only positive values")
    }
  } else {

    par_v_y <- c(0, 8, 1, 0)
    vm_y <- 0
    vs_y <- 0

    variance_y <- fun(par = par_v_y,
                      m = vm_y,
                      s = vs_y)(grid)

  }


  X_list <- list()
  for (jj in seq_len(p)) {
    meig_jj <- e$vectors[1:P + (jj-1)*P, ]
    meig_jj_interpolate <- apply(t(meig_jj),
                                 1,
                                 function(y) stats::approx(x_seq, y, grid)$y)
    X_jj_scaled <- csi_X %*% t(meig_jj_interpolate)
    X_jj <- t(mean_x[[jj]] + t(X_jj_scaled) * sqrt(variance_x[[jj]]))
    X_list[[jj]] <- X_jj + stats::rnorm(length(X_jj), sd = sd_x[jj])
  }

  ndigits <- floor(log10(p)) + 1
  names(X_list) <- paste0("X",
                          sprintf(seq_len(p),
                                  fmt = paste0("%0", ndigits, "d")))

  eig_y_interpolate <- apply(t(ey$vectors),
                             1,
                             function(y) stats::approx(x_seq, y, grid)$y)
  Y_scaled <- csi_Y %*% t(eig_y_interpolate)
  Y <- t(mean_y + t(Y_scaled) * sqrt(variance_y))
  Y <- Y + stats::rnorm(length(Y), sd = sd_y)

  vary_scalar <- 1
  b_sof <- rep(sqrt(R2 * vary_scalar / sum(e$values[1:10])), 10)
  R2_sof_check <- sum(b_sof^2 * e$values[1:10])

  var_eps_scalar <- vary_scalar - sum(b_sof^2 * e$values[1:10])
  eps_scalar <- stats::rnorm(n = nobs, mean = 0, sd = sqrt(var_eps_scalar))
  y_scalar <- as.numeric(d_y_scalar + csi_X[, 1:10] %*% b_sof + eps_scalar)

  beta_fof <- beta_sof <- NULL
  if (save_beta) {
    beta_fof <- 0
    for (kk in seq_along(diag(B))) {
      beta_fof <- beta_fof + diag(B)[kk] *
        outer(e$vectors[, kk], ey$vectors[, kk])
    }
    beta_fof_list <- list()
    for (jj in seq_len(p)) {
      beta_fof_list[[jj]] <- beta_fof[1:P + (jj-1)*P, ]
    }

    beta_sof <- as.numeric(e$vectors[, 1:10] %*% b_sof)
    beta_sof_list <- list()
    for (jj in seq_len(p)) {
      beta_sof_list[[jj]] <- beta_sof[1:P + (jj-1)*P]
    }
  }

  out <- list(X_list = X_list,
              Y = Y,
              y_scalar = y_scalar)
  if (save_beta) {
    out$beta_fof <- beta_fof_list
    out$beta_sof <- beta_sof_list
  }
  out

}

#' @noRd
#'
generate_cov_str <- function(p = 3,
                             P = 500,
                             correlation_type = c("Bessel",
                                                  "Gaussian",
                                                  "Exponential"),
                             n_comp = 50) {

  if (length(correlation_type) != p) {
    stop("correlation_type length must be equal to p.")
  }
  if (!all(correlation_type %in% c("Bessel", "Gaussian", "Exponential"))) {
    stop("correlation_type admits only values
         \"Bessel\", \"Gaussian\" and \"Exponential\"")
  }

  x_seq <- seq(0, 1, l = P)

  get_mat <- function(cov) {
    P <- length(cov)
    covmat <- matrix(0, nrow = P, ncol = P)
    for (ii in seq_len(P)) {
      covmat[ii, ii:P] <- cov[seq_len(P - ii + 1)]
      covmat[P - ii + 1, seq_len(P - ii + 1)] <- rev(cov[seq_len(P - ii + 1)])
    }
    covmat
  }

  cov_fun <- function(x, cor_type) {
    if (cor_type == "Bessel") {
      ret <- besselJ(x * 4, 0)
    }
    if (cor_type == "Gaussian") {
      ret <- exp(-x^2)
    }
    if (cor_type == "Exponential") {
      ret <- exp(-sqrt(x))
    }
    ret
  }

  w <- 1 / P

  cov_list <- list()
  cov_mat_list <- list()
  eig_list <- list()
  eigenvalues_mat <- list()
  for (jj in seq_len(p)) {
    cov_list[[jj]] <- cov_fun(x_seq, correlation_type[jj])
    cov_mat_list[[jj]] <- get_mat(cov_list[[jj]])
    eig_list[[jj]] <- RSpectra::eigs_sym(cov_mat_list[[jj]], n_comp + 10)
    eig_list[[jj]]$vectors <- eig_list[[jj]]$vectors / sqrt(w)
    eig_list[[jj]]$values <- eig_list[[jj]]$values * w
    eigenvalues_mat[[jj]] <- eig_list[[jj]]$values
  }

  if (p == 1) {
    e <- eig_list[[1]]
    e$values <- e$values[seq_len(n_comp)]
    e$vectors <- e$vectors[, seq_len(n_comp)]
    return(e)
  } else {
    eigenvalues <- rowMeans(do.call(cbind, eigenvalues_mat))

    corr_mat <- vector("list", p)
    for (ii in seq_len(p)) {
      corr_mat[[ii]] <- vector("list", p)
    }

    for (ii in seq_len(p)) {
      for (jj in ii:p) {
        if (jj == ii) {
          corr_mat[[ii]][[jj]] <- cov_mat_list[[ii]]
        } else {
          V1 <- eig_list[[ii]]$vectors[, seq_len(n_comp)]
          V2 <- eig_list[[jj]]$vectors[, seq_len(n_comp)]
          lambdas <- eigenvalues[seq_len(n_comp)]
          sum <- V1 %*% (t(V2) * lambdas) / p
          corr_mat[[ii]][[jj]] <- sum
          corr_mat[[jj]][[ii]] <- t(sum)
        }
      }
      corr_mat[[ii]] <- do.call("cbind", corr_mat[[ii]])
    }
    corr_mat <- do.call("rbind", corr_mat)
    e <- RSpectra::eigs_sym(corr_mat, n_comp + 10)

    e$vectors <- e$vectors / sqrt(w)
    e$values <- e$values * w
    e$values <- e$values[seq_len(n_comp)]
    e$vectors <- e$vectors[, seq_len(n_comp)]
    return(e)
  }
}




# Generate data for the robust multivariate functional control cha --------
#' @noRd
generate_cov_str_RoMFCC <- function(p = 3,
                                    P = 100,
                                    correlation = "decreasing",
                                    k = 1) {
  n_comp_x <- 100
  x_seq <- seq(0, 1, l = P)

  get_mat <- function(cov) {
    P <- length(cov)
    covmat <- matrix(0, nrow = P, ncol = P)
    for (ii in 1:P) {
      covmat[ii, ii:P] <- cov[1:(P - ii + 1)]
      covmat[P - ii + 1, 1:(P - ii + 1)] <- rev(cov[1:(P - ii + 1)])
    }
    covmat
  }
  eig <- covmatList <- list()
  cov1 <- besselJ(x_seq * 32, 0)
  covmat1 <- get_mat(cov1)
  eig1 <- eigen(covmat1)
  w <- 1 / P
  eig1$vectors <- eig1$vectors / sqrt(w)
  eig1$values <- eig1$values * w
  for (ii in 1:p) {
    covmatList[[ii]] <- covmat1
    eig[[ii]] <- eig1
  }
  eigList <- eig

  eigenvalues <- rowMeans(sapply(1:p, function(ii)
    eig[[ii]]$values))

  corr_mat <- vector("list", p)
  for (ii in 1:p) {
    corr_mat[[ii]] <- vector("list", p)
  }

  for (ii in 1:p) {
    for (jj in ii:p) {
      if (jj == ii) {
        corr_mat[[ii]][[jj]] <- covmatList[[ii]]
      } else {
        V <- eigList[[ii]]$vectors[, 1:n_comp_x]
        lambdas <- eigenvalues[1:n_comp_x]

        if (correlation == "decreasing") {
          sum <- V %*% (t(V) * lambdas) / (1 + abs(ii - jj) / k)
        }
        if (correlation == "equal") {
          sum <- V %*% (t(V) * lambdas) / (1 + 1 / k)
        }
        if (!(correlation %in% c("decreasing", "equal"))) {
          stop("correlation must be either decreasing or equal")
        }

        corr_mat[[ii]][[jj]] <- sum
        corr_mat[[jj]][[ii]] <- t(sum)
      }
    }
    corr_mat[[ii]] <- do.call("cbind", corr_mat[[ii]])
  }
  corr_mat <- do.call("rbind", corr_mat)
  e <- eigen(corr_mat)

  list(e = e, P = P)
}

#' Simulate multivariate functional data with casewise and componentwise contamination
#'
#' Generate multivariate functional data under different contamination models
#' (casewise, componentwise, or both) for use in simulation studies of the
#' Robust Multivariate Functional Control Chart (RoMFCC) as described in
#' Capezza, Centofanti, Lepore, and Palumbo (2024).
#'
#' The generated data mimic dynamic resistance curves (DRCs) in resistance
#' spot welding processes and allow for controlled introduction of
#' casewise and/or componentwise outliers, as in the Monte Carlo study
#' presented in the RoMFCC paper.
#'
#' @param nobs Integer. Number of observations to simulate (default 1000).
#' @param p Integer. Number of functional components (variables) (default 3).
#' @param p_cellwise Numeric in \eqn{[0,1]}. Probability of cellwise contamination
#'   (componentwise outliers) for each component (default 0).
#' @param p_casewise Numeric in \eqn{[0,1]}. Probability of casewise contamination
#'   (entire observation contaminated) (default 0).
#' @param sd_e Numeric. Standard deviation of the additive measurement noise (default 0.005).
#' @param sd Numeric. Standard deviation scaling of the functional part (default 0.002).
#' @param T_exp Numeric in \eqn{(0,1)}. Expansion parameter for phase outliers (default 0.6).
#' @param outlier Character. Type of contamination in Phase I sample:
#'   \code{"no"} (no contamination),
#'   \code{"outlier_M"} (mean shift),
#'   \code{"outlier_E"} (exponential shift),
#'   \code{"outlier_P"} (phase shift). Default \code{"no"}.
#' @param M_outlier_case Numeric. Magnitude of casewise outlier contamination (default 0).
#' @param M_outlier_cell Numeric. Magnitude of cellwise outlier contamination (default 0).
#' @param OC Character. Out-of-control model in Phase II data:
#'   \code{"no"} (in control),
#'   \code{"OC_M"} (mean shift),
#'   \code{"OC_E"} (exponential shift),
#'   \code{"OC_P"} (phase shift). Default \code{"no"}.
#' @param M_OC Numeric. Magnitude of out-of-control shift (default 0).
#' @param P Integer. Number of grid points in each functional profile (default 100).
#' @param max_n_cellwise Integer. Maximum number of components per observation
#'   allowed to be cellwise contaminated (default \code{Inf}).
#' @param correlation Character. Correlation structure among components:
#'   typically \code{"decreasing"} (default).
#' @param k Integer. Correlation parameter (default 1).
#' @param which_OC Integer vector. Indices of components subject to out-of-control
#'   shifts in Phase II (default 5).
#'
#' @details
#' The function generates \code{nobs} realizations of a \code{p}-variate functional
#' quality characteristic observed on an equally spaced grid of size \code{P}.
#' The underlying process is simulated through a Karhunen–Loève expansion
#' with eigenfunctions and eigenvalues derived from a specified correlation
#' structure. Outliers can be introduced at cellwise level (single components),
#' casewise level (entire observation), or both, using probability parameters
#' \code{p_cellwise} and \code{p_casewise} and magnitudes
#' \code{M_outlier_cell}, \code{M_outlier_case}.
#' Out-of-control shifts in Phase II can be introduced via the \code{OC} argument.
#'
#' This setup mirrors the simulation design in Section 4 of
#' Capezza et al. (2024) where RoMFCC was benchmarked against competing
#' control charts under various contamination scenarios.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{X_mat_list}{A list of length \code{p}, each element a matrix of
#'     dimension \code{nobs × P} with the simulated functional observations.}
#'   \item{ind_out}{A list of length \code{p}, each element containing the
#'     indices of observations contaminated in that component.}
#' }
#'
#' Capezza, C., Centofanti, F., Lepore, A., Palumbo, B. (2024)
#' Robust Multivariate Functional Control Chart.
#' \emph{Technometrics}, 66(4):531--547, <doi:10.1080/00401706.2024.2327346>.
#'
#' @examples
#' \donttest{
#' # Simulate uncontaminated data (Phase I)
#' sim <- simulate_data_RoMFCC(nobs = 200, p = 3, outlier = "no", OC = "no")
#' str(sim$X_mat_list)
#'
#' # Simulate with componentwise outliers in Phase I
#' sim2 <- simulate_data_RoMFCC(nobs = 200, p = 3,
#'                              p_cellwise = 0.05, M_outlier_cell = 0.03,
#'                              outlier = "outlier_E")
#'
#' # Simulate Phase II with a mean shift in one component
#' sim3 <- simulate_data_RoMFCC(nobs = 200, p = 3,
#'                              OC = "OC_M", M_OC = 0.04, which_OC = 2)
#' }
#'
#' @export
simulate_data_RoMFCC <- function(nobs = 1000,
                                 p = 3,
                                 p_cellwise = 0,
                                 p_casewise = 0,
                                 sd_e = 0.005,
                                 sd = 0.002,
                                 T_exp = 0.6,
                                 outlier = "no",
                                 M_outlier_case = 0,
                                 M_outlier_cell = 0,
                                 OC = "no",
                                 M_OC = 0,
                                 P = 100,
                                 max_n_cellwise = Inf,
                                 correlation = "decreasing",
                                 k = 1,
                                 which_OC = 5) {
  cov_str <- generate_cov_str_RoMFCC(p, P, correlation = correlation, k = k)
  e <- cov_str$e
  P <- cov_str$P
  x_seq <- seq(0, 1, l = P)

  w <- 1 / P
  n_comp_x <- max(which((cumsum(e$values) / sum(e$values)) < 1))

  meig <- list()
  for (ii in 1:p) {
    index <- 1:P + (ii - 1) * P
    meig[[ii]] <- e$vectors[index, 1:n_comp_x] / sqrt(w)
  }
  meigenvalues <- e$values[1:n_comp_x] * w
  meigenvalues[meigenvalues < 0] <- 0

  csi_X <- stats::rnorm(
    n = length(meigenvalues) * nobs,
    mean = 0,
    sd = sqrt(rep(meigenvalues, each = nobs))
  )
  csi_X <- matrix(csi_X, nrow = nobs)


  fun_phase <- function(x_ini, M_g) {
    x <- numeric(P)
    T1 <- 0.0
    T2 <- 0.6
    a <- (T2 - M_g / 1.5 - T1) / (T2 - T1)
    b <- T1 - a * T1
    xx <- x_ini * a + b
    a <- (T2 - M_g / 1.5 - 1) / (T2 - 1)
    b <- 1 - a
    xx2 <- x_ini * a + b
    x[1:(T1 * P)] <- x_ini[1:(T1 * P)]
    x[((T1 * P) + 1):(T2 * P)] <- xx[((T1 * P) + 1):(T2 * P)]
    x[(T2 * P + 1):(P)] <- xx2[(T2 * P + 1):(P)]
    x <-
      (x - min(x)) / (max(x) - min(x)) * (0.15 - 0.0045) + 0.0045

    x3 <- x_ini ^ (1 + M_g / 0.6)
    x3 <-
      (x3 - min(x3)) / (max(x3) - min(x3)) * (0.15 - 0.0045) + 0.0045
    x <- x3

    - (M_g / 5) * x_ini + 0.45 * (M_g / 5) + 0.2074 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(-0.8217)) * x)) - 423.3 * (1 + tanh(-26.15 * (x - (-0.1715))))
  }
  fun_m <- function(x_ini, M_g) {
    x <- x_ini
    x <- (x - min(x)) / (max(x) - min(x)) * (0.15 - 0.0045) + 0.0045
    0.2074 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(-0.8217)) * x)) - 423.3 * (1 + tanh(-26.15 * (x - (-0.1715))))
  }
  if (OC == "OC_P") {
    f1_m <- fun_m
  } else {
    f1_m <- fun_m
  }

  if (outlier == "no") {
    cont_function <- function(nobs, p_g, M_g, T_exp)
      0
  } else if (outlier == "outlier_M") {
    cont_function <- function(nobs, p_g, M_g, T_exp) {
      rb <-
        stats::rbinom(nobs, 1, p_g) * sample(c(-1, 1), nobs, replace = T)
      M_g * matrix(rb , P, nobs, byrow = T)
    }
  } else if (outlier == "outlier_E") {
    cont_function <- function(nobs, p_g, M_g, T_exp) {
      rb <- stats::rbinom(nobs, 1, p_g)
      grid <- 1:P
      grid_new <- (grid - min(grid)) / (max(grid) - min(grid))
      a <- -M_g / (1 - T_exp)
      y <- a * (grid_new - T_exp)
      y[y > 0] <- 0

      grid <- 1:P
      grid_new <- (grid - min(grid)) / (max(grid) - min(grid))
      a <- -M_g * 0.5 / (1 - T_exp)
      y2 <- a * (grid_new - T_exp)
      y[y2 > 0] <- y2[y2 > 0]
      if (nobs == 1) {
        out_values <- (y * matrix(rb, P, nobs, byrow = T))
      } else {
        out_values <- (y * matrix(rb, P, nobs, byrow = T))
      }

      return(out_values)
    }
  }
  else if (outlier == "outlier_P") {
    cont_function <- function(nobs, p_g, M_g, T_exp) {
      rb <- stats::rbinom(nobs, 1, p_g)
      (-fun_m(x_seq, 0) + fun_phase(x_seq, M_g)) * matrix(rb, P, nobs, byrow = T)
    }
  }
  if (OC == "no") {
    out_function <- function(nobs, M_g, T_exp)
      rep(0, P)
  }
  else if (OC == "OC_M") {
    out_function <- function(nobs, M_g, T_exp) {
      rep(M_g, P)
    }
  }
  else if (OC == "OC_E") {
    out_function <- function(nobs, M_g, T_exp) {
      out_values <- rep(0 , P)
      grid <- 1:P
      grid_new <- (grid - min(grid)) / (max(grid) - min(grid))
      a <- -M_g / (1 - T_exp)
      y <- a * (grid_new - T_exp)
      y[y > 0] <- 0


      out_values <- rep(0 , P)
      grid <- 1:P
      grid_new <- (grid - min(grid)) / (max(grid) - min(grid))
      a <- -M_g * 0.5 / (1 - T_exp)
      y2 <- a * (grid_new - T_exp)
      y[y2 > 0] <- y2[y2 > 0]

      out_values <- (y)

      return(out_values)
    }
  }
  else if (OC == "OC_P") {
    out_function <- function(nobs, M_g, T_exp) {
      fun_phase(x_seq, M_g) - f1_m(x_seq, M_g)
    }
  }
  cont_cell_mat <- matrix(0, nobs, P * p)
  for (ii in 1:p) {
    index <- 1:P + (ii - 1) * P
    cont_cell_mat[, index] <-
      t(cont_function(nobs, p_cellwise, M_outlier_cell, T_exp))
  }
  if (max_n_cellwise < p) {
    cont_cell_mat_which <-
      sapply(1:10, function(jj)
        round(rowSums(abs(cont_cell_mat[, P * (jj - 1) + (1:P)])) , 5) > 0)
    rows_greater_than_allowed <-
      rowSums(cont_cell_mat_which) > max_n_cellwise

    for (ii in 1:nobs) {
      if (rows_greater_than_allowed[ii]) {
        idx <- which(cont_cell_mat_which[ii,])
        which_to_retain <- sample(idx, max_n_cellwise)
        which_to_exclude <- setdiff(idx, which_to_retain)
        for (jj in which_to_exclude) {
          cont_cell_mat[ii, P * (jj - 1) + (1:P)] <- 0
        }
      }
    }
  }

  cont_case_mat <- matrix(0, nobs, P * p)
  ind_out_case <- stats::rbinom(nobs, 1, p_casewise)
  for (ii in 1:nobs) {
    if (ind_out_case[ii] == 1) {
      cont_case_mat[ii,] <-
        as.numeric(sapply(1:p, function(iii)
          t(
            cont_function(1, 1, M_outlier_case, T_exp)
          )))
    }
  }
  cont_mat <- cont_cell_mat + cont_case_mat
  ind_out <-
    lapply(1:p, function(ii)
      which((abs(
        rowSums(cont_mat[, 1:P + (ii - 1) * P])
      ) > 0)))
  cont_list <- list()
  for (ii in 1:p) {
    index <- 1:P + (ii - 1) * P
    cont_list[[ii]] <- cont_mat[, index]
  }
  oc_case_mat <- matrix(0, nobs, P * p)
  for (ii in 1:nobs) {
    oc_case_mat[ii, ] <- as.numeric(sapply(1:p, function(jj) {
      if (!(jj %in% which_OC)) {
        ret <- rep(0, P)
      } else {
        ret <- t(out_function(1, M_OC, T_exp))
      }
      return(ret)
    }))

  }
  oc_mat <- oc_case_mat
  oc_list <- list()
  for (ii in 1:p) {
    index <- 1:P + (ii - 1) * P
    oc_list[[ii]] <- oc_mat[, index]
  }

  X_mat_list <- list()
  for (ii in 1:p) {
    X1_scaled <- csi_X %*% t(meig[[ii]])
    X_mat_list[[ii]] <-
      t(f1_m(x_seq, M_OC) + t(X1_scaled) * sd) + stats::rnorm(prod(dim(X1_scaled)), sd = sd_e) + cont_list[[ii]] + oc_list[[ii]]
  }
  names(X_mat_list) <- paste0("X", sprintf(1:p, fmt = "%02d"))
  out <- list(X_mat_list = X_mat_list, ind_out = ind_out)
  return(out)
}

