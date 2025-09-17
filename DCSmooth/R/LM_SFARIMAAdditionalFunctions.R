################################################################################
#                                                                              #
#                DCSmooth Package: Additional Functions for SFARIMA            #
#                                                                              #
################################################################################

### Functions related to SFARIMA but not directly linked to estimation

  # sfarima.sim
  # macoef (UNUSED?)

#----------------------------Simulation Function-------------------------------#

#' Simulation of a \eqn{SFARIMA(p, q, d)}-process
#' 
#' @description \code{sfarima.sim} simulates a specified SFARIMA-model
#'  on a lattice with normally distributed innovations.
#' 
#' @section Details:
#' Simulation of a separable spatial fractionally ARIMA process (SFARIMA). This 
#' function returns an object of class \code{"sfarima"}. The simulated
#' innovations are created from a normal distribution with specified variance
#' \eqn{\sigma^2}{sigma^2}.
#' 
#' @param n_x Number of simulated observation rows.
#' @param n_t Number of simulated observation columns.
#' @param model A list containing the coefficient matrices \code{ar} and 
#'  \code{ma} of the QARMA model, the long memory parameter vector \code{d} as
#'  well as the standard deviation of innovations \code{sigma}.
#' 
#' @return The function returns an object of class \code{"sfarima"}, consisting
#' of
#' 
#'  \tabular{ll}{
#'   \code{Y} \tab A \eqn{n_x \times n_t}{n_x x n_t}-matrix of simulated values
#'   of the specified SFARIMA process.\cr
#'   \code{innov} \tab The innovations used for simulation, iid. drawn from a 
#'    normal distribution with zero mean and variance 
#'    \eqn{\sigma^2}{(sigma)^2}.\cr
#'   \code{model} \tab The model used for simulation, inherited from input.\cr
#'   \code{stnry} \tab An logical variable indicating whether the simulated 
#'   model is stationary.\cr
#' }
#' 
#' @section Details: see the vignette for further details.
#' 
#' @seealso \code{\link{qarma.est}}
#' 
#' @examples
#' # See vignette("DCSmooth") for examples and explanation
#'  
#' ma <- matrix(c(1, 0.2, 0.4, 0.1), nrow = 2, ncol = 2)
#' ar <- matrix(c(1, 0.5, -0.1, 0.1), nrow = 2, ncol = 2)
#' d <- c(0.1, 0.1)
#' sigma <- 0.5
#' sfarima_model <- list(ar = ar, ma = ma, d = d, sigma = sigma)
#' 
#' sfarima_sim <- sfarima.sim(100, 100, model = sfarima_model)
#' surface.dcs(sfarima_sim$Y)
#' 
#' @export

sfarima.sim <- function(n_x, n_t, model)
{
  ar_mat <- as.matrix(model$ar); ma_mat = as.matrix(model$ma)
  ar_x <- -ar_mat[-1, 1]; ar_t = -ar_mat[1, -1]
  ma_x <- ma_mat[-1, 1]; ma_t = ma_mat[1, -1]
  
  # check if provided model is correctly specified
  if (isFALSE(all.equal(as.matrix(ar_mat[-1, -1]), ar_x %*% t(ar_t)))  ||
      isFALSE(all.equal(as.matrix(ma_mat[-1, -1]), ma_x %*% t(ma_t))))
  {
    warning("Provided coefficient matrices do not specify a separable process.")
  }
  if (!any(is.numeric(model$d)) || any(abs(model$d) > 0.5))
  {
    stop("Long memory parameter \"d\" incorrectly specified.")
  }
  
  # meta options
  nstart <- max(floor(1.5 * c(n_x, n_t)), 150)
  k_x <- min(50, n_x); k_t = min(50, n_t)
  
  n_x <- n_x + nstart
  n_t <- n_t + nstart
  eps_mat <- matrix(stats::rnorm(n_x * n_t), n_x, n_t) * model$sigma
  
  ma_inf_x <- c(1, stats::ARMAtoMA(ar = ar_x, ma = ma_x, lag.max = k_x))
  d_x <- choose(-model$d[1], 0:k_x) * ((-1)^(0:k_x))
  coef_x <- cumsum_part_reverse(d_x, ma_inf_x)
  
  ma_inf_t <- t(c(1, stats::ARMAtoMA(ar = ar_t, ma = ma_t, lag.max = k_t)))
  d_t <- choose(-model$d[2], 0:k_t) * ((-1)^(0:k_t))
  coef_t <- cumsum_part_reverse(d_t, ma_inf_t)
  
  X1.sim <- X2.sim <- matrix(0, n_x, n_t)

  for(j in 1:n_t) {
    if (j <= k_t) {
      X2.sim[, j] <- eps_mat[, j:1, drop = FALSE] %*% coef_t[1:j, drop = FALSE] 
    }
    else {
      X2.sim[, j] <- eps_mat[, j:(j - k_t)] %*% coef_t
    }
  }
  for(i in 1:n_x) {
    if (i <= k_x) {
      X1.sim[i, ] <- coef_x[1:i, drop = FALSE] %*% X2.sim[i:1, , drop = FALSE]
    }
    else {
      X1.sim[i, ] <- t(coef_x) %*% X2.sim[i:(i - k_x), ]
    }
  }
    
  sfarima_out <- X1.sim[(nstart + 1):n_x, (nstart + 1):n_t]
  error_out <- eps_mat[(nstart + 1):n_x, (nstart + 1):n_t]
  coef_out <- list(Y = sfarima_out, innov = error_out, model = model,
                  stnry = TRUE)
  class(coef_out) <- "sfarima"
  attr(coef_out, "subclass") <- "sim"
    
  return(coef_out)
}

#--------------------BIC/AIC ORDER SELECTION FOR SFARIMA-----------------------#

sfarima.ord <- function(Rmat, pmax = c(0, 0), qmax = c(0, 0), crit = "bic",
                        restr = NULL, sFUN = min, parallel = TRUE)
{
  if(crit == "bic") {
    crit.fun = stats::BIC
  }
  else if (crit == "aic") {
    crit.fun = stats::AIC
  }
  
  bic_x =  matrix(0, pmax[1] + 1, qmax[1] + 1)
  bic_t =  matrix(0, pmax[2] + 1, qmax[2] + 1)
  R_x = as.vector(Rmat)
  R_t = as.vector(t(Rmat))
  
  if (parallel == TRUE)
  {
    n.cores = parallel::detectCores(logical = TRUE) - 1
    doParallel::registerDoParallel(n.cores)
    
    `%dopar%` = foreach::`%dopar%`
    `%:%` = foreach::`%:%`
    
    bic_x = foreach::foreach(i = 1:(pmax[1] + 1), .combine = "rbind") %:%
      foreach::foreach(j = 1:(qmax[1] + 1), .combine = "c") %dopar%
      {
        bic = crit.fun(suppressWarnings(fracdiff::fracdiff(R_x, nar = i - 1,
                                                           nma = j - 1, drange = c(0, 0.5))))
      }
    
    bic_t = foreach::foreach(i = 1:(pmax[2] + 1), .combine = "rbind") %:%
      foreach::foreach(j = 1:(qmax[2] + 1), .combine = "c") %dopar%
      {
        bic = crit.fun(suppressWarnings(fracdiff::fracdiff(R_t, nar = i - 1,
                                                           nma = j - 1, drange = c(0, 0.5))))
      }
    
    doParallel::stopImplicitCluster()
  } else {
    for (i in 1:(pmax[1] + 1))
    {
      for (j in 1:(qmax[1] + 1))
      {
        bic_x[i, j] = crit.fun(suppressWarnings(fracdiff::fracdiff(R_x,
                                                                   nar = i - 1, nma = j - 1, drange = c(0, 0.5))))
      }
    }
    for (i in 1:(pmax[2] + 1))
    {
      for (j in 1:(qmax[2] + 1))
      {
        bic_t[i, j] = crit.fun(suppressWarnings(fracdiff::fracdiff(R_t,
                                                                   nar = i - 1, nma = j - 1, drange = c(0, 0.5))))
      }
    }
  }
  
  restr = substitute(restr)
  if(!is.null(restr)){
    ord.opt_x <- c(which(bic_x == sFUN(bic_x[eval(restr)]), arr.ind = TRUE) - 1)
    ord.opt_t <- c(which(bic_t == sFUN(bic_t[eval(restr)]), arr.ind = TRUE) - 1)
  } else {
    ord.opt_x <- c(which(bic_x == sFUN(bic_x), arr.ind = TRUE) - 1)
    ord.opt_t <- c(which(bic_t == sFUN(bic_t), arr.ind = TRUE) - 1)
  }
  
  # put model_orders into list
  ar = c(ord.opt_x[1], ord.opt_t[1])
  ma = c(ord.opt_x[2], ord.opt_t[2])
  model_order = list(ar = ar, ma = ma)
  
  return(model_order)   
}