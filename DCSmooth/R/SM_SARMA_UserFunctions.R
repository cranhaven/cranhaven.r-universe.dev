################################################################################
#                                                                              #
#                DCSmooth Package: Estimation of SARMA models                  #
#                                                                              #
################################################################################

# Exported functions for the package user:

# sarma.est (exported)
  # qarma.est (exported)

# sarma.sim (exported)
  # qarma.sim (exported)

# sarma.order (UNUSED)

#---------------------------User Estimation Function---------------------------#

#' Estimation of an SARMA-process
#'
#' @description Parametric Estimation of an \eqn{SARMA(p, q)}-process on a 
#'  lattice.
#' 
#' @section Details:
#' The MA- and AR-parameters of a top-left quadrant ARMA process are estimated
#' by the specified method. The lag-orders of the \eqn{SARMA(p, q)} are given by
#'  \eqn{p = (p_1, p_2), q = (q_1, q_2)}{p = (p1, p2), q = (q1, q2)}, where
#'  \eqn{p_1, q_1}{p1, q1} are the lags over the rows and \eqn{p_2, q_2}{p2, q2}
#'  are the lags over the columns. The estimation process is based on the model
#'  \deqn{\phi(B_{1}B_{2})X_{i,j} = \theta(B_{1}B_{2})u_{i,j}}{\phi(B1 B2)
#'  X[i,j] = \theta(B1 B2)u[i,j]}.
#' 
#' @param Y A numeric matrix that contains the demeaned observations of the
#'   random field or functional time-series.
#' @param method Method used for estimation of the parameters. One of \code{
#'   "HR", "sep", "RSS"}, default value is \code{"HR"}
#' @param model_order A list containing the orders of the SARMA model in the
#'   form \code{model_order = list(ar = c(p1, p2), ma = c(q1, q2))}. Default
#'   value is a \eqn{SARMA((1, 1), (1, 1))} model.
#' 
#' @return The function returns an object of class \code{"sarma"} including
#'  \tabular{ll}{
#'   \code{Y} \tab The matrix of observations, inherited from input.\cr
#'   \code{innov} The estimated innovations.\cr
#'   \code{model} \tab The estimated model consisting of the coefficient 
#'   matrices \code{ar} and \code{ma} and standard deviation of innovations
#'   \code{sigma}.\cr
#'   \code{stnry} \tab An logical variable indicating whether the estimated
#'   model is stationary.\cr
#' }
#' 
#' @seealso \code{\link{sarma.sim}}, \code{\link{sfarima.est}}
#' 
#' @examples
#' # See vignette("DCSmooth") for examples and explanation
#' 
#' ## simulation of SARMA process
#' ma <- matrix(c(1, 0.2, 0.4, 0.1), nrow = 2, ncol = 2)
#' ar <- matrix(c(1, 0.5, -0.1, 0.1), nrow = 2, ncol = 2)
#' sigma <- 0.5
#' sarma_model <- list(ar = ar, ma = ma, sigma = sigma)
#' sarma_simulated <- sarma.sim(100, 100, model = sarma_model)
#' sarma_simulated$model
#' 
#' ## estimation of SARMA process
#' sarma.est(sarma_simulated$Y)$model
#' sarma.est(sarma_simulated$Y, 
#'            model_order = list(ar = c(1, 1), ma = c(1, 1)))$model
#' 
#' @export
sarma.est <- function(Y, method = "HR",
                     model_order = list(ar = c(1, 1), ma = c(1, 1)))
{
  exception.check.Y(Y)
  exception.check.model_order(model_order, var_model = 
                              c("sarma_HR", "sarma_sep", "sarma_RSS"))
  
  if (method == "HR")
  {
    sarma_est <- sarma.HR.est(Y = Y, model_order = model_order)
  } else if (method == "sep") {
    sarma_est <- sarma.sep.est(Y = Y, model_order = model_order)
  } else if (method == "RSS") {
    sarma_est <- sarma.RSS.est(Y = Y, model_order = model_order)
  } else {
    stop("Unknown value ", method, " in argument \"method\".")
  }
  
  if (sarma_est$stnry == FALSE)
  {
    warning("Estimated SARMA model not stationary.")
  }
  
  return(sarma_est)
}

# Wrapper for old "qarma.est" function

#' @rdname sarma.est
#' @export
qarma.est <- function(Y, model_order = list(ar = c(1, 1), ma = c(1, 1)))
{
  sarma_est <- sarma.HR.est(Y = Y, model_order = model_order)
  message("Note: \"qarma.est\" is deprecated. Function redirects to ",
          "sarma.est with method = \"HR\".")
  return(sarma_est)
}

#----------------------------Simulation Function-------------------------------#

#' Simulation of a \eqn{SARMA(p, q)}-process
#' 
#' @description \code{sarma.sim} simulates a specified SARMA-model
#'  on a lattice with normally distributed innovations.
#' 
#' @section Details:
#' Simulation of a top-left dependent spatial ARMA process (SARMA). This 
#' function returns an object of class \code{"sarma"}. The simulated innovations
#' are created from a normal distribution with specified variance
#' \eqn{\sigma^2}{sigma^2}.
#' 
#' @param n_x Number of simulated observation rows.
#' @param n_t Number of simulated observation columns.
#' @param model A list containing the coefficient matrices \code{ar} and 
#'  \code{ma} of the SARMA model as well as the standard deviation of 
#'  innovations \code{sigma}.
#' 
#' @return The function returns an object of class \code{"sarma"}, consisting of
#' 
#'  \tabular{ll}{
#'   \code{Y} \tab A \eqn{n_x \times n_t}{n_x x n_t}-matrix of simulated values
#'   of the specified SARMA process.\cr
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
#' @seealso \code{\link{sarma.est}}, \code{\link{sfarima.est}}
#' 
#' @examples
#' # See vignette("DCSmooth") for examples and explanation
#'  
#' ma <- matrix(c(1, 0.2, 0.4, 0.1), nrow = 2, ncol = 2)
#' ar <- matrix(c(1, 0.5, -0.1, 0.1), nrow = 2, ncol = 2)
#' sigma <- 0.5
#' sarma_model <- list(ar = ar, ma = ma, sigma = sigma)
#' 
#' sarma_sim <- sarma.sim(100, 100, model = sarma_model)
#' summary(sarma_sim)
#' 
#' @export
sarma.sim <- function(n_x, n_t, model)
{
  exception.check.model.sarma(model)
  
  ar_mat <- as.matrix(model$ar); ma_mat = as.matrix(model$ma)
  ar_x <- dim(ar_mat)[1] - 1; ar_t = dim(ar_mat)[2] - 1
  ma_x <- dim(ma_mat)[1] - 1; ma_t = dim(ma_mat)[2] - 1
  x_init <- max(ar_x, ma_x) + 1
  t_init <- max(ar_t, ma_t) + 1
  
  # set coefficients for zero-lags
  if (ar_mat[1, 1] != 1)
  {
    warning("AR(0,0) coefficient in upper left of parameter matrix is not 1.")
  }
  ar_mat[1, 1] <- 0 # AR-coefficients
  
  n_mat <- floor(1.25 * c(n_x, n_t))
  error_mat <- matrix(stats::rnorm(prod(n_mat)), nrow = n_mat[1], 
                     ncol = n_mat[2]) * model$sigma
  arma_mat <- error_mat
  
  for (i in x_init:n_mat[1])
  {
    for (j in t_init:n_mat[2])
    {
      arma_mat[i, j] <- sum(-ar_mat * arma_mat[i:(i - ar_x), j:(j - ar_t)]) +
        sum(ma_mat * error_mat[i:(i - ma_x), j:(j - ma_t)])
    }
  }
  
  arma_out <- arma_mat[(n_mat[1] - n_x + 1):
                        n_mat[1], (n_mat[2] - n_t + 1):n_mat[2]]
  error_out <- error_mat[(n_mat[1] - n_x + 1):
                          n_mat[1], (n_mat[2] - n_t + 1):n_mat[2]]
  
  coef_out <- list(Y = arma_out, innov = error_out, model = model,
                  stnry = TRUE)
  class(coef_out) <- "sarma"
  attr(coef_out, "subclass") <- "sim"
  
  return(coef_out)
}

# Wrapper for old "qarma.sim" function

#' @rdname sarma.sim
#' @export

qarma.sim <- function(n_x, n_t, model)
{
  sarma_sim <- sarma.sim(n_x = n_x, n_t = n_t, model = model)
  message("Note: \"qarma.sim\" is deprecated. Function redirects to ",
          "sarma.sim.")
  return(sarma_sim)
}

#----------------------------SARMA Order Selection-----------------------------#

sarma.order <- function(Y, method = "sep", criterion = "bic",
                       order_max = list(ar = c(1, 1), ma = c(1, 1)),
                       parallel = FALSE)
{
  if (criterion %in% c("aic", "bic"))
  {
    order_opt <- sarma.order.aic.bic(Y, pmax = order_max$ar,
                                    qmax = order_max$ma,
                                    crit = criterion, restr = NULL, sFUN = min,
                                    parallel = parallel)
  } else if (criterion == "gpac") {
    order_opt <- sarma.order.gpac(Y, order_max = order_max)
  }
  
  return(order_opt)
}
