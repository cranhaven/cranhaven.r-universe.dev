#' Obtain Optimized Sensitivity Parameters Using Multivariate Calibration Criterion
#'
#' @param mu_y_dt Scalar or vector that contains naive estimates of treatment effects
#' ignoring confounding.
#' @param mu_u_dt Matrix of difference in conditional confounder means, \eqn{E(U \mid t1) - E(U \mid t2)},
#' with latent variables in columns.
#' @param cov_u_t Covariance matrix of confounders conditional on treatments.
#' @param sigma_y_t Scalar of the standard deviation of outcome conditional on treatments.
#' @param R2_constr an optional scalar or vector specifying the upper limit constraint on \eqn{R^2} .
#' By default, \code{R2_constr = 1}.
#' @param normtype character. Optional function \code{m} for the multivariate calibration criterion.
#' By default, the L2 norm will be applied.\cr
#' "L1" - apply the L1 norm, \code{sum(abs(x))}. \cr
#' "L2" - apply the L2 norm, \code{sqrt(sum(x^2))}.\cr
#' "Inf" - apply the infinity norm, \code{max(abs(x))}. \cr
#' @param ... further arguments passed to \code{\link{solve}}
#'
#' @return Optimized sensitivity parameters.
#'

get_opt_gamma <- function(mu_y_dt, mu_u_dt, cov_u_t, sigma_y_t,
                          R2_constr = 1, normtype = "L2", ...) {

    gamma <- CVXR::Variable(ncol(mu_u_dt))
    if (normtype == "L1") {
      # obj <- cvxr_norm(mu_y_dt - mu_u_dt %*% gamma, 1)
      obj <- sum(abs(mu_y_dt - mu_u_dt %*% gamma))
    } else if (normtype == "L2") {
      obj <- CVXR::cvxr_norm(mu_y_dt - mu_u_dt %*% gamma, 2)
      # obj <- sqrt(sum((mu_y_dt - mu_u_dt %*% gamma)^2))
    } else if (normtype == "Inf") {
      # obj <- cvxr_norm(mu_y_dt - mu_u_dt %*% gamma, "inf")
      obj <- max(abs(mu_y_dt - mu_u_dt %*% gamma))
    } else {stop("Please specify a norm type.")}

    constr <- list(CVXR::quad_form(gamma, cov_u_t/sigma_y_t^2) <= R2_constr)
    prob <- CVXR::Problem(CVXR::Minimize(obj),  constr)
    result <- CVXR::solve(prob, ...)
    return(result$getValue(gamma))
}


