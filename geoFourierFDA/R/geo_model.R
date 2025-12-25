#' EStimates the parameters of the exponential model.
#'
#' \code{geo_model} finds the maximum likelihood estimate for the parameters
#' in the geostatistical exponential model.
#'
#' @param v_data a numeric vector with the data
#' @param m_coord a matrix with two column. The first column must be the
#' latitude and the second column must be the longitude.
#'
#' @return a list with components
#' \describe{
#'  \item{\code{mean}}{mean of the process}
#'  \item{\code{phi}}{range of exponential model}
#'  \item{\code{sigmasq}}{total sill of exponential model}
#'  \item{\code{convergence}}{convergence as specified in the function
#'  \code{nlminb}}
#' }
#' @export
#'
#' @examples
#'data(canada)
#'v_data <- canada$m_data[1, ]
#'geo_model(v_data, canada$m_coord)
geo_model <- function(v_data, m_coord) {

  # distance matrix
  m_dist <- m_coord %>%
    dist() %>%
    as.matrix()

  v_mu <- mean(v_data) * matrix(1, nrow = nrow(m_dist), ncol = 1) # mean
  v_diff <- matrix(v_data, ncol = 1) - v_mu

  f_min <- function(params) {
    # phi <- params[1]
    # sigmasq <- params[2]
    #
    # m_cov <- sigmasq * exp(-m_dist / phi)
    #
    # log(det(m_cov)) + t(solve(m_cov, v_diff)) %*% v_diff
    logLik(m_dist, v_diff, params[1], params[2])
  }

  fit <- nlminb(start = c(10, 2), f_min,
                lower = c(1e-8, 1e-8))

  list(phi = fit$par[1], sigmasq = fit$par[2], mean = mean(v_data),
       convergence = fit$convergence)
}

