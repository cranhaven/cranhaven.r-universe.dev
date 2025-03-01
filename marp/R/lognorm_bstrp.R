#' A function to generate (double) bootstrap samples and fit Log-Normal renewal model
#' @param n number of inter-event times
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param B number of bootstrap samples
#' @param BB number of double-bootstrap samples
#' @param par_hat estimated parameters
#' @param mu_hat estimated mean inter-event times
#' @param pr_hat estimated time to event probability
#' @param haz_hat estimated hazard rates
#' @param y user-specified time point (used to compute time-to-event probability)
#'
#' @return returns list of estimates after fitting Log-Normal renewal model on (double) bootstrap samples
#' \describe{
#' \item{mu_star}{Estimated mean from bootstrapped samples }
#' \item{pr_star}{Estimated probability from bootstrapped samples }
#' \item{haz_star}{Estimated hazard rates from bootstrapped samples}
#' \item{mu_var_hat}{Variance of estimated mean}
#' \item{pr_var_hat}{Variance of estimated probability}
#' \item{haz_var_hat}{Variance of estimated hazard rates}
#' \item{mu_var_double}{Variance of estimated mean of bootstrapped samples (via double-bootstrapping)}
#' \item{pr_var_double}{Variance of estimated probability of bootstrapped samples (via double-bootstrapping)}
#' \item{haz_var_double}{Variance of estimated hazard rates of bootstrapped samples (via double-bootstrapping)}
#' \item{mu_Tstar}{Pivot quantity of the estimated mean}
#' \item{pr_Tstar}{Pivot quantity of the estimated probability}
#' \item{haz_Tstar}{Pivot quantity of the estimated hazard rates}
#' }
#'
#' @examples
#' \donttest{
#' # set some parameters
#' n <- 30 # sample size
#' t <- seq(100, 200, by = 10) # time intervals
#' B <- 100 # number of bootstraps
#' BB <- 100 # number of double-bootstraps
#' # m <- 10 # number of iterations for MLE optimization
#' par_hat <- c(
#'   3.41361e-03, 2.76268e+00, 2.60370e+00, 3.30802e+02, 5.48822e+00, 2.92945e+02, NA,
#'   9.43071e-03, 2.47598e+02, 1.80102e+00, 6.50845e-01, 7.18247e-01
#' )
#' mu_hat <- c(292.94512, 292.94513, 319.72017, 294.16945, 298.87286, 292.94512)
#' pr_hat <- c(0.60039, 0.42155, 0.53434, 0.30780, 0.56416, 0.61795)
#' haz_hat <-   matrix(c(
#'   -5.67999, -5.67999, -5.67999, -5.67999, -5.67999, -5.67999,
#'   -5.67999, -5.67999, -5.67999, -5.67999, -5.67999, -6.09420,
#'   -5.99679, -5.91174, -5.83682, -5.77031, -5.71085, -5.65738,
#'   -5.60904, -5.56512, -5.52504, -5.48833, -6.09902, -5.97017,
#'   -5.85769, -5.75939, -5.67350, -5.59856, -5.53336, -5.47683,
#'   -5.42805, -5.38621, -5.35060, -6.17146, -6.09512, -6.02542,
#'   -5.96131, -5.90194, -5.84668, -5.79498, -5.74642, -5.70064,
#'   -5.65733, -5.61624, -5.92355, -5.80239, -5.70475, -5.62524,
#'   -5.55994, -5.50595, -5.46106, -5.42359, -5.39222, -5.36591,
#'   -5.34383, -5.79111, -5.67660, -5.58924, -5.52166, -5.46879,
#'   -5.42707, -5.39394, -5.36751, -5.34637, -5.32946, -5.31596
#' ),length(t),6)
#' y <- 304 # cut-off year for estimating probablity
#'
#' # generate bootstrapped samples then fit renewal model
#' res <- marp::lognorm_bstrp(n, t, B, BB, par_hat, mu_hat, pr_hat, haz_hat, y)
#' }
#'
#' @export


lognorm_bstrp <- function(n, t, B, BB, par_hat, mu_hat, pr_hat, haz_hat, y) {
  ## bootstraps
  bstrp <- replicate(B, stats::rlnorm(n, par_hat[5], par_hat[11]))
  ## fit a Gamma Log-Normal model
  star <- apply(bstrp, 2, function(x) lognorm_rp(x, t, y))
  ## parameters
  par1_star <- sapply(star, '[[', 1)
  par2_star <- sapply(star, '[[', 2)
  ## estimated mean, (logit) probability and (log) hazard rates
  mu_star <- sapply(star, '[[', 6)
  pr_star <- sapply(star, '[[', 7)
  haz_star <- sapply(star, '[[', 8)
  ## variance of estimated mean, (logit) probability and (log) hazard rates
  mu_var_hat <- stats::var(mu_star)
  pr_var_hat <- stats::var(pr_star)
  haz_var_hat <- apply(haz_star, 1, stats::var)
  ## double-bootstraps & fit a Log-Normal renewal model
  double <- sapply(1:B, function(i) replicate(BB, stats::rlnorm(n, par1_star[i], par2_star[i])), simplify = "array")
  star_double <- apply(double, c(2, 3), function(x) lognorm_rp(x, t, y))
  ## estimated mean, (logit) probability and (log) hazard rates from double bootstraps
  mu_double <- apply(star_double, c(1, 2), function(x) sapply(x, '[[', 6))
  pr_double <- apply(star_double, c(1, 2), function(x) sapply(x, '[[', 7))
  haz_double <- apply(star_double, c(1, 2), function(x) sapply(x, '[[', 8))
  ## variance of estimated mean, (logit) probability and (log) hazard rates from double bootstraps
  mu_var_double <- apply(mu_double, 2, stats::var)
  pr_var_double <- apply(pr_double, 2, stats::var)
  haz_var_double <- apply(haz_double, c(1, 3), stats::var)
  ## t-statistics of estimated mean, (logit) probability and (log) hazard rates from double bootstraps
  mu_Tstar <- (mu_star - mu_hat[5]) / sqrt(mu_var_double)
  pr_Tstar <- (pr_star - pr_hat[5]) / sqrt(pr_var_double)
  haz_Tstar <- (haz_star - haz_hat[, 5]) / sqrt(haz_var_double)
  return(list('mu_star' = mu_star,
              'pr_star' = pr_star,
              'haz_star' = haz_star,
              'mu_var_hat' =  mu_var_hat,
              'pr_var_hat' =  pr_var_hat,
              'haz_var_hat' = matrix(haz_var_hat, length(t), 1),
              'mu_var_double' = mu_var_double,
              'pr_var_double' = pr_var_double,
              'haz_var_double' = haz_var_double,
              'mu_Tstar' = mu_Tstar,
              'pr_Tstar' = pr_Tstar,
              'haz_Tstar' = haz_Tstar))
}
