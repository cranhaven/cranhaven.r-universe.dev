#' A function to fit model-averaged renewal process
#' @param n number of inter-event times
#' @param t user-specified time intervals (used to compute hazard rate)
#' @param B number of bootstrap samples
#' @param BB number of double-bootstrap samples
#' @param m the number of iterations in nlm
#' @param par_hat estimated parameters
#' @param mu_hat estimated mean inter-event times
#' @param pr_hat estimated time to event probability
#' @param haz_hat estimated hazard rates
#' @param y user-specified time point (used to compute time-to-event probability)
#'
#' @return returns list of estimates after fitting different renewal models on (double) bootstrap samples
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
#' m <- 10 # number of iterations for MLE optimization
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
#' res <- marp::marp_bstrp(n, t, B, BB, m, par_hat, mu_hat, pr_hat, haz_hat, y)
#' }
#'
#' @export

### Model-Averaged Renewal Processes -------------------------------
marp_bstrp <- function(n,t,B,BB,m,par_hat,mu_hat,pr_hat,haz_hat,y){
  out <- mapply(rbind,
                poisson_bstrp(n,t,B,BB,par_hat,mu_hat,pr_hat,haz_hat,y), ## 1. Poisson renewal model
                gamma_bstrp(n,t,B,BB,m,par_hat,mu_hat,pr_hat,haz_hat,y), ## 2. Gamma renewal model
                loglogis_bstrp(n,t,B,BB,m,par_hat,mu_hat,pr_hat,haz_hat,y), ## 3. Log-Logistics renewal model
                weibull_bstrp(n,t,B,BB,m,par_hat,mu_hat,pr_hat,haz_hat,y), ## 4. Weibull renewal model
                lognorm_bstrp(n,t,B,BB,par_hat,mu_hat,pr_hat,haz_hat,y), ## 5. Log-Normal renewal model
                bpt_bstrp(n,t,B,BB,m,par_hat,mu_hat,pr_hat,haz_hat,y) ## 6. BPT renewal model
  )
  return(list("mu_star" = out$mu_star,
              "pr_star" = out$pr_star,
              "haz_star" = array(out$haz_star, c(length(t), 6, B)),
              "mu_var_hat" = out$mu_var_hat,
              "pr_var_hat" = out$pr_var_hat,
              "haz_var_hat" = array(out$haz_var_hat, c(length(t), 6, 1)),
              "mu_var_double" = out$mu_var_double,
              "pr_var_double" = out$pr_var_double,
              "haz_var_double" = array(out$haz_var_double, c(length(t), 6, B)),
              "mu_Tstar" = out$mu_Tstar,
              "pr_Tstar" = out$pr_Tstar,
              "haz_Tstar" = array(out$haz_Tstar, c(length(t), 6, B))))
}
