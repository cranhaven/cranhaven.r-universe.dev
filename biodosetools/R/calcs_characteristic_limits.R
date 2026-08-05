#' Characteristic limits function-----------------------------------------------
#' @param mu0 Background rate
#' @param n1 Number of cells that will be analysed.
#' @param y0 Background number of dicentrics.
#' @param n0 Background number of cells analysed.
#' @param alpha Type I error rate, 0.05 by default.
#' @param beta Type II error rate, 0.1 by default.
#' @param ymax Max dicentrics to evaluate.
#' @param type Type.
#'
#' @example man/examples/calculate_characteristic_limits_example.R
#' @return List of characteristic limits (\code{decision_threshold}, \code{detection_limit}).
#' @export

calculate_characteristic_limits <- function(mu0=NULL, n1=NULL, y0=NULL, n0=NULL, alpha = 0.05, beta = 0.1, ymax=100, type = "const") {
  y <- 1:ymax
  if(type == "const"){
    p <- sapply(y, function(x) stats::poisson.test(x, n1, r = mu0)$p.value)
    y_d <- y[min(which(p < alpha & y/n1 > mu0))] - 1

  }else{
    p <- sapply(y, function(x) stats::poisson.test(c(x, y0), c(n1, n0))$p.value)
    y_d <- y[min(which(p < alpha & y/n1 > y0/n0))] - 1
  }

  y_z <- stats::qchisq(1 - beta, 2 * (y_d + 1)) / 2
  # Return objects
  results_list <- list(
    decision_threshold = y_d,
    detection_limit = y_z
  )

  return(results_list)
}

##############
#
# fun.dec.thresh.variable <- function(y, y0, n1, n0){
#   y.seq <- 0:y
#   p <- n1/(n1 + n0)
#   return(1-sum(exp(lchoose(y0 + y + 1, y.seq) + y.seq*log(p) + (y0 + y + 1 -y.seq)*log(1-p))))
# }
