#' Detecting changes in mean
#'
#' @param dataset a numeric matrix of dimension \eqn{n\times d}, where each row represents an observation and each column stands for a variable. A numeric vector is also acceptable for univariate observations.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#' @param Sigma if a numeric matrix (or constant) is supplied, it will be taken as the value of the common covariance (or variance). By default it is \code{NULL}, and the covariance is estimated by \deqn{\widehat{\Sigma} = \frac{1}{2(n-1)}\sum_{i=1}^{n-1} (Y_i-Y_{i+1})(Y_i-Y_{i+1})';}
#'
#' @return \code{cpss.mean} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries. See \code{\link{cpss.custom}}.
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500): 1590–1598.
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @seealso \code{\link{cpss.meanvar}} \code{\link{cpss.var}}
#' @examples
#' library("cpss")
#' set.seed(666)
#' n <- 2048
#' tau <- c(205, 267, 308, 472, 512, 820, 902, 1332, 1557, 1598, 1659)
#' seg_len <- diff(c(0, tau, n))
#' mu <- rep(c(0, 14.64, -3.66, 7.32, -7.32, 10.98, -4.39, 3.29, 19.03, 7.68, 15.37, 0), seg_len)
#' ep <- 7 * rnorm(n)
#' y <- mu + ep
#' \donttest{
#' res <- cpss.mean(y, algorithm = "SN", ncps_max = 20)
#' summary(res)
#' # 205  267  307  471  512  820  897  1332  1557  1601  1659
#' plot(res, type = "scatter")
#' plot(res, type = "path")
#' out <- update(res, dim_update = 12)
#' out@cps
#' # 205  267  307  471  512  820  897 1332 1557 1601 1659 1769
#' # coef(out)
#' }
cpss.mean <- function(dataset, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2, Sigma = NULL) {

  dataset <- as.matrix(dataset)
  n <- nrow(dataset)
  if (is.null(Sigma)) {
    Sigma <- est_cov(dataset)
  }
  res <- cpss.custom(
    dataset,
    n,
    g_subdat,
    g_param_mean,
    g_cost_mvnorm,
    algorithm,
    dist_min,
    ncps_max,
    pelt_pen_val,
    pelt_K,
    wbs_nintervals,
    criterion,
    times,
    model = "mean",
    g_smry = g_smry_mean,
    easy_cost = NULL,
    param.opt = Sigma
  )
  res@call <- list(call = match.call())

  return(res)
}

#' Detecting changes in (co)variance
#'
#' @param dataset a numeric matrix of dimension \eqn{n\times d}, where each row represents an observation and each column stands for a variable. A numeric vector is also acceptable for univariate observations.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#' @param mu If a numeric vector or constant is supplied, it will be taken as the value of the common mean. By default it is \code{NULL}, and the mean is estimated by the sample mean.
#'
#' @return \code{cpss.var} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries. See \code{\link{cpss.custom}}.
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500): 1590–1598.
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @seealso \code{\link{cpss.meanvar}} \code{\link{cpss.mean}}
#' @examples
#' library("cpss")
#' if (!requireNamespace("MASS", quietly = TRUE)) {
#'   stop("Please install the package \"MASS\".")
#' }
#' set.seed(666)
#' n <- 1000
#' tau <- c(200, 500, 750)
#' mu <- list(rep(0, 2), rep(0, 2), rep(0, 2), rep(0, 2))
#' Sigma <- list(diag(2), matrix(c(1, 0, 0, 4), 2), matrix(c(1, -0.5, -0.5, 4), 2), diag(2))
#' seg_len <- diff(c(0, tau, n))
#' y <- lapply(seq(1, length(tau) + 1), function(k) {
#'   MASS::mvrnorm(n = seg_len[k], mu = mu[[k]], Sigma = Sigma[[k]])
#' })
#' y <- do.call(rbind, y)
#' res <- cpss.var(y, algorithm = "BS", dist_min = 20)
#' cps(res)
#' # [1] 215 515 751
cpss.var <- function(dataset, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2, mu = NULL) {

  dataset <- as.matrix(dataset)
  n <- nrow(dataset)
  if (is.null(mu)) {
    mu <- colMeans(dataset)
  }
  res <- cpss.custom(
    dataset,
    n,
    g_subdat,
    g_param_var,
    g_cost_mvnorm,
    algorithm,
    dist_min,
    ncps_max,
    pelt_pen_val,
    pelt_K,
    wbs_nintervals,
    criterion,
    times,
    model = "var",
    g_smry = g_smry_var,
    easy_cost = NULL,
    param.opt = mu
  )
  res@call <- list(call = match.call())

  return(res)
}

#' Detecting changes in mean and (co)variance
#'
#' @param dataset a numeric matrix of dimension \eqn{n\times d}, where each row represents an observation and each column stands for a variable. A numeric vector is also acceptable for univariate observations.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#' @return \code{cpss.meanvar} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries. See \code{\link{cpss.custom}}.
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500):1590–1598.
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @seealso \code{\link{cpss.mean}} \code{\link{cpss.var}}
#' @examples
#' library("cpss")
#' if (!requireNamespace("MASS", quietly = TRUE)) {
#'   stop("Please install the package \"MASS\".")
#' }
#' set.seed(666)
#' n <- 1000
#' tau <- c(200, 400, 600, 800)
#' mu <- list(rep(0, 2), rep(1, 2), rep(1, 2), rep(0, 2), rep(0, 2))
#' Sigma <- list(diag(2), diag(2), matrix(c(1,-1,-1, 4), 2), matrix(c(1, 0.5, 0.5, 1), 2), diag(2))
#' seg_len <- diff(c(0, tau, n))
#' y <- lapply(seq(1, length(tau) + 1), function(k) {
#'   MASS::mvrnorm(n = seg_len[k], mu = mu[[k]], Sigma = Sigma[[k]])
#' })
#' y <- do.call(rbind, y)
#' res <- cpss.meanvar(y, algorithm = "BS", dist_min = 20)
#' cps(res)
#' # [1] 211 402 598 804
#' plot(res, type = "coef")
cpss.meanvar <- function(dataset, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2) {

  dataset <- as.matrix(dataset)
  n <- nrow(dataset)
  res <- cpss.custom(
    dataset,
    n,
    g_subdat,
    g_param_meanvar,
    g_cost_mvnorm,
    algorithm,
    dist_min,
    ncps_max,
    pelt_pen_val,
    pelt_K,
    wbs_nintervals,
    criterion,
    times,
    model = "meanvar",
    g_smry = g_smry_meanvar,
    easy_cost = NULL,
    param.opt = NULL
  )
  res@call <- list(call = match.call())

  return(res)
}
