#' Detecting changes in exponential family
#'
#' @param dataset a numeric matrix of dimension \eqn{n\times d}, where each row represents an observation and each column stands for a variable. A numeric vector is also acceptable for univariate observations.
#' @param family a character string specifying the underlying distribution. In the current version, detecting changes in binomial ("\code{binom}"), multinomial ("\code{multinom}"), Poisson ("\code{pois}"), exponential ("\code{exp}"), geometric ("\code{geom}"), Dirichlet ("\code{diri}"), gamma ("\code{gamma}"), beta ("\code{beta}"), chi-square ("\code{chisq}") and inverse gaussian ("\code{invgauss}") distributions are supported.
#' @param size an integer indicating the number of trials only if \code{family = "binom"} or \code{family = "multinom"}.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#'
#' @return \code{cpss.em} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries. See \code{\link{cpss.custom}}.
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500):1590–1598.
#'
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @seealso \code{\link{cpss.meanvar}} \code{\link{cpss.mean}} \code{\link{cpss.var}}
#' @examples
#' library("cpss")
#' set.seed(666)
#' n <- 1000
#' tau <- c(100, 300, 700, 900)
#' tau_ext <- c(0, tau, n)
#' theta <- c(1, 0.2, 1, 0.2, 1)
#' seg_len <- diff(c(0, tau, n))
#' y <- unlist(lapply(seq(1, length(tau) + 1), function(k) {
#'   rexp(seg_len[k], theta[k])
#' }))
#' res <- cpss.em(
#'   y, family = "exp", algorithm = "WBS", ncps_max = 10,
#'   criterion = "MS", times = 10
#' )
#' cps(res)
#' # [1] 100 299 705 901
cpss.em <- function(dataset, family, size = NULL, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2) {

  dataset <- as.matrix(dataset)
  n <- nrow(dataset)
  param.opt <- list()
  param.opt$em <- family
  param.opt$N <- size
  if (family %in% c("binom", "multinom", "pois", "exp", "geom")) {
    res <- cpss.custom(
      dataset,
      n,
      g_subdat,
      g_param_em,
      g_cost_em,
      algorithm,
      dist_min,
      ncps_max,
      pelt_pen_val,
      pelt_K,
      wbs_nintervals,
      criterion,
      times,
      model = "em",
      g_smry = g_smry_em,
      easy_cost = NULL,
      param.opt = param.opt
    )
  } else if (family %in% c("diri", "gamma", "beta", "chisq", "invgauss")) {
    res <- cpss.custom(
      dataset,
      n,
      g_subdat,
      g_param_em,
      g_cost_em,
      algorithm,
      dist_min,
      ncps_max,
      pelt_pen_val,
      pelt_K,
      wbs_nintervals,
      criterion,
      times,
      model = "em",
      g_smry = NULL,
      easy_cost = NULL,
      param.opt = param.opt
    )
  } else {
    stop("Not yet supported distribution!")
  }

  res@call <- list(call = match.call())

  return(res)
}
