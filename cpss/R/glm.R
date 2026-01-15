#' Detecting changes in GLMs
#'
#' @param formula a \code{formula} object specifying the GLM with change-points.
#' @param family a description of the error distribution and link function to be used in the model, which can be a character string naming a family function or a family function.
#' @param data an optional data frame containing the variables in the model.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#'
#' @return \code{cpss.glm} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries. See \code{\link{cpss.custom}}.
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500):1590–1598.
#'
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @seealso \code{\link{cpss.lm}}
#' @examples
#' library("cpss")
#' set.seed(666)
#' n <- 200
#' size <- rpois(n, 20 - 1) + 1
#' tau <- c(75, 100, 175)
#' tau_ext <- c(0, tau, n)
#' be <- list(c(0, 0.5), c(0, -0.5), c(0.5, -0.5), c(-0.5, -0.5))
#' seg_len <- diff(c(0, tau, n))
#' x <- rnorm(n)
#' eta <- lapply(seq(1, length(tau) + 1), function(k) {
#'   be[[k]][1] + be[[k]][2] * x[(tau_ext[k] + 1):tau_ext[k + 1]]
#' })
#' eta <- do.call(c, eta)
#' p <- 1 / (1 + exp(-eta))
#' y <- rbinom(n, size = size, prob = p)
#' \donttest{
#' pelt_pen_val <- (log(n))^seq(0.5, 2, by = 0.1)
#' res <- cpss.glm(
#'   formula = cbind(y, size - y) ~ x, family = binomial(),
#'   algorithm = "PELT", pelt_pen_val = pelt_pen_val, ncps_max = 10
#' )
#' summary(res)
#' # 75  105  175
#' coef(res)
#' # [1,] 0.02540872  0.08389551  0.5284425 -0.4980768
#' # [2,] 0.57222684 -0.45430385 -0.5203319 -0.4581678
#' }
#' @importFrom stats family glm model.response model.matrix
cpss.glm <- function(formula, family, data = NULL, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2) {

  temp <- glm(formula, family, data, method = "model.frame")
  dataset <- cbind(model.response(temp), model.matrix(temp))
  n <- nrow(dataset)
  res <- cpss.custom(
    dataset,
    n,
    g_subdat,
    g_param_glm,
    g_cost_glm,
    algorithm,
    dist_min,
    ncps_max,
    pelt_pen_val,
    pelt_K,
    wbs_nintervals,
    criterion,
    times,
    model = "glm",
    g_smry = NULL,
    easy_cost = NULL,
    param.opt = family
  )
  res@call <- list(call = match.call())

  return(res)
}

#' Detecting changes in linear models
#'
#' @param formula a \code{formula} object specifying the GLM with change-points.
#' @param data an optional data frame containing the variables in the model.
#' @param algorithm a character string specifying the change-point searching algorithm, one of the following choices: "SN" (segment neighborhood), "BS" (binary segmentation), "WBS" (wild binary segmentation) and "PELT" (pruned exact linear time) algorithms.
#' @param dist_min an integer specifying minimum searching distance (length of feasible segments).
#' @param ncps_max an integer specifying an upper bound of the number of true change-points.
#' @param pelt_pen_val a numeric vector specifying candidate values of the penalty only if \code{algorithm = "PELT"}.
#' @param pelt_K a numeric value for pruning adjustment only if \code{algorithm = "PELT"}. It is usually taken to be 0 if the negative log-likelihood is used as a cost, see Killick et al. (2012).
#' @param wbs_nintervals an integer specifying the number of random intervals drawn only if \code{algorithm = "WBS"}, see Fryzlewicz (2014).
#' @param criterion a character string specifying the model selection criterion, "CV" ("cross-validation") or "MS" ("multiple-splitting").
#' @param times an integer specifying how many times of sample-splitting should be performed; It should be 2 if \code{criterion = "CV"}.
#'
#' @return \code{cpss.lm} returns an object of an \proglang{S4} class, called "\code{cpss}", which collects data and information required for further change-point analyses and summaries. See \code{\link{cpss.custom}}.
#' @export
#'
#' @references
#' Killick, R., Fearnhead, P., and Eckley, I. A. (2012). Optimal Detection of Changepoints With a Linear Computational Cost. Journal of the American Statistical Association, 107(500):1590–1598.
#'
#' Fryzlewicz, P. (2014). Wild binary segmentation for multiple change-point detection. The Annals of Statistics, 42(6): 2243–2281.
#' @seealso \code{\link{cpss.glm}}
#' @examples
#' library("cpss")
#' set.seed(666)
#' n <- 400
#' tau <- c(80, 200, 300)
#' tau_ext <- c(0, tau, n)
#' be <- list(c(0, 1), c(1, 0.5), c(0, 1), c(-1, 0.5))
#' seg_len <- diff(c(0, tau, n))
#' x <- rnorm(n)
#' mu <- lapply(seq(1, length(tau) + 1), function(k) {
#'   be[[k]][1] + be[[k]][2] * x[(tau_ext[k] + 1):tau_ext[k + 1]]
#' })
#' mu <- do.call(c, mu)
#' sig <- unlist(lapply(seq(1, length(tau) + 1), function(k) {
#'   rep(be[[k]][2], seg_len[k])
#' }))
#' y <- rnorm(n, mu, sig)
#' res <- cpss.lm(
#'   formula = y ~ x,
#'   algorithm = "BS", ncps_max = 10
#' )
#' summary(res)
#' # 80  202  291
#' coef(res)
#' # $coef
#' #             [,1]      [,2]        [,3]       [,4]
#' # [1,] -0.00188792 1.0457718 -0.03963209 -0.9444813
#' # [2,]  0.91061557 0.6291965  1.20694409  0.4410036
#' #
#' # $sigma
#' # [1] 0.8732233 0.4753216 0.9566516 0.4782329
#' @importFrom stats gaussian
cpss.lm <- function(formula, data = NULL, algorithm = "BS", dist_min = floor(log(n)), ncps_max = ceiling(n^0.4), pelt_pen_val = NULL, pelt_K = 0, wbs_nintervals = 500, criterion = "CV", times = 2) {

  family <- gaussian()
  temp <- glm(formula, family, data, method = "model.frame")
  dataset <- cbind(model.response(temp), model.matrix(temp))
  n <- nrow(dataset)
  res <- cpss.custom(
    dataset,
    n,
    g_subdat,
    g_param_glm,
    g_cost_glm,
    algorithm,
    dist_min,
    ncps_max,
    pelt_pen_val,
    pelt_K,
    wbs_nintervals,
    criterion,
    times,
    model = "lm",
    g_smry = NULL,
    easy_cost = NULL,
    param.opt = family
  )
  res@call <- list(call = match.call())

  return(res)
}
