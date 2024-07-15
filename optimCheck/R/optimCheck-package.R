#' Graphical and numerical checks for mode-finding routines.
#'
#' @name optimCheck
#' @docType package
#' @importFrom graphics abline mtext par plot
#' @importFrom stats optim
#' @examples
#' # example: logistic regression
#' ilogit <- binomial()$linkinv
#'
#' # generate data
#' p <- sample(2:10,1) # number of parameters
#' n <- sample(1000:2000,1) # number of observations
#' X <- matrix(rnorm(n*p),n,p) # design matrix
#' beta0 <- rnorm(p, sd = .1) # true parameter values
#' y <- rbinom(n, size = 1, prob = ilogit(X %*% beta0)) # response
#'
#' # fit logistic regression
#' bhat <- coef(glm(y ~ X - 1, family = binomial))
#'
#' # check convergence
#'
#' # likelihood function
#' loglik <- function(beta, y, X) {
#'   sum(dbinom(y, size = 1, prob = ilogit(X %*% beta), log = TRUE))
#' }
#'
#' # projection plots
#' bnames <- parse(text = paste0("beta[", 1:p, "]"))
#' system.time({
#'   oproj <- optim_proj(xsol = bhat,
#'                       fun = function(beta) loglik(beta, y, X),
#'                       xnames = bnames,
#'                       xlab = "Coefficient", ylab = "Loglikelihood")
#' })
#'
#' # numerical summary
#' oproj # see ?summary.optproj for more information
#'
#' # elementwise differences between potential and optimal solution
#' diff(oproj) # same as summary(oproj)$xdiff
#'
#' # refit general purpose optimizer starting from bhat
#' # often faster than optim_proj, but less stable
#' system.time({
#'   orefit <- optim_refit(xsol = bhat,
#'                         fun = function(beta) loglik(beta, y, X))
#' })
#' orefit
NULL

