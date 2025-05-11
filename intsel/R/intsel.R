#' Logistic regression with two-way interaction screening
#'
#' @description
#' Fit a logistic regression model including all the two-way interaction terms between the user-specified set of variables. The method uses an overlapping group lasso penalty that respects the commonly recognized selection rule, which is that, when the interaction term is selected into the model, both main effect terms should be in the model too. The regularization path is computed at a grid of values for the regularization parameter lambda.
#' 
#' @param x Predictor matrix with dimension \eqn{n * p}, where \eqn{n} is the number of subjects, and \eqn{p} is the number of predictors.
#' @param y Binary outcome, a vector of length \eqn{n}.
#' @param weights Optional, observation weights. Default is 1 for all observations.
#' @param intercept Logical, indicating whether an intercept term should be included in the model. The intercept term will not be penalized. The default is \code{TRUE}.
#' @param p.screen Number of variables of which all two-way interactions are screened. These variables should be placed in the \code{p.screen} left-most columns of matrix \code{x}.
#' @param lambda Sequence of regularization coefficients \eqn{\lambda}'s. Will be sorted in a decreasing order.
#' @param par_init Optional, vector of initial values of the optimization algorithm. Default initial value is zero for all \eqn{p} variables.
#' @param stepsize_init Initial value of the stepsize of the optimization algorithm. Default is 1.0.
#' @param stepsize_shrink Factor in \eqn{(0,1)} by which the stepsize shrinks in the backtracking linesearch. Default is 0.8.
#' @param tol Convergence criterion. Algorithm stops when the \eqn{l_2} norm of the parameter update is smaller than \code{tol}. Default is \code{1e-5}.
#' @param maxit Maximum number of iterations allowed. Default is \code{100L}.
#' @param verbose Logical, whether progress is printed. Default is \code{FALSE}.
#' @return A list.
#'   \item{lambdas}{The user-specified regularization coefficients \code{lambda} sorted in decreasing order.}
#'   \item{estimates}{A matrix, with each column corresponding to the coefficient estimates at each \eqn{\lambda} in \code{lambdas}.}
#'   \item{iterations}{A vector of number of iterations it takes to converge at each \eqn{\lambda} in \code{lambdas}.}
#'   \item{x.original}{The input matrix \code{x}.}
#'   \item{x}{The predictor matrix with \code{x} plus \code{p.screen} * (\code{p.screen} - 1)/2 interaction terms.}
#'   \item{y}{The input \code{y}.}
#'   \item{p.screen}{The input \code{p.screen}.}
#'   \item{intercept}{The input \code{intercept}.}
#' @export
#' @examples
#' n <- 1000
#' p.int <- 5
#' p.noint <- 3
#' intercept <- TRUE
#' p.screen <- 5
#' 
#' p.int.expand <- p.int*(p.int-1)/2
#' p.main <- p.int + p.noint
#' x <- matrix(rnorm(n * p.main), nrow = n, ncol = p.main)
#' 
#' # true model
#' # logit(p) = 0.1 + 0.3 x1 + 0.3 x2 + 0.3 x8 + 0.2 * x1 * x2
#' 
#' beta.true <- rep(0, p.main)
#' beta.true[c(1, 2, p.main)] <- 0.3
#' eta <- x %*% beta.true + 0.2 * x[, 1] * x[, 2]
#' 
#' if (intercept) eta <- eta + 0.1
#' 
#' py <- 1/(1 + exp(-eta))
#' 
#' y <- rbinom(n, 1, py)
#' 
#' nlam <- 30
#' lambdas <- exp(seq(log(0.1), log(0.00005), length.out = nlam))
#' 
#' # All the pairwise two-way interactions for the first p.screen variables 
#' # are included in the model and screened in a data-driven manner.
#' fit <- intsel(x = x,
#'               y = y,
#'               p.screen = 5,
#'               intercept = intercept,
#'               lambda = lambdas)
#' fit$iterations
#' fit$estimates[, 1]

intsel <- function(
    x,
    y,
    weights,
    intercept = TRUE,
    p.screen,
    lambda,
    par_init,
    stepsize_init = 1,
    stepsize_shrink = 0.8,
    tol = 1e-5,
    maxit = 1000L,
    verbose = FALSE
) {
  if (missing(weights)) weights <- rep(1, nrow(x))
  
  penalty <- "overlapping"
  
  p.main <- ncol(x)
  p <- p.main + p.screen*(p.screen - 1)/2
  
  two.way.intx <- unname(as.matrix(expand.grid(seq(p.screen), seq(p.screen))))
  two.way.intx <- two.way.intx[, 2:1]
  two.way.intx <- two.way.intx[two.way.intx[, 1] < two.way.intx[, 2], ]
  
  x.int <- matrix(NA, nrow = nrow(x), ncol = nrow(two.way.intx))
  for (i in seq(nrow(two.way.intx))) {
    x.int[, i] <- x[, two.way.intx[i, 1]] * x[, two.way.intx[i, 2]]
  }
  
  x.origin <- x
  x <- cbind(x, x.int)
  
  groups <- vector(mode = "list", length = p )
  for (i in 1:p) {
    groups[[i]] <- c(i, which(rowMeans(i == two.way.intx) > 0) + p.main)
  }
  
  for (i in (p.main+1):p) {
    groups[[i]] <- i
  }
  
  if (intercept) {
    x <- cbind(x, 1)
    p <- p + 1
    groups <- c(groups, p)
  }
  
  grp.pars <- overlap_structure(groups)
  if (intercept) {
    grp.pars$group_weights[p] <- 1e-6
  }
  
  if (missing(par_init)) {
    par_init <- rep(0, p)
  }

  lambdas <- sort(lambda, decreasing = TRUE)

  fit <- intsel_cpp(x = x,
                    y = y,
                    w = weights,
                    regul = "graph",
                    lam = lambdas,
                    grp = grp.pars$groups,
                    grpV = grp.pars$groups_var,
                    own_var = integer(),
                    N_own_var = integer(),
                    etaG = grp.pars$group_weights,
                    init = par_init,
                    init_stepsize = stepsize_init,
                    ls_shrink = stepsize_shrink,
                    partol = tol,
                    maxit = maxit,
                    verbose = verbose)
  
  estimates <- fit$Estimates
  colnames(estimates) <- lambdas
  row.name <- c(paste0("x", seq(p.main)),
                apply(two.way.intx, 1, function(x) {paste0("x", x[1], ":", "x", x[2])}))
  if (intercept) row.name <- c(row.name, "(Intercept)")
  rownames(estimates) <- row.name
  iterations <- fit$Iterations
  
  results <- list(lambdas = lambdas,
                  estimates = estimates,
                  iterations = iterations,
                  x.original = x.origin,
                  x = x,
                  y = y,
                  p.screen = p.screen,
                  intercept = intercept)
  
  if ( sum(iterations == maxit) > 0 ) {
    warning("Maximum iterations reached at some lambdas. Check ` $iterations`.")
  }
  
  class(results) <- "intsel"
  return(results)
}