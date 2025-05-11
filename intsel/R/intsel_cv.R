#' Cross-validation for logistic regression with two-way interaction screening
#'
#' @description
#' Cross-validation function for \code{\link{intsel}}
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
#' @param nfolds Optional, the folds of cross-validation. Default is 10.
#' @param foldid Optional, user-specified vector indicating the cross-validation fold in which each observation should be included. Values in this vector should range from 1 to \code{nfolds}. If left unspecified, \code{intsel} will randomly assign observations to folds
#' @param tol Convergence criterion. Algorithm stops when the \eqn{l_2} norm of the parameter update is smaller than \code{tol}. Default is \code{1e-5}.
#' @param maxit Maximum number of iterations allowed. Default is \code{100L}.
#' @param verbose Logical, whether progress is printed. Default is \code{FALSE}.
#' @return A list.
#'   \item{lambdas}{A vector of lambda used for each cross-validation.}
#'   \item{cvm}{The cv error averaged across all folds for each lambda.}
#'   \item{cvsd}{The standard error of the cv error for each lambda.}
#'   \item{cvup}{The cv error plus its standard error for each lambda.}
#'   \item{cvlo}{The cv error minus its standard error for each lambda.}
#'   \item{nzero}{The number of non-zero coefficients at each lambda.}
#'   \item{intsel.fit}{A fitted model for the full data at all lambdas of class "\code{intsel}".}
#'   \item{lambda.min}{The lambda such that the \code{cvm} reach its minimum.}
#'   \item{lambda.1se}{The maximum of lambda such that the \code{cvm} is less than the minimum the \code{cvup} (the minmum of \code{cvm} plus its standard error).}
#'   \item{foldid}{The fold assignments used.}
#'   \item{index}{A one column matrix with the indices of \code{lambda.min} and \code{lambda.1se}}.
#'   \item{iterations}{A vector of number of iterations it takes to converge at each \eqn{\lambda} in \code{lambdas}}.
#'   \item{x.original}{The input matrix \code{x}.}
#'   \item{x}{The predictor matrix with \code{x} plus \code{p.screen} * (\code{p.screen} - 1)/2 interaction terms.}
#'   \item{y}{The input \code{y}.}
#'   \item{p.screen}{The input \code{p.screen}.}
#'   \item{intercept}{The input \code{intercept}.}
#' @export
#' @importFrom stats sd
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
#' cv <- intsel_cv(x = x,
#'                 y = y,
#'                 p.screen =5,
#'                 intercept = intercept,
#'                 stepsize_init = 1,
#'                 lambda = lambdas,
#'                 nfolds = 5,
#'                 foldid = NULL)
#' cv$index
intsel_cv <- function(
    x,
    y,
    weights,
    intercept = TRUE,
    p.screen,
    lambda,
    par_init,
    stepsize_init = 1,
    stepsize_shrink = 0.8,
    nfolds = 10,
    foldid = NULL,
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
  
  nlam <- length(lambda)
  lambdas <- sort(lambda, decreasing = TRUE)
  
  
  
  ### intsel fit
  
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
  iterations <- fit$Iterations
  row.name <- c(paste0("x", seq(p.main)),
                apply(two.way.intx, 1, function(x) {paste0("x", x[1], ":", "x", x[2])}))
  if (intercept) row.name <- c(row.name, "(Intercept)")
  rownames(estimates) <- row.name
  intsel.fit <- list(lambdas = lambdas,
                  estimates = estimates,
                  iterations = iterations)
  
  nzero <- colSums(apply(estimates, MARGIN = 2, "!=", 0))
  
  ### train-test split and pre-allocate result storage
  if (is.null(foldid)) {
    fold_id <- sample(rep(seq(nfolds), length = length(y)))
  } else {
    fold_id <- foldid
  }
  cv_error <- matrix(NA, nrow = nfolds, ncol = nlam)
  iterations <- matrix(NA, nrow = nfolds, ncol = nlam)
  
  for ( k in seq(nfolds) ) {
    message("fold ", k)
    
    rows_train <- fold_id != k

    train_x <- x[rows_train, ]
    train_y <- y[rows_train]
    train_w <- weights[rows_train]
    test_x <- x[!rows_train, ]
    test_y <- y[!rows_train]
    test_w <- weights[!rows_train]
    
    fit <- intsel_cpp(x = train_x,
                      y = train_y,
                      w = train_w,
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
    
    cv_error[k, ] <- apply(fit$Estimates, MARGIN = 2, FUN = l_ld, x = test_x, y = test_y, w = test_w)[1, ]
    iterations[k, ] <- fit$Iterations
  }
  
  ### cv evaluation
  cvm <- apply(cv_error, MARGIN = 2, FUN = mean)
  cvsd <- apply(cv_error, MARGIN = 2, FUN = sd)/sqrt(nfolds)
  cvup <- cvm + cvsd
  cvlo <- cvm - cvsd
  lam.min <- which.min(cvm)
  lam.1se <- sum(cvm[seq(lam.min)] > cvup[lam.min]) + 1
  lam.min.1se.id <- matrix(nrow = 2, ncol = 1)
  lam.min.1se.id[1, ] <- lam.min
  lam.min.1se.id[2, ] <- lam.1se
  rownames(lam.min.1se.id) <- c("min", "1se")
  colnames(lam.min.1se.id) <- "Lambda"
  
  results <- list(lambdas = lambdas,
                  cvm = cvm,
                  cvsd = cvsd,
                  cvup = cvup,
                  cvlo = cvlo,
                  nzero = nzero,
                  intsel.fit = intsel.fit,
                  lambda.min = lambdas[lam.min],
                  lambda.1se = lambdas[lam.1se],
                  foldid = fold_id,
                  index = lam.min.1se.id,
                  iterations = iterations,
                  x.original = x.origin,
                  x = x,
                  y = y,
                  p.screen = p.screen,
                  intercept = intercept)
  
  if ( sum(iterations == maxit) > 0 ) {
    warning("Maximum iterations reached at some lambdas. Check ` $iterations`.")
  }
  
  class(results$intsel.fit) <- "intsel"
  class(results) <- "intsel_cv"
  return(results)
}

