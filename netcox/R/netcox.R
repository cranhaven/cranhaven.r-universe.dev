#' fit a (time-dependent) Cox model with structured variable selection
#'
#' @description
#' Fit a (time-dependent) Cox model via penalized maximum likelihood, where the penalization is a weighted sum of infinity norm of (overlapping) groups of coefficients. The regularization path is computed at a grid of values for the regularization parameter lambda.
#' 
#' @param x Predictor matrix with dimension \eqn{nm * p}, where \eqn{n} is the number of subjects, \eqn{m} is the maximum observation time, and \eqn{p} is the number of predictors. See Details.
#' @param ID The ID of each subjects, each subject has one ID (many rows in \code{x} share one \code{ID}).
#' @param time Represents the start of each time interval.
#' @param time2 Represents the stop of each time interval.
#' @param event Indicator of event. \code{event = 1} when event occurs and \code{event = 0} otherwise.
#' @param lambda Sequence of regularization coefficients \eqn{\lambda}'s.
#' @param group \eqn{G * G} matrix describing the relationship between the groups of variables, where \eqn{G} represents the number of groups. Denote the \eqn{i}-th group of variables by \eqn{g_i}. The \eqn{(i,j)} entry is \code{1} if and only if \eqn{i\neq j} and \eqn{g_i} is a child group (subset) of \eqn{g_j}, and is \code{0} otherwise. See Examples and Details.
#' @param group_variable \eqn{p * G} matrix describing the relationship between the groups and the variables. The \eqn{(i,j)} entry is \code{1} if and only if variable \eqn{i} is in group \eqn{g_j}, but not in any child group of \eqn{g_j}, and is \code{0} otherwise. See Examples and Details.
#' @param penalty_weights Optional, vector of length \eqn{G} specifying the group-specific penalty weights. If not specified, the default value is \eqn{\mathbf{1}_G}. Modify with caution.
#' @param par_init Optional, vector of initial values of the optimization algorithm. Default initial value is zero for all \eqn{p} variables.
#' @param stepsize_init Initial value of the stepsize of the optimization algorithm. Default is 1.
#' @param stepsize_shrink Factor in \eqn{(0,1)} by which the stepsize shrinks in the backtracking linesearch. Default is 0.8.
#' @param tol Convergence criterion. Algorithm stops when the \eqn{l_2} norm of the difference between two consecutive updates is smaller than \code{tol}.
#' @param maxit Maximum number of iterations allowed.
#' @param verbose Logical, whether progress is printed.
#' 
#' @examples 
#' # g3 in g1 -> grp_31 = 1
#' # g3 in g2 -> grp_32 = 1
#' # g5 in g2 -> grp_52 = 1
#' # g5 in g4 -> grp_54 = 1
#' grp <- matrix(c(0, 0, 0, 0, 0,
#'                 0, 0, 0, 0, 0,
#'                 1, 1, 0, 0, 0,
#'                 0, 0, 0, 0, 0,
#'                 0, 1, 0, 1, 0),
#'               ncol = 5, byrow = TRUE)
#'
#' # Variable A1 is in g1 only: grp.var_11 = 1
#' # Variable A1B is in g1 and g3, but g3 is a child group of g1,
#' # so grp.var_63 = 1 while grp.var_61 = 0.
#' grp.var <- matrix(c(1, 0, 0, 0, 0, #A1
#'                     1, 0, 0, 0, 0, #A2
#'                     0, 0, 0, 1, 0, #C1
#'                     0, 0, 0, 1, 0, #C2
#'                     0, 1, 0, 0, 0, #B
#'                     0, 0, 1, 0, 0, #A1B
#'                     0, 0, 1, 0, 0, #A2B
#'                     0, 0, 0, 0, 1, #C1B
#'                     0, 0, 0, 0, 1  #C2B
#'                    ), ncol = 5, byrow = TRUE)
#' eta_g <- rep(1, 5)
#' x <- as.matrix(sim[, c("A1","A2","C1","C2","B",
#'                        "A1B","A2B","C1B","C2B")])
#' lam.seq <- 10^seq(0, -2, by = -0.2)
#' 
#' fit <- netcox(x = x,
#'               ID = sim$Id,
#'               time = sim$Start,
#'               time2 = sim$Stop,
#'               event = sim$Event,
#'               lambda = lam.seq,
#'               group = grp,
#'               group_variable = grp.var,
#'               penalty_weights = eta_g,
#'               tol = 1e-4,
#'               maxit = 1e3,
#'               verbose = FALSE)
#' @details
#' The predictor matrix should be of dimension \eqn{nm * p}. Each row records the values of covariates for one subject at one time, for example, the values at the day from \code{time} (Start) to \code{time2} (Stop). An example dataset \code{\link{sim}} is provided. The dataset has the same format produced by the \code{R} package \pkg{PermAlgo}. 
#' The specification of arguments \code{group} and \code{group_variable} for the grouping structure can be found in \url{http://thoth.inrialpes.fr/people/mairal/spams/doc-R/html/doc_spams006.html#sec27}, the same as the grouping structure specification in the \code{R} package \pkg{spams}.
#'
#' In the Examples below, \eqn{p=9,G=5}, the group structure is: \deqn{g_1 = \{A_{1}, A_{2}, A_{1}B, A_{2}B\},} \deqn{g_2  = \{B, A_{1}B, A_{2}B, C_{1}B, C_{2}B\},} \deqn{g_3  = \{A_{1}B, A_{2}B\},} \deqn{g_4  = \{C_1, C_2, C_{1}B, C_{2}B\},} \deqn{g_5  = \{C_{1}B, C_{2}B\}.}
#' 
#' where \eqn{g_3} is a subset of \eqn{g_1} and \eqn{g_2}, and \eqn{g_5} is a subset of \eqn{g_2} and \eqn{g_4}.
#' @return A list with the following three elements.
#'   \item{lambdas}{The user-specified regularization coefficients \code{lambda} sorted in decreasing order.}
#'   \item{estimates}{A matrix, with each column corresponding to the coefficient estimates at each \eqn{\lambda} in \code{lambdas}.}
#'   \item{iterations}{A vector of number of iterations it takes to converge at each \eqn{\lambda} in \code{lambdas}.}
netcox <- function(x, ID,
                   time, time2, event,
                   lambda,
                   group,
                   group_variable,
                   penalty_weights,
                   par_init,
                   stepsize_init = 1,
                   stepsize_shrink = 0.8,
                   tol = 1e-5,
                   maxit = 1000L,
                   verbose = FALSE) {
  
  p <- ncol(x)
  if (missing(par_init)) {
    par_init <- rep(0, p)
  }
  
  G <- nrow(group)
  if (missing(penalty_weights)) {
    penalty_weights <- rep(1, G)
  }
  
  n <- length(unique(ID))
  
  lambdas <- sort(lambda, decreasing = TRUE)
  
  fit <- netcox_cpp(x = x,
                    start = time,
                    stop = time2,
                    event = event,
                    n_unique = n,
                    regul = "graph",
                    lam = lambdas,
                    grp = group,
                    grpV = group_variable,
                    etaG = penalty_weights,
                    init = par_init,
                    l_ld = l_ld,
                    init_stepsize = stepsize_init,
                    ls_shrink = stepsize_shrink,
                    partol = tol,
                    maxit = maxit,
                    verbose = verbose)
  
  estimates <- fit$Estimates
  colnames(estimates) <- lambdas
  iterations <- fit$Iterations
  
  results <- list(lambdas = lambdas,
                  estimates = estimates,
                  iterations = iterations)
  
  if ( sum(iterations == maxit) > 0 ) {
    warning("Maximum iterations reached at some lambdas. Check ` $iterations`.")
  }
  
  return(results)
}