#' cross-validation for \code{netcox}
#'
#' @description
#' Conduct cross-validation (cv) for \code{netcox}.
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
#' @param nfolds Optional, the folds of cross-validation. Default is 10.
#' @param stepsize_init Initial value of the stepsize of the optimization algorithm. Default is 1.
#' @param stepsize_shrink Factor in \eqn{(0,1)} by which the stepsize shrinks in the backtracking linesearch. Default is 0.8.
#' @param tol Convergence criterion. Algorithm stops when the \eqn{l_2} norm of the difference between two consecutive updates is smaller than \code{tol}.
#' @param maxit Maximum number of iterations allowed.
#' @param verbose Logical, whether progress is printed.
#' 
#' @details
#' For each lambda, 10 folds cross-validation (by default) is performed. The cv error is defined as follows. Suppose we perform \eqn{K}-fold cross-validation, denote \eqn{\hat{\beta}^{-k}} by the estimate obtained from the rest of \eqn{K-1} folds (training set). The error of the \eqn{k}-th fold (test set) is defined as \eqn{2(P-Q)} divided by \eqn{R}, where \eqn{P} is the log partial likelihood evaluated at  \eqn{\hat{\beta}^{-k}} using the entire dataset, Q is the log partial likelihood evaluated at \eqn{\hat{\beta}^{-k}} using the training set, and R is the number of events in the test set. We do not use the negative log partial likelihood evaluated at \eqn{\hat{\beta}^{-k}} using the test set because the former definition can efficiently use the risk set, and thus it is more stable when the number of events in each test set is small (think of leave-one-out). The cv error is used in parameter tuning. To account for balance in outcomes among the randomly formed test set, we divide the deviance \eqn{2(P-Q)} by R. 
#' To get the estimated coefficients that has the minimum cv error, use \code{netcox()$Estimates[netcox()$Lambdas==netcox_cv()$lambda.min]}. To apply the 1-se rule, use \code{netcox()$Estimates[netcox()$Lambdas==netcox_cv()$lambda.1se]}.
#' 
#' @examples 
#' grp <- matrix(c(0, 0, 0, 0, 0,
#'                 0, 0, 0, 0, 0,
#'                 1, 1, 0, 0, 0,
#'                 0, 0, 0, 0, 0,
#'                 0, 1, 0, 1, 0),
#'               ncol = 5, byrow = TRUE)
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
#' cv <- netcox_cv(x = x,
#'                 ID = sim$Id,
#'                 time = sim$Start,
#'                 time2 = sim$Stop,
#'                 event = sim$Event,
#'                 lambda = lam.seq,
#'                 group = grp,
#'                 group_variable = grp.var,
#'                 penalty_weights = eta_g,
#'                 nfolds = 5,
#'                 tol = 1e-4,
#'                 maxit = 1e3,
#'                 verbose = FALSE)
#'                 
#' @return A list.
#'   \item{lambdas}{A vector of lambda used for each cross-validation.}
#'   \item{cvm}{The cv error averaged across all folds for each lambda.}
#'   \item{cvsd}{The standard error of the cv error for each lambda.}
#'   \item{cvup}{The cv error plus its standard error for each lambda.}
#'   \item{cvlo}{The cv error minus its standard error for each lambda.}
#'   \item{nzero}{The number of non-zero coefficients at each lambda.}
#'   \item{netcox.fit}{A netcox fit for the full data at all lambdas.}
#'   \item{lambda.min}{The lambda such that the \code{cvm} reach its minimum.}
#'   \item{lambda.1se}{The maximum of lambda such that the \code{cvm} is less than the minimum the \code{cvup} (the minmum of \code{cvm} plus its standard error).}
#' @seealso \code{\link{netcox}}, \code{\link{plot_netcox_cv}}.

netcox_cv <- function(x, ID,
                      time, time2, event,
                      lambda, group,
                      group_variable,
                      penalty_weights,
                      par_init,
                      nfolds = 10,
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
  
  unique_ID <- unique(ID)
  n <- length(unique_ID)
  
  nlam <- length(lambda)
  lambdas <- sort(lambda, decreasing = TRUE)
  
  ### netcox fit
  
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
  
  netcox.fit <- list(lambdas = lambdas,
                     estimates = estimates,
                     iterations = iterations)
  
  nzero <- colSums(apply(estimates, MARGIN = 2, "!=", 0))
  
  ### train-test split and pre-allocate result storage
  fold_id <- sample(rep(seq(nfolds), length = n))
  cv_error <- matrix(NA, nrow = nfolds, ncol = nlam)
  iterations <- matrix(NA, nrow = nfolds, ncol = nlam)
  
  for ( k in seq(nfolds) ) {
    message("fold ", k)
    
    which_train <- unique_ID[fold_id != k]
    rows_train <- ID %in% which_train
    
    n_tmp <- length(which_train)
    
    train_x <- x[rows_train, ]
    train_time <- time[rows_train]
    train_time2 <- time2[rows_train]
    train_event <- event[rows_train]
    test_x <- x[!rows_train, ]
    test_time <- time[!rows_train]
    test_time2 <- time2[!rows_train]
    test_event <- event[!rows_train]
    
    fit <- netcox_cpp(x = train_x,
                      start = train_time,
                      stop = train_time2,
                      event = train_event,
                      n_unique = n_tmp,
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
    
    deviance_full <- glmnet::coxnet.deviance(y = Surv(time,
                                                      time2,
                                                      event),
                                             x = x,
                                             beta = fit$Estimates)
    deviance_train <- glmnet::coxnet.deviance(y = Surv(train_time,
                                                       train_time2,
                                                       train_event),
                                              x = train_x,
                                              beta = fit$Estimates)
    
    cv_error[k, ] <- ( deviance_full - deviance_train )/sum(test_event)
    iterations[k, ] <- fit$Iterations
  }
  
  ### cv evaluation
  cvm <- apply(cv_error, MARGIN = 2, FUN = mean)
  cvsd <- apply(cv_error, MARGIN = 2, FUN = sd)/sqrt(nfolds)
  cvup <- cvm + cvsd
  cvlo <- cvm - cvsd
  lam.min <- which.min(cvm)
  lam.1se <- sum(cvm[seq(lam.min)] > cvup[lam.min]) + 1
  
  results <- list(lambdas = lambdas,
                  cvm = cvm,
                  cvsd = cvsd,
                  cvup = cvup,
                  cvlo = cvlo,
                  nzero = nzero,
                  netcox.fit = netcox.fit,
                  lambda.min = lambdas[lam.min],
                  lambda.1se = lambdas[lam.1se],
                  iterations = iterations)
  
  if ( sum(iterations == maxit) > 0 ) {
    warning("Maximum iterations reached at some lambdas. Check ` $iterations`.")
  }
  
  return(results)
}

