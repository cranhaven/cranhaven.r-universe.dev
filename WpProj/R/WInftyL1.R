#' Infinity-Wasserstein Linear Projections With an L1 Penalty
#'
#' @param X An n x p matrix of covariates
#' @param Y An n x s matrix of predictions
#' @param theta optional parameter matrix for selection methods. Should be p x s.
#' @param penalty Form of penalty. One of "none", "lasso", "mcp","scad"
#' @param lambda Penalty parameter for lasso regression.
#' @param lambda.min.ratio Minimum lambda ratio for self selected lambda.
#' @param gamma tuning parameters for SCAD and MCP. 
#' @param nlambda Number of lambda values. 
#' @param solver Which solver to use. One of "cone","mosek", or "gurobi". Note "mosek" and "gurobi" are commercial installers.
#' @param options A list containing slots `solver_opts`, options for each solver, `init`, initial conditions fed into each solver, `tol`, tolerance for convergence, and `iter`, the maximum number of iterations
#' @param model.size The maximum number of paramters to consider. Should be an integer greater than 1 and less than or equal to the number of covariates
#' @param display.progress Whether to display progress. TRUE or FALSE
#' @param ... Additional arguments passed to the solver as needed
#'
#' @return A `WpProj` object
#' @keywords internal
#'
# @examples
# if(rlang::is_installed("stats")) {
# n <- 128
# p <- 10
# s <- 99
# x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
# beta <- (1:10)/10
# y <- x %*% beta + stats::rnorm(n)
# post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
# post_mu <- x %*% post_beta
# 
# fit <-  WInfL1(X=x, Y=t(post_mu), theta = t(post_beta),
#              penalty = "lasso",
#              solver = "cone", lambda = 0.5
# )
# }
WInfL1 <- function(X, Y, theta = NULL, penalty = c("none","lasso", "mcp","scad"), 
                 lambda = numeric(0), 
                 lambda.min.ratio = 1e-4, 
                 gamma = 1.5,
                 nlambda = 10, 
                 solver = c("cone","mosek","gurobi"),
                 options = list(solver_opts = NULL,
                                init = NULL,
                                tol = 1e-7,
                                iter = 100),
                 model.size = NULL,
                 display.progress = FALSE,
                 ...) {
  
  mosek_found <- rlang::is_installed("Rmosek")
  gurobi_found <- rlang::is_installed("gurobi")
  ecos_found <- rlang::is_installed("ROI.plugin.ecos")
  if (!mosek_found && !gurobi_found && !ecos_found) {
    stop("One of `Rmosek`, `gurobi`, or `ROI` with `ROI.plugin.ecos` must be installed to use this function")
  }
  
  this.call <- as.list(match.call()[-1])
  
  solver <- match.arg(solver)
  
  if(any(penalty == "ols")) penalty <- "none"
  if(any(grepl("lasso", penalty))) penalty <- "lasso"
  if(any(penalty == "elastic.net")) penalty <- "lasso"
  if(any(grepl("mcp", penalty))) penalty <- "mcp"
  if(any(grepl("scad", penalty))) penalty <- "scad"
  penalty <- match.arg(penalty, choices = c("none","lasso", "mcp","scad"))
  
  n <- nrow(X)
  d <- ncol(X)
  
  if(is.null(Y) | missing(Y)) {
    if(!(is.null(theta) | missing(theta))) {
      if(nrow(theta) != ncol(X)) theta <- t(theta)
      Y <- X %*% theta
    }
  }
  
  s <- ncol(Y)
  # cols <- lapply(1:s, function(ss) Matrix::sparseMatrix(i = n*(ss-1) + rep(1:n,d), 
  #                                                       j = rep(1:d,each = n), 
  #                                                       x = c(X),
  #                                                       dims = c(n*s, d)))
  # Xmat <- do.call(cbind, cols)
  Xmat <- Matrix::sparseMatrix(i = c(sapply(1:s, function(ss) n*(ss-1) + rep(1:n,d))), 
                               j = c(sapply(1:s, function(ss) rep(1:d + d * (ss-1),each = n))), 
                               x = c(X),
                               dims = c(n*s, d*s))
  
  
  if(penalty != "none" & length(lambda) == 0) {
    if(!is.null(theta)) {
      lambda.max <- max(sqrt(rowSums(theta^2)))
      if(lambda.max == 0) lambda.max <- max(abs(crossprod(X,Y)))/(n)
    } else {
      lambda.max <- max(abs(crossprod(X,Y)))/(n)
    }
    lambda <-  exp(log(lambda.max) + seq(0, log(lambda.min.ratio), length.out = nlambda))
  } else if (penalty == "none"){
    lambda <- 0
    nlambda <- 0
  }
  
  if(is.null(options$tol)) options$tol <- 1e-7
  if(is.null(options$iter)) options$iter <- 100
  
  if(is.null(model.size) | length(model.size) == 0) {
    model.size <- ncol(Xmat)
  } else {
    model.size <- model.size * s
  }
  
  # check duplicated names in dots
  dots <- list(...)
  if ( any(...names() %in% methods::formalArgs(GroupLambda)) ) {
    dots <- dots[!names(dots) %in% methods::formalArgs(GroupLambda)]
  }
  
  GL_args <- c(list(X = Xmat, Y = Y, power = Inf, groups = rep(1:d,s), lambda = lambda,
                  penalty = penalty,
                  gamma = gamma, solver = solver,
                  model.size = model.size,
                  options = options, 
                  display.progress = display.progress),
  dots)
  
  beta <- do.call("GroupLambda", GL_args)
    # beta <- linf_norm(X = Xmat, Y = Y, deriv_func = deriv_func, thresholder = thresh_fun,
    #                  lambda = lambda, groups=rep(1:d, s), solver = solver, 
    #                  gamma = gamma, opts = options$solver_opts, init = options$init, iter = options$iter, tol = options$tol)
    
  if(solver == "mosek") Rmosek::mosek_clean()
  
  output <- list()
  output$beta <- as.matrix(beta)
  output$penalty <- penalty
  output$lambda <- lambda
  output$nvars <- d
  output$maxit <- NULL
  output$call <- formals(WInfL1)
  output$call[names(this.call)] <- this.call
  output$nonzero_beta <- colSums(output$beta != 0)
  output$method <- "projection"
  output$power <- Inf
  class(output) <- c("WpProj", "optimization")
  
  extract <- extractTheta(output, matrix(0, d,s))
  output$nzero <- extract$nzero
  output$eta <- lapply(extract$theta, function(tt) X %*%tt )
  output$theta <- extract$theta
  return(output)
} 
