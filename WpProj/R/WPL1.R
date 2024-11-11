#' p-Wasserstein Linear Projections With an \eqn{L_1} Penalty
#'
#' @param X matrix of covariates
#' @param Y matrix of predictions
#' @param theta optional parameter matrix for selection methods.
#' @param power power of the Wasserstein distance
#' @param penalty Form of penalty. One of "lasso", "ols", "mcp", "elastic.net","selection.lasso", "scad", "mcp.net", "scad.net", "grp.lasso", "grp.lasso.net", "grp.mcp","grp.scad", "grp.mcp.net", "grp.scad.net", "sparse.grp.lasso"
#' @param model.size How many coefficients should final model have
#' @param lambda penalty parameter
#' @param nlambda number of lambdas to explore
#' @param lambda.min.ratio minimum ratio of max to min lambda
#' @param gamma Tuning parameter for SCAD and MCP methods
#' @param maxit maximum iterations for optimization
#' @param tol tolerance for convergence
#' @param ... arguments passed to other methods such as Wasserstein distance
#'
#' @return object of class `WpProj`
#' @keywords internal
#' @seealso [W1L1()], [W2L1()], [WInfL1]
#' 
# @examples
#  if(rlang::is_installed("stats")) {
# n <- 128
# p <- 10
# s <- 99
# x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
# beta <- (1:10)/10
# y <- x %*% beta + stats::rnorm(n)
# post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
# post_mu <- x %*% post_beta
# 
# fit <-  WPL1(X=x, Y=post_mu, power = 2.0,
#                penalty = "lasso",
#                method = "projection" #default
# )
# }
WPL1 <- function(X, Y=NULL, theta = NULL, power = 2.0,
                 penalty =  c("lasso", "ols", "mcp", "elastic.net",
                              "selection.lasso",
                              "scad", "mcp.net",
                              "scad.net",
                              "grp.lasso",
                              "grp.lasso.net", "grp.mcp",
                              "grp.scad", "grp.mcp.net",
                              "grp.scad.net",
                              "sparse.grp.lasso"),
                 model.size = NULL,
                 lambda = numeric(0),
                 nlambda = 100L,
                 lambda.min.ratio = 1e-4,
                 gamma = 1, maxit = 500L,
                 tol = 1e-07, ...)
{
  pot.args <- c(as.list(environment()), list(...))
  this.call <- as.list(match.call()[-1])
  stopifnot(power >= 1)
  if(power == 2.0) {
    w2l1.arg.sel <- which(names(pot.args) %in% methods::formalArgs("W2L1"))
    w2l1.args <- pot.args[w2l1.arg.sel]
    argn <- lapply(names(w2l1.args), as.name)
    names(argn) <- names(w2l1.args)
    f.call <- as.call(c(list(as.name("W2L1")), argn))
    output <- eval(f.call, envir = w2l1.args)
    # output <- W2L1(X = X, Y = Y, theta = theta, penalty = penalty,
    #                )
  # } else if (power == 1) {
  #   w1l1.args <- pot.args
  #   w1l1.args$alpha <- w1l1.args$tau <-  w1l1.args$power <- NULL
  #   # w2l1.arg.sel <- which(names(pot.args) %in% formalArgs("W2L1"))
  #   # w2l1.args <- pot.args[w2l1.arg.sel]
  #   argn <- lapply(names(w1l1.args), as.name)
  #   names(argn) <- names(w1l1.args)
  #   f.call <- as.call(c(list(as.name("W1L1")), argn))
  #   output <- eval(f.call, envir = w1l1.args)
  # } else if (power == Inf) {
  #   winfl1.args <- pot.args
  #   winfl1.args$alpha <- winfl1.args$tau <- winfl1.args$power <- NULL
  #   # w2l1.arg.sel <- which(names(pot.args) %in% formalArgs("W2L1"))
  #   # w2l1.args <- pot.args[w2l1.arg.sel]
  #   argn <- lapply(names(winfl1.args), as.name)
  #   names(argn) <- names(winfl1.args)
  #   f.call <- as.call(c(list(as.name("WInfL1")), argn))
  #   output <- eval(f.call, envir = winfl1.args)
  } else {
    # lp.arg.sel <- which(names(pot.args) %in% formalArgs("lp_reg"))
    # lp.args <- pot.args[lp.arg.sel]
    # lp.args$x <- X
    # lp.args$y <- Y
    # argn <- lapply(names(lp.args), as.name)
    # names(argn) <- names(lp.args)
    # f.call <- as.call(c(list(as.name("lp_reg")), argn))
    # output <- eval(f.call, envir = lp.args)
    dots <- list(...)
    if(is.null(dots$alpha)) dots$alpha <- 1
    if(is.null(dots$tau)) dots$tau <- 0.5
    if(is.null(dots$display.progress)) dots$display.progress <- FALSE
    output <- lp_reg(x = X, y = Y, theta = theta, power = power, gamma = gamma,
                     alpha = dots$alpha,
                     tau = dots$tau,
                     penalty = penalty,
                     penalty.factor = dots$penalty.factor,
                     lambda = lambda,
                     nlambda = nlambda,
                     lambda.min.ratio = lambda.min.ratio,
                     model.size = model.size,
                     iter = maxit,
                     tol = tol,
                     display.progress = dots$display.progress)
  }
  
  return(output)
  
}


lp_reg <- function(x, y, theta = NULL, power, 
                   gamma = 3, 
                   alpha = 1, tau = .5, 
                   penalty, penalty.factor = numeric(0),
                   lambda = numeric(0),
                   nlambda = 100,
                   lambda.min.ratio = 1e-4,
                   model.size = NULL,
                   iter = 100,
                   tol = 1e-7,
                   display.progress = FALSE) {
  this.call <- as.list(match.call()[-1])
  
  log_sum_exp <- function(x) {
    # if(is.vector(x)) {
    if(all(is.infinite(x))) return(x[1])
    mx <- max(x)
    x_temp <- x - mx
    return(log(sum(exp(x_temp)))+ mx)
    # } else if (is.matrix(x)) {
    #   mx <- apply(x, 1, max)
    #   x_temp <- x - mx
    #   return(log(rowSums(exp(x_temp)))+ mx)
    # }
  }
  obs_weight_update <- function(X,Y,beta,power,pmin_limit) {
    resid <- log(abs(Y - X %*% beta))
    w <- resid * (power - 2)
    if(is.finite(power) && all(is.finite(w))) {
      w <- resid * (power - 2)
      w <- pmin(w, pmin_limit)
      w <- exp(w - log_sum_exp(w) )@x # because is sparseMat
    } else {
      w <- rep(0, nrow(X))
      w[which.max(resid@x)] <- 1.0
    }
    return(w)
  }
  n <- nrow(x)
  d <- ncol(x)
  
  if(is.null(y) | missing(y)) {
    if(!(is.null(theta) | missing(theta))) {
      if(nrow(theta) != ncol(x)) theta <- t(theta)
      y <- x %*% theta
    }
  }
  
  s <- ncol(y)
  cols <- lapply(1:s, function(ss) Matrix::sparseMatrix(i = n*(ss-1) + rep(1:n,d),
                                                        j = rep(1:d,each = n),
                                                        x = c(x),
                                                        dims = c(n*s, d)))
  Xmat <- do.call(cbind, cols)
  rm(cols)
  
  if(is.null(model.size) | length(model.size) == 0) {
    model.size <- ncol(Xmat)
  } else {
    model.size <- model.size * s
  }
  
  Y <- as.matrix(c(y))
  obs.weights <- list(rep(1/(n*s),n*s))
  ow <- list()
  Xw <- list()
  Yw <- list()
  pmin_limit <- log(1e20)
  # XtX <- list()
  # XtY <- list()
  oem_holder <- list()
  
  if(penalty != "ols" & !grepl("grp.", penalty)) penalty <- paste0("grp.",penalty)
  
  if(length(penalty.factor) == 0 | missing(penalty.factor) | is.null(penalty.factor)) {
    penalty.factor <- rep(1, d)
  }
  penalty.factor <- penalty.factor * d /sum(penalty.factor)
  
  groups <- rep(1:d, s)
  # penalty.factor <- rep(penalty.factor, s)
  groups[penalty.factor == 0] <- 0
  
  if(length(lambda) == 0 | is.null(lambda)) {
    max.lambda <- max(sqrt(rowSums(crossprod(x,y)^2)))/(n*s)
    lambda <- max.lambda * exp( seq(0, log(lambda.min.ratio), length.out = nlambda))
  }
  
  beta <- beta_old <- lapply(1:length(lambda), function(l) rep(Inf, d * s))
  if(display.progress) pb <- utils::txtProgressBar(min = 0, max = length(lambda), style = 3)
  
  for(l in seq_along(lambda)) {
    lam <- lambda[l]
    for(i in 1:iter) {
      ow[[1]]  <- Matrix::Diagonal( n*s, sqrt(obs.weights[[1]]))
        # Matrix::sparseMatrix(i = 1:(n*s), j = 1:(n*s), x = obs.weights[[1]],
        #                               dims = c(n*s, n*s))
      Xw[[1]]  <- ow[[1]] %*% Xmat
      Yw[[1]]  <- ow[[1]] %*% Y
      # XtX[[1]] <- Matrix::crossprod( Xw[[1]], Xmat)
      # XtY[[1]] <- Matrix::crossprod(Xw[[1]], Y)
      oem_holder[[1]] <- oem::oem(x = Xw[[1]], y = Yw[[1]], family = "gaussian",
                                      penalty = penalty, lambda = lam,
                                      nlambda = 1,
                                      intercept = FALSE,
                                      gamma = gamma, alpha = alpha,
                                      tau = tau,
                                      standardize = FALSE,
                                      tol = tol, maxit = iter*5, groups = groups,
                                      group.weights = penalty.factor)
      beta[[l]] <-  c(oem_holder[[1]]$beta[[1]][-1])
      if(not.converged(beta[[l]], beta_old[[l]], tol)){
        beta_old[[l]] <- beta[[l]]
        obs.weights[[1]] <- obs_weight_update(Xmat,Y,beta[[l]],power,pmin_limit)
      } else {
        break
      }
      if(sum(beta[[l]] != 0) > model.size) break
      
    }
    if(display.progress) utils::setTxtProgressBar(pb, l)
  }
  
  output <- list()
  output$beta <- do.call("cbind", beta)
  output$penalty <- penalty
  output$lambda <- lambda
  output$nvars <- d
  output$call <- formals(lp_reg)
  output$call[names(this.call)] <- this.call
  output$nonzero_beta <- colSums(output$beta != 0)
  output$method <- "projection"
  output$power <- power
  class(output) <- c("WpProj", "optimization")
  
  extract <- extractTheta(output, matrix(0, d,s))
  output$nzero <- extract$nzero
  output$eta <- lapply(extract$theta, function(tt) x %*%tt )
  output$theta <- extract$theta
  
  return(output)
}
