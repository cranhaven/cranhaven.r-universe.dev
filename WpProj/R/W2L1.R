#' 2-Wasserstein distance linear projections with an \eqn{L_1} penalty
#'
#' @param X An n x p matrix of covariates
#' @param Y An n x s matrix of predictions
#' @param theta optional parameter matrix for selection methods. Should be p x s.
#' @param penalty Form of penalty. One of "lasso", "ols", "mcp", "elastic.net","selection.lasso", "scad", "mcp.net", "scad.net", "grp.lasso", "grp.lasso.net", "grp.mcp","grp.scad", "grp.mcp.net", "grp.scad.net", "sparse.grp.lasso"
#' @param method "selection.variable" or "projection
#' @param transport.method Method for calculating the Wasserstein distance. One of "exact", "sinkhorn", "greenkhorn","hilbert" 
#' @param epsilon Penalty parameter for Sinkhorn and Greenkhorn and  optimal transport
#' @param OTmaxit Maximum iterations for the optimal transport iterations
#' @param model.size The maximum number of desired covariates. Defaults to the number of covariates.
#' @param lambda Penalty parameter for lasso regression. See \link[oem]{oem}.
#' @param nlambda Number of lambda values. See \link[oem]{oem}.
#' @param lambda.min.ratio Minimum lambda ratio for self selected lambda. See \link[oem]{oem}.
#' @param alpha elastic net mixing. See \link[oem]{oem}.
#' @param gamma tuning parameters for SCAD and MCP. See \link[oem]{oem}.
#' @param tau mixing parameter for sparse group lasso. See \link[oem]{oem}.
#' @param groups A vector of grouping values. See \link[oem]{oem}.
#' @param scale.factor Value to standardize the covariates by. Typically, is the standard deviation. Should have length 1 or length same as the number of covariates
#' @param penalty.factor Penalty factor for OEM. See \link[oem]{oem}.
#' @param group.weights Weights for group lasso. See \link[oem]{oem}.
#' @param maxit Max iteration for OEM. See \link[oem]{oem}.
#' @param tol Tolerance for OEM. See \link[oem]{oem}.
#' @param irls.maxit IRLS max iterations for OEM. See \link[oem]{oem}.
#' @param irls.tol IRLS tolerance for OEM. See \link[oem]{oem}.
#' @param infimum.maxit Maximum number of iterations alternating optimization and Wasserstein distance calculation. Irrelevant for projection method.
#' @param display.progress Display intermediate progress?
#'
#' @return Object of class `WpProj`
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
# # for selection method, must specify an OT method and theta
# fit.s <-  W2L1(X=x, Y=t(post_mu), theta = t(post_beta),
#              penalty = "lasso",
#              method = "selection.variable",
#              transport.method = "sinkhorn",
#              epsilon = 0.05, OTmaxit = 100
# )
# 
# # for projection method, no OT method or theta need be specified
# fit.p <-  W2L1(X=x, Y=t(post_mu),
#              penalty = "lasso",
#              method = "projection"
# )
# }
W2L1 <- function(X, Y=NULL, theta = NULL, 
                 penalty =  c("lasso", "ols", "mcp", "elastic.net", 
                              "selection.lasso",
                              "scad", "mcp.net", 
                              "scad.net", 
                              "grp.lasso", 
                              "grp.lasso.net", "grp.mcp",
                              "grp.scad", "grp.mcp.net",
                              "grp.scad.net",
                              "sparse.grp.lasso"), 
                 method = c("projection","selection.variable","location.scale","scale"),
                 transport.method = transport_options(),
                 epsilon = 0.05,
                 OTmaxit = 100,
                 model.size = NULL,
                 lambda = numeric(0), 
                 nlambda = 100L, 
                 lambda.min.ratio = NULL, alpha = 1, 
                 gamma = 1, tau = 0.5, 
                 groups = numeric(0), 
                 scale.factor = numeric(0), 
                 penalty.factor = NULL, 
                 group.weights = NULL, maxit = 500L, 
                 tol = 1e-07, irls.maxit = 100L, 
                 irls.tol = 0.001,
                 # pseudo_observations = 0.0,
                 infimum.maxit=NULL,
                 display.progress=FALSE) 
{
  this.call <- as.list(match.call()[-1])
  
  family <- "gaussian"
  # family <- match.arg(family)
  if ("penalty" %in% names(this.call)) {
    penalty <- match.arg(penalty, several.ok = TRUE)
  }
  else {
    penalty <- match.arg(penalty, several.ok = FALSE)
  }
  if(is.null(method)){
    method <- "projection"
  }
  method <- match.arg(method)
  
  # make sure X is a matrix
  if(!is.matrix(X)) X <- as.matrix(X)
  
  # confirm dims
  dims <- dim(X)
  n_obs <- dims[1L]
  p <- dims[2L]
  
  # make sure isn't a sparse matrix
  if (inherits(X, "sparseMatrix")) {
    stop("Sparse matrices not allowed at this time")
  }
  
  #transpose X so covariates are by row
  X_ <- t(X)
  
  # is Y the same as X*theta ?
  same <- FALSE # default is false
  
  if(!is.null(theta)) {
    
    # make sure theta is a matrix
    if(!is.matrix(theta)) theta <- as.matrix(theta)
    
    # transpose theta if covariates are not by row
    if(ncol(theta) == p){
      theta_ <- t(theta)
    } else {
      theta_ <- theta
    }
    
    # create Y if needed
    if (is.null(Y) ) {
      same <- TRUE
      Y <- crossprod(X_,theta_)
    }
    
    
  } else if (is.null(Y)) {
    stop("Must specify Y or theta if method == 'projection'. If method != 'projection' you must always specify theta. In the latter case, Y is optional")
  }
  
  if(!any(dim(Y) %in% dim(X_))) stop("Number of observations of Y must match X")
  if(!is.matrix(Y)) Y <- as.matrix(Y)
  
  # properly orient Y
  if (nrow(Y) == n_obs) { 
    Y_ <- Y
  } else if (ncol(Y) == n_obs) {
    Y_ <- t(Y)
  } else {
    stop("The number of observations of Y do not match X")
  }
  
  if(method != "projection") if(all(Y_==crossprod(X_, theta_))) same <- TRUE
  
  # create dummy theta for projection method
  if (!is.null(Y_) && method == "projection") {
    theta_ <- matrix(1, nrow = p, ncol = ncol(Y_))
  }
  theta_save <- theta_
  
  if (method != "projection") {
    if(nrow(theta_) != p) stop("dimensions of theta must match X")
    if(ncol(Y_) != ncol(theta_)) stop("ncol of Y should be same as ncol of theta")
  }
  if(nrow(Y_) != n_obs) stop("The number of observations in Y and X don't line up. Make sure X is input with observations in rows.")
  
  
  # xty <- drop(xty)
  # if (p != NROW(xty))
  #   stop("xty must have length equal to the number of columns and rows of xtx. do NOT provide response vector")
  if (family == "binomial")
    stop("binomial not implemented yet")
  if (is.null(penalty.factor)) {
    penalty.factor <- rep(1, p)
  }
  varnames <- colnames(X)
  if (is.null(varnames))
    varnames = paste("V", seq(p), sep = "")
  if (length(penalty.factor) != p) {
    stop("penalty.factor must have same length as number of columns in x")
  }
  if ( any(penalty.factor < 0) ) {
    penalty.factor <- abs( penalty.factor )
    warning("penalty.factor had negative values. These were turned to positive values via abs().")
  }
  penalty.factor <- penalty.factor * p/sum(penalty.factor)
  penalty.factor <- drop(penalty.factor)
  if (any(grep("grp", penalty) > 0) & method !="location.scale") {
    if (length(groups) != p) {
      stop("groups must have same length as number of columns in x")
    }
    unique.groups <- sort(unique(groups))
    zero.idx <- unique.groups[which(unique.groups == 0)]
    groups <- drop(groups)
    if (length(group.weights)!=0) {
      if (length(zero.idx) > 0) {
        group.weights[zero.idx] <- 0
      }
      group.weights <- drop(group.weights)
      if (length(group.weights) != length(unique.groups)) {
        stop("group.weights must have same length as the number of groups")
      }
      group.weights <- as.numeric(group.weights)
    } else {
      group.weights <- numeric(0)
    }
  } else if ( method != "location.scale" ) {
    unique.groups <- numeric(0)
    group.weights <- numeric(0)
  }
  
  if (is.null(lambda.min.ratio)) {
    lambda.min.ratio <- 1e-04
  } else {
    lambda.min.ratio <- as.numeric(lambda.min.ratio)
  }
  
  if (lambda.min.ratio >= 1 | lambda.min.ratio <= 0) {
    stop("lambda.min.ratio must be between 0 and 1")
  }
  
  if (nlambda[1] <= 0) {
    stop("nlambda must be a positive integer")
  }
  
  if (!is.list(lambda)) {
    lambda <- sort(as.numeric(lambda), decreasing = TRUE)
    if (length(lambda) > 0) {
      lambda <- as.double(lambda)
    }
    lambda <- rep(list(lambda), length(penalty))
  } 
  else {
    if (length(lambda) != length(penalty)) {
      stop("If list of lambda vectors is provided, it must be the same length as the number of penalties fit")
    }
    nlambda.tmp <- length(lambda[[1]])
    for (l in 1:length(lambda)) {
      if (is.null(lambda[[l]]) || length(lambda[[l]]) <
          1) {
        stop("Provided lambda vector must have at least one value")
      }
      if (length(lambda[[l]]) != nlambda.tmp) {
        stop("All provided lambda vectors must have same length")
      }
      lambda[[l]] <- as.double(sort(as.numeric(lambda[[l]]),
                                    decreasing = TRUE))
    }
  }
  if (is.null(model.size)) {
    model.size <- p
  }
  infm.maxit <- infimum.maxit
  if(is.null(infm.maxit)){
    infm.maxit <-maxit
  }
  
  orig.method <- method
  # if(method == "selection.variable" & penalty != "selection.lasso"){
  #   penalty <- "selection.lasso"
  #   warning("Penalty changed to 'selection.lasso' since 'selection.variable' method was chosen.")
  # }
  if(is.null(transport.method)){
    transport.method <- "exact"
  } else {
    transport.method <- match.arg(transport.method, transport_options())
  }
  if(nrow(X) == 1) {
    transport.method <- "univariate.approximation.pwr"
  }
  
  
  rmv.idx <- NULL
  if(any(apply(theta_,1, function(x) all(x == 0)))) {
    rmv.idx <- which(apply(theta_,1, function(x) all(x == 0)))
    
    X_ <- X_[-rmv.idx, ,drop=FALSE]
    theta_ <- theta_[-rmv.idx,, drop = FALSE]
    penalty.factor <- penalty.factor[-rmv.idx]
    warning("Some dimensions of 'theta' have no variation. These have been removed")
  }
  
  if ( method == "projection") {
    # if(any(grep("grp", penalty) > 0)) stop("don't specify a group penalty")
    if(penalty == "selection.lasso") penalty <- "lasso"
    if(penalty != "ols") penalty <- paste0("projection.",penalty)
    if( infm.maxit != 1){
      infm.maxit <- 1
      # warning("Infimum iterations set to 1 for projection method.")
    }
  }
  if ( method == "location.scale" ) {
    if (!any(grep("grp", penalty) > 0)) penalty <- paste0("grp.",penalty)
    if (length(groups)>0) {
      unique.groups <- sort(unique(groups))
      zero.idx <- unique.groups[which(unique.groups == 0)]
      groups <- drop(groups)
    } else {
      unique.groups <- 1:p
      groups <- rep(unique.groups,2)
      zero.idx <- which(penalty == 0)
    }
    if ( length( group.weights ) > 0 ) {
      #   group.weights <- penalty.factor
      # } else {
      if (length(zero.idx) > 0) {
        group.weights[zero.idx] <- 0
      }
      group.weights <- drop(group.weights)
      if (length(group.weights) != length(unique.groups)) {
        stop("group.weights must have same length as the number of groups (2 times ncol X)")
      }
      group.weights <- as.numeric(group.weights)
    } else {
      group.weights <- penalty.factor
      if (length(group.weights) != length(unique.groups)) {
        stop("group.weights or penalty.factor must have same length as the number of groups (ncol X)")
      }
    }
    if ( length( penalty.factor ) != 2 * p ) penalty.factor <- rep( penalty.factor, 2 )
    if ( length( penalty.factor ) != 2 * p ) stop( "penalty.factor must have same length as ncol(X)" )
    if(!any(grep("sparse", penalty) > 0)) penalty.factor <- rep(1, length(penalty.factor))
    
    X_ <- rbind(X_,X_)
    m_theta <- matrix(rowMeans(theta_), p, ncol(theta_))
    c_theta <- theta_ - m_theta
    theta_ <- rbind(c_theta,m_theta)
    method <- "scale"
  }
  
  if(is.null(epsilon)) {
    epsilon <- 0.05
  }
  if(is.null(OTmaxit)) {
    OTmaxit <- 100
  }
  # pseudo_observations <- 0.0
  
  #make R types align with c types
  groups <- as.integer(groups)
  unique.groups <- as.integer(unique.groups)
  nlambda <- as.integer(nlambda)
  alpha <- as.double(alpha)
  gamma <- as.double(gamma)
  tau <- as.double(tau)
  tol <- as.double(tol)
  irls.tol <- as.double(irls.tol)
  irls.maxit <- as.integer(irls.maxit)
  maxit <- as.integer(maxit)
  # pseudo_observations <- as.double(pseudo_observations)
  infm.maxit <- as.integer(infm.maxit)
  display.progress <- as.logical(display.progress)
  method <- as.character(method)
  model.size <- as.integer(model.size)
  not_same <- as.logical(!same)
  epsilon <- as.double(epsilon)
  OTmaxit <- as.integer(OTmaxit)
  
  if (length(scale.factor) > 0) {
    if (length(scale.factor) != p)
      stop("scale.factor must be same length as xty (nvars)")
    scale.factor <- as.double(scale.factor)
  }
  
  if (maxit <= 0 | irls.maxit <= 0 | infm.maxit <=0) {
    stop("maxit, irls.maxit, and infm.maxit should be greater than 0")
  }
  
  if (tol < 0 | irls.tol < 0) {
    stop("tol and irls.tol should be nonnegative")
  }
  
  options <- list(maxit = maxit, tol = tol, irls_maxit = irls.maxit,
                  irls_tol = irls.tol, infm_maxit = infm.maxit,
                  display_progress = display.progress, 
                  method = method,
                  transport_method = transport.method,
                  model_size = model.size,
                  not_same = not_same,
                  epsilon = epsilon,
                  OTmaxit = OTmaxit)
  
  
  # if (method == "projection" & (penalty == "projection.lasso") | (penalty == "projection.lasso.net")) {
  #   lambdas <- lambda[[1]]
  #   xglmnet <- t(X_)
  #   yglmnet <- t(Y_)
  #   if(length(lambdas)==0) lambdas <- NULL
  #   dimnames(yglmnet)[[1]] <- dimnames(xglmnet)[[1]]
  #   dimnames(yglmnet)[[2]] <- paste0("sample_",1:ncol(yglmnet))
  #   output <- glmnet::glmnet(xglmnet, yglmnet, family="mgaussian", 
  #                            lambda.min.ratio = lambda.min.ratio, 
  #                            nlambda=nlambda, alpha = alpha, lambda=lambdas, 
  #                            penalty.factor = penalty.factor, intercept = FALSE,
  #                            standardize.response = FALSE)
  #   beta_list <- lapply(output$beta, function(bb) data.frame(as.matrix(bb)))
  #   beta <- data.table::rbindlist(beta_list)
  #   output$beta <- beta
  #   output$niter <- matrix(0, nrow=1,ncol=length(output$lambda))
  #   output$innerIter <- rep(1, length(output$lambda))
  # } else {
  # cat("\n")
  output <- W2penalized(X_,Y_, theta_, family, 
                        penalty, groups, unique.groups, group.weights, 
                        lambda, nlambda, lambda.min.ratio, alpha, gamma, tau, 
                        scale.factor, penalty.factor, options)
  # }
  
  if (any(output$innerIter == infm.maxit) & infm.maxit>1) warning("Maximum iterations hit when optimizing 2-Wasserstein distance over possible infimums. Increase infimum.maxit to ensure have a local minimum.", call. = FALSE)
  if (any(output$niter == maxit)) warning("Maximum iterations hit when optimizing parameters. Consider increasing maxit.", call. = FALSE)
  if(penalty != "ols") {
    if (not_same)  {
      options_ols <- options
      options_ols$display_progress <- FALSE
      if(method == "selection.variable"){
        penalty_ols <- penalty
      } else {
        penalty_ols <- "ols"
      }
      options_ols$infm_maxit <- 1
      options_ols$model_size <- p
      ols.out <- W2penalized(X_,Y_, theta_, family, 
                          penalty_ols, groups, unique.groups, group.weights,
                          list(as.double(0)), as.integer(1), lambda.min.ratio, alpha, gamma, tau, 
                          scale.factor, penalty.factor, options_ols)[c("beta","niter")]
      output$niter <- cbind(output$niter, 0)
      output$niter[1,ncol(output$niter)] <- ols.out$niter[1]
      output$innerIter <- c(output$innerIter,1)
      output$lambda <- c(output$lambda, 0)
      output$beta <- cbind(output$beta, ols.out$beta)
      rm(ols.out)
    } else if (same & method == "selection.variable") {
      output$lambda <- c(output$lambda, 0)
      output$beta <- cbind(output$beta, rep(1, nrow(output$beta)))
      output$niter <- cbind(output$niter, 0)
      output$innerIter <- c(output$innerIter,0)
    } else if (same & method == "projection") {
      output$lambda <- c(output$lambda, 0)
      output$beta <- cbind(output$beta, c(theta))
      output$niter <- cbind(output$niter, 0)
      output$innerIter <- c(output$innerIter,0)
    }
  }
  
  output$nvars <- p
  output$maxit <- maxit
  output$power <- 2.0
  output$penalty <- penalty
  # output$family <- family
  output$varnames <- varnames
  output$call <- formals(W2L1)
  output$call[names(this.call)] <- this.call
  output$method <- orig.method
  output$remove.idx <- rmv.idx
  output$nonzero_beta <- colSums(output$beta != 0)
  # output$nzero <- nz
  class(output) <- c("WpProj", "optimization")
  extract <- extractTheta(output, theta_)
  output$nzero <- extract$nzero
  output$eta <- lapply(extract$theta, function(tt) crossprod(X_, tt))
  output$theta <- extract$theta
  if(!is.null(rmv.idx)) {
    for(i in seq_along(output$theta)){
      output$theta[[i]] <- theta_save
      output$theta[[i]][-rmv.idx,] <- extract$theta[[i]]
    }
  }
  
  return(output)
  
}
