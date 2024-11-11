#' @name HC
#' 
#' @title Run the Hahn-Carvalho Method
#' 
#' @description 
#' `r lifecycle::badge("experimental")` 
#' Runs the Hahn-Carvalho method but adapted to return full distributions.
#' 
#' @param X Covariates
#' @param Y Predictions
#' @param theta Parameters
#' @param family Family for method. See \link[oem]{oem}.
#' @param penalty Penalty function. See \link[oem]{oem}.
#' @param method Should we run a selection variable methodology or projection?
#' @param lambda lambda for lasso. See \link[oem]{oem} for this and all options below
#' @param nlambda Number of lambda values.
#' @param lambda.min.ratio Minimum lambda ratio for self selected lambda
#' @param alpha elastic net mixing.
#' @param gamma tuning parameters for SCAD and MCP
#' @param tau mixing parameter for sparse group lasso
#' @param groups A vector of grouping values
#' @param penalty.factor Penalty factor for OEM.
#' @param group.weights Weights for groupped lasso
#' @param maxit Max iteration for OEM
#' @param tol Tolerance for OEM
#' @param irls.maxit IRLS max iterations for OEM
#' @param irls.tol IRLS tolerance for OEM
#' 
#' @return a `WpProj` object with selected covariates and their values
#' 
#' @references Hahn, P. Richard and Carlos M. Carvalho. (2014) "Decoupling Shrinkage and Selection in Bayesian Linear Models: A Posterior Summary Perspective." <https://arxiv.org/pdf/1408.0464.pdf>
#' 
#' @export
#' 
#' @examples
#' n <- 32
#' p <- 10
#' s <- 99
#' x <- matrix( 1, nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta
#' post_beta <- matrix(beta, nrow=p, ncol=s) 
#' post_mu <- x %*% post_beta
#' 
#' fit <-  HC(X=x, Y=post_mu, theta = post_beta,
#'                penalty = "lasso", 
#'                method = "projection"
#' )
HC <- function(X, Y=NULL, theta, family="gaussian", 
               penalty =  c("elastic.net", "selection.lasso",
                            "lasso", "ols", "mcp",
                            "scad", "mcp.net", 
                            "scad.net", 
                            "grp.lasso", 
                            "grp.lasso.net", "grp.mcp",
                            "grp.scad", "grp.mcp.net",
                            "grp.scad.net", 
                            "sparse.grp.lasso"), 
               method = c("selection.variable", "projection"),
               lambda = numeric(0), 
               nlambda = 100L, 
               lambda.min.ratio = NULL, alpha = 1,  
               gamma = 1, tau = 0.5, 
               groups = numeric(0), 
               # scale.factor = numeric(0), 
               penalty.factor = NULL, 
               group.weights = NULL, maxit = 500L, 
               tol = 1e-07, irls.maxit = 100L, 
               irls.tol = 0.001
               )
{
  if( family == "cox" | family == "exponential" | family == "survival") {
    if(!(penalty == "lasso")) {
      warning("penalty must be lasso for survival method")
      penalty <- "lasso"
    }
  }
  method <- match.arg(method)
  intercept <- FALSE
  
  p <- ncol(X)
  n <- nrow(X)
  
  if(ncol(X) == ncol(theta)) {
    theta <- t(theta)
  }
  s <- ncol(theta)
  
  if(is.null(Y)) {
    Y <- X %*% theta
    if ( family == "binomial") {
      Y_ <- c(rep(1,n), rep(0,n))
    } else if (family == "gaussian" ) {
      Y_ <- rowMeans(Y)
    } else if (family == "exponential" | family == "survival" | family == "cox") {
      Y_ <- NULL # need to implement survival times
    }
  } else {
    stopifnot(nrow(Y) == nrow(X))
    stopifnot(ncol(Y) == ncol(theta))
      Y_ <- rowMeans(Y)
  }
  if(family == "binomial") {
    prob <- rowMeans(stats::plogis(X %*% theta))
    weights <- c(prob, 1-prob)
  } else {
    weights <- numeric(0)
  }
  
  hessian.type <- "upper.bound"
  if(family == "binomial"){
    if(n > 100*p){
      hessian.type <- "upper.bound"
    } else {
      hessian.type <- "full"
    }
  }
  if(all(X[,1]==1) & intercept == TRUE) {
    x <- X[,-1]
    if(length(penalty.factor) == ncol(X)) penalty.factor <- penalty.factor[-1]
  } else {
    x <- X
  }
  
  if( family %in% c("gaussian")) {
    
   output <-  oem::oem(X, Y_, family = family, penalty = penalty,
                  weights = weights, lambda = lambda, nlambda = nlambda,
                  lambda.min.ratio = lambda.min.ratio, alpha=alpha, gamma = gamma, tau = tau,
                  groups = groups, penalty.factor = penalty.factor, group.weights = group.weights,
                  standardize = TRUE, intercept = FALSE, 
                  maxit = maxit, tol = tol, irls.maxit = irls.maxit,
                  irls.tol = irls.tol, accelerate = FALSE, ncores = -1, 
                  compute.loss = FALSE, hessian.type = hessian.type)
  
   } else if (family == "exponential" | family == "survival" | family == "cox") {
    
    output <-  glmnet::glmnet(x, Y_, family = "cox", penalty = penalty,
             weights = weights, lambda = lambda, nlambda = nlambda,
             lambda.min.ratio = lambda.min.ratio, alpha=alpha, 
             thresh = 1e-07, dfmax = ncol(X) + 1,
             penalty.factor = penalty.factor,
             standardize.response = TRUE, intercept = intercept)
   } else if (family == "binomial") {
     
     output <-  glmnet::glmnet(x, Y_, family = "binomial", penalty = penalty,
             weights = weights, lambda = lambda, nlambda = nlambda,
             lambda.min.ratio = lambda.min.ratio, alpha=alpha, 
             thresh = 1e-07, dfmax = ncol(X) + 1,
             penalty.factor = penalty.factor,
             standardize.response = TRUE, intercept = intercept)
  }
  if(!intercept) {
    output$beta[[1]] <- output$beta[[1]][-1,]
  }
  extract <- extractCoef(output)
  output$nzero <- c(extract$nzero,p)
  output$E_theta <- extract$coefs
  if (intercept) {
    output$E_eta <- lapply( 1:ncol(output$E_theta), function(et) x %*% output$E_theta[-1,et] + output$E_theta[1,et])
  } else {
    output$E_eta <- lapply( 1:ncol(output$E_theta), function(et) x %*% output$E_theta[,et])
  }
  
  if(intercept) {
      xtx <- crossprod(cbind(1,X))
      xty <- crossprod(cbind(1,X),Y)
    } else {
      xtx <- crossprod(X)
      xty <- crossprod(X,Y)
    }
  
  output$theta <- lapply(1:ncol(output$E_theta), function(i){
    et <- output$E_theta[,i]
    coefs <- matrix(0, p, s) 
    idx <- which(et != 0)
    if(length(idx) == 0) return(coefs)
    if ( method == "selection.variable" ) {
      coefs[idx,] <- theta[idx,]
    } else {
      coefs <- calc.beta(xtx = xtx, xty = xty, 
                         active.idx = idx, method = "projection",
                          OToptions = NULL)
    }
    return(coefs)
  })
  output$theta[[length(output$theta)+1]] <- theta
  if (intercept) {
    output$eta <- lapply(output$theta, function(tt) x %*% tt[-1,,drop = FALSE] + matrix(tt[1,,drop = FALSE], nrow=n, ncol=s))
    } else {
      output$eta <- lapply(output$theta, function(tt) x %*% tt)
    }
  class(output) <- c("WpProj", "HC")
  
  return(output)
}
