#' p-Wasserstein Variable Importance
#'
#' @param X Covariates
#' @param eta Predictions from the estimated model
#' @param theta Parameters from the estimated model.
#' @param pred.fun A prediction function. must take variables x, theta as arguments: `pred.fun(x,theta)`
#' @param p Power of Wasserstein distance
#' @param ground_p Power of distance metric
#' @param transport.method Transport methods. See [transport_options()] for more details.
#' @param epsilon Hyperparameter for Sinkhorn iterations
#' @param OTmaxit Maximum number of iterations for the Wasserstein method
#' @param display.progress Display intermediate progress
#' @param parallel a foreach backend if already created
#'
#' @return Returns an integer vector ranking covariate importance from most to least important.
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This function will measure how much removing each covariate harms prediction accuracy. 
#' 
#' @export
#' 
#' @examples
#' n <- 128
#' p <- 10
#' s <- 99
#' x <- matrix(1, nrow = n, ncol = p )
#' beta <- (1:10)/10
#' y <- x %*% beta 
#' post_beta <- matrix(beta, nrow=p, ncol=s) 
#' post_mu <- x %*% post_beta
#' 
#' fit <-  WpProj(X=x, eta=post_mu, power = 2.0)
#' WPVI(X = x, eta = post_mu, theta = post_beta, transport.method = "hilbert")
WPVI <- function(X, eta, theta, pred.fun = NULL, p = 2, ground_p = 2,
                 transport.method = transport_options(),
                 epsilon = 0.05,
                 OTmaxit = 100,
                 display.progress = FALSE,
                 parallel = NULL) {
  this.call <- as.list(match.call()[-1])
  Y <- eta
  
  d <- ncol(X)
  n <- nrow(X)
  
  if(is.null(pred.fun) ) {
    pred.fun <- function(x,theta) {
      return(x %*% theta)
    }
    stopifnot(is.matrix(theta))
    if(ncol(theta) == ncol(X)) {
      theta <- t(theta)
    }
  }
  stopifnot(is.function(pred.fun))
  
  S <- ncol(theta)
  X_ <- t(X)
  if(is.null(Y)) {
    Y_ <- pred.fun(X, theta)
    same <- TRUE
  } else {
    if(nrow(Y) != n){
      Y_ <- t(Y)
    } else {
      Y_ <- Y
    }
    same <- FALSE
    if(all(Y_==pred.fun(X, theta))) same <- TRUE
  }
  transport.method <- match.arg(transport.method)
  
  if(!is.null(parallel)){
    if(!inherits(parallel, "cluster")) {
      stop("parallel must be a registered cluster backend or NULL")
    }
    doParallel::registerDoParallel(parallel)
    display.progress <- FALSE
  } else{
    foreach::registerDoSEQ()
  }
  i <- NULL
  wp <- foreach::foreach(i  = 1:d) %dorng% {
    x_temp <- X
    x_temp[,i] <- 0
    mu <- pred.fun(x_temp, theta)
    return(
      WpProj::wasserstein(mu, Y_, 
                  p = p, ground_p = ground_p, 
                  observation.orientation = "colwise",
                transport.method = transport.method, 
                epsilon = epsilon, niter = OTmaxit)
    )
  }
  orders <- order(unlist(wp), decreasing = TRUE)
  names(orders) <- colnames(X)
  return(orders)
}
