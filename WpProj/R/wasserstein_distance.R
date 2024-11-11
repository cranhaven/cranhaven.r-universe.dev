#' Calculate Wasserstein distances
#'
#' @param X Matrix for first group
#' @param Y Matrix for second group
#' @param p Power of the Wasserstein distance
#' @param ground_p Power of the distance metric. Usually same as `p`
#' @param observation.orientation Are observations unique by rows or columns? One of "colwise" or "rowwise"
#' @param method One of the outputs of [transport_options()]
#' @param ... additional options for sinkhorn based methods. `epsilon` and `niter` determining the hyperparameters for the negative entropy penalty
#'
#' @return A numeric value
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This function will calculate exact or approximate Wasserstein distances between two groups of observations. Please note that this function will likely be deprecated in favor of using the native function from the `approxOT` package.
#' @export
#' 
#' @examples
#' if(rlang::is_installed("stats")) {
#' n <- 128
#' p <- 10
#' x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#' y <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#' 
#' dist <- wasserstein(x,y, p = 2, ground_p = 1, observation.orientation = "rowwise",
#'             method = "hilbert") #fast
#' print(dist)
#' }
wasserstein <- function (X, Y, p = 2, ground_p = 2, observation.orientation = c("rowwise","colwise"), 
                         method = c("exact", "sinkhorn", "greenkhorn",
                                    # "randkhorn", "gandkhorn",
                                    "hilbert", "rank", #"sinkhorn2",
                                    "univariate.approximation", 
                                    "univariate.approximation.pwr","univariate"), ... ) {
  # lifecycle::deprecate_soft(when = "0.2.0", 
  #                           what = "wasserstein()", 
  #                           details = "Please use the native function from the `approxOT` package")
  
  obs <- match.arg(observation.orientation)
  method <- match.arg(method)
  
  if(missing(X)) stop("Must specify X")
  if(missing(Y)) stop("Must specify Y")
  
  if (!is.matrix(X)) {
    # warning("Attempting to coerce X to a matrix")
    X <- as.matrix(X)
    if(dim(X)[2] == 1 & obs == "colwise") X <- t(X)
  }
  if (!is.matrix(Y)) {
    # warning("Attempting to coerce Y to a matrix")
    Y <- as.matrix(Y)
    if(dim(Y)[2] == 1 & obs == "colwise") Y <- t(Y)
  }
  p <- as.double(p)
  ground_p <- as.double(ground_p)
  
  if(!(p >= 1)) stop("p must be >= 1")
  
  if(obs == "rowwise"){
    X <- t(X)
    Y <- t(Y)
    obs <- "colwise"
  }
  stopifnot(nrow(X) == nrow(Y))
  stopifnot(all(is.finite(X)))
  stopifnot(all(is.finite(Y)))
  
  if (method == "univariate.approximation" ) {
    loss <- wasserstein_p_iid_(X,Y, p)
  } else if ( method == "univariate.approximation.pwr") {
    loss <- wasserstein_p_iid_p_(X,Y, p)
  } else if (method == "univariate" | method == "hilbert" | method == "rank") {
    tp <- transport_plan(X = X, Y = Y, p = p, ground_p = ground_p,
                         observation.orientation = obs, method = method, ...)
    # loss <- c((((colSums(abs(X[, tp$tplan$from, drop = FALSE] - Y[, tp$tplan$to, drop=FALSE])^ground_p))^(1/ground_p))^p %*% tp$tplan$mass)^(1/p))
    loss <- tp$cost
  } else {
    n1 <- ncol(X)
    n2 <- ncol(Y)
    mass_x <- as.double(rep(1/n1, n1))
    mass_y <- as.double(rep(1/n2, n2))
    
    cost <- cost_calc(X, Y, ground_p)
    
    # if (method == "exact") {
      
      tplan <- transport_plan_given_C(mass_x, mass_y, p, cost, method, ...)
      loss <- wasserstein_(mass_ = tplan$mass, 
                           cost_ = cost, 
                           p = p, from_ = tplan$from, 
                           to_ = tplan$to)
      
    # } else if (method == "sinkhorn") {
    #   dots <- list(...)
    #   eps <- dots$eps
    #   niter <- dots$niter
    #   if (is.null(eps)) eps <- 0.05
    #   if (is.null(niter)) niter <- 100
    #   
    #   sink_out <- sinkhorn_distance(mass_x, mass_y, cost, p, eps, niter)
    #   loss <- sink_out$corrected
    # }
    
  }
  
  return(loss)
}

# wasserstein_post <- function(mod = NULL, idx= NULL, coefs = NULL, post = NULL, p = 2, n.param=NULL){
#   if(!is.null(mod)){
#     ml <- minLambda(mod)
#     idx <- ml$nzero
#     coefs <- ml$coefs
#   }
#   if(!is.matrix(coefs)) coefs <- as.matrix(coefs)
#   p_post <- transport::pp(post)
#   if(p < 1) stop("p must be greater or equal to 1")
#   
#   sink("temp_asterix1234.txt") #transport uses annoying Rprintf which can't be diverted normally
#   loss <- apply(coefs, 2, function(gamma) 
#     transport::wasserstein(transport::pp(post %*% diag(gamma)), 
#                            p_post, p=p, method="shortsimplex"))
#   sink()
#   file.remove("temp_asterix1234.txt")
#   
#   out <- rep(NA, n.param)
#   out[idx] <- loss^p
#   return(out)
# }

wasserstein_individual <- function(X,Y, ground_p, p, observation.orientation = c("colwise","rowwise")) {
  if(!is.matrix(X)) X <- as.matrix(X)
  if(!is.matrix(Y)) Y <- as.matrix(Y)
  obs <- match.arg(observation.orientation)
  if(obs == "rowwise"){
    X <- t(X)
    Y <- t(Y)
  }
  
  Xs <- apply(X,2,sort)
  Ys <- apply(Y,2,sort)
  
  loss <- colMeans((Xs - Ys)^ground_p)
  
  return(loss^(1/p))
  
}
