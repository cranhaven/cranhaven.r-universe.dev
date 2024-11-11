#' Calculate the Wasserstein distance
#'
#' @param X The covariate data of the first sample.
#' @param Y The covariate data of the second sample.
#' @param a Optional. Empirical measure of the first sample
#' @param b Optional. Empirical measure of the second sample
#' @param cost Specify the cost matrix in advance.
#' @param tplan Give a transportation plan with slots "from", "to", and "mass", like that returned by the [tranportation_plan()] function.
#' @param p The power of the Wasserstein distance
#' @param ground_p The power of the Lp norm
#' @param method Which transportation method to use. See [transport_options()]
#' @param cost_a The cost matrix for the first sample with itself. Only used for unbiased Sinkhorn
#' @param cost_b The cost matrix for the second sample with itself. Only used for unbiased Sinkhorn
#' @param ... Additional arguments for various methods:
#' \itemize{
#' \item"niter": The number of iterations to use for the entropically penalized optimal transport distances
#' \item"epsilon": The multiple of the median cost to use as a penalty in the entropically penalized optimal transport distances
#' \item"unbiased": If using Sinkhorn distances, should the distance be de-biased? (TRUE/FALSE)
#' \item"nboot": If using sliced Wasserstein distances, specify the number of Monte Carlo samples
#' }
#'
#' @return The p-Wasserstein distance, a numeric value
#' @export
#'
#' @examples
#' set.seed(11289374)
#' n <- 100
#' z <- stats::rnorm(n)
#' w <- stats::rnorm(n)
#' uni <- approxOT::wasserstein(X = z, Y = w, 
#' p = 2, ground_p = 2, 
#' observation.orientation = "colwise", 
#' method = "univariate")
wasserstein <- function(X = NULL, Y = NULL, a= NULL, b = NULL, cost = NULL, tplan = NULL, 
                        p = 2, ground_p = 2, 
                        method = transport_options(), 
                        cost_a = NULL, cost_b = NULL, ... ) {
  
  if (is.null(method)) method <- "networkflow"
  method <- match.arg(method)
  p <- as.double(p)
  
  if (!(p >= 1)) stop("p must be >= 1")
  
  
  if (is.null(cost) & is.null(tplan)) {
    
    args <- list(X = X, Y = Y, a = a, b = b, p = p, ground_p = ground_p,  
                 method = method, ... )
    args <- args[!duplicated(names(args))]
    argn <- lapply(names(args), as.name)
    f.call <- as.call(stats::setNames(c(as.name("wasserstein_calc_cost"), argn), c("", names(args))))
    
    loss <- eval(f.call, envir = args)
    
  } else if (is.null(tplan)) {
    
    if (is.null(ground_p)) ground_p <- p
    
    n1 <- nrow(cost)
    n2 <- ncol(cost)
    
    if (missing(a) || is.null(a)) {
      warning("assuming all points in first group have equal mass")
      a <- as.double(rep(1/n1, n1))
    }
    if (missing(b) || is.null(b)) {
      warning("assuming all points in first group have equal mass")
      b <- as.double(rep(1/n2, n2))
    }
    
    nzero_a <- a != 0
    nzero_b <- b != 0
    
    mass_x <- a[nzero_a]
    mass_y <- b[nzero_b]
    
    cost <- cost[nzero_a, nzero_b, drop = FALSE]
    
    if (method == "sinkhorn" && isTRUE(list(...)$unbiased) ) {
      cost_a <- cost_a[nzero_a, nzero_a, drop = FALSE]
      cost_b <- cost_b[nzero_b, nzero_b, drop = FALSE]
      
      if(is.null(cost_a) || is.null(cost_b)) stop("Must specify cost matrices for both separate groups for unbiased sinkhorn.")
      
      pot <- sinkhorn_pot(mass_x = mass_x, mass_y = mass_y, p = p, 
                          cost = cost, cost_a = cost_a, cost_b = cost_b, ...)
      
      return((sum(mass_x * pot$f) + sum(mass_y * pot$g))^(1/p))
      
    }
    
    tplan <- transport_plan_given_C(mass_x, mass_y, p, cost, method, cost_a, cost_b, ...)
    loss  <- wasserstein_(mass_ = tplan$mass, cost_ = cost, p = p, from_ = tplan$from, to_ = tplan$to)
    
  } else {
    loss <- wasserstein_(mass_ = tplan$mass, cost_ = cost, p = p, from_ = tplan$from, to_ = tplan$to)
    nzero_a <- rep(TRUE, nrow(cost))
    nzero_b <- rep(TRUE, ncol(cost))
  }
  
  # if (isTRUE(list(...)$unbiased) && !(method == "networkflow" | method == "shortsimplex") ) {
  #   dots <- list(...)
  #   eps <- dots$epsilon
  #   if (is.null(eps)) eps <- 0.05
  #   
  #   
  #   
  #   # if (is.null(cost) & is.null(tplan)) {
  #   #   if ( isTRUE(dots$observation.orientation == "rowwise") ) {
  #   #     n1  <- nrow(X)
  #   #     n2  <- nrow(Y)
  #   #   } else if ( isTRUE(dots$observation.orientation == "colwise")) {
  #   #     n1  <- ncol(X)
  #   #     n2  <- ncol(Y)
  #   #   }
  #   #   if (missing(a) || is.null(a)) {
  #   #     a <- as.double(rep(1/n1, n1))
  #   #   }
  #   #   if (missing(b) || is.null(b)) {
  #   #     b <- as.double(rep(1/n2, n2))
  #   #   }
  #   #   
  #   #   nzero_a <- a != 0
  #   #   nzero_b <- b != 0
  #   #   mass_x <- a[nzero_a]
  #   #   mass_y <- b[nzero_b]
  #   #   
  #   # }
  #  
  #     
  #   if (is.null(cost) || is.null(dots$cost_a) || is.null(dots$cost_b)) {
  #     obs <- match.arg(dots$observation.orientation, c("rowwise","colwise"))
  #     if (obs == "rowwise") {
  #       X <- t(X)
  #       Y <- t(Y)
  #     }
  #   }
  #   if (is.null(cost) ) {
  #     cost   <- cost_calc(X, Y, ground_p)
  #     
  #     n1 <- nrow(cost)
  #     n2 <- ncol(cost)
  #     
  #     if (missing(a) || is.null(a)) {
  #       warning("assuming all points in first group have equal mass")
  #       a <- as.double(rep(1/n1, n1))
  #     }
  #     if (missing(b) || is.null(b)) {
  #       warning("assuming all points in first group have equal mass")
  #       b <- as.double(rep(1/n2, n2))
  #     }
  #     
  #     nzero_a <- a != 0
  #     nzero_b <- b != 0
  #     
  #     mass_x <- a[nzero_a]
  #     mass_y <- b[nzero_b]
  #     
  #     cost <- cost[nzero_a, nzero_b, drop = FALSE]
  #   }
  #   
  #   if (is.null(dots$cost_a) && is.null(dots$cost_b)) {
  #     
  #     # if ( isTRUE(dots$observation.orientation == "rowwise") ) {
  #     #   X <- X[nzero_a,,drop = FALSE]
  #     #   Y <- Y[nzero_b,,drop = FALSE]
  #     # } else if ( isTRUE(dots$observation.orientation == "colwise")) {
  #     #   X <- X[,nzero_a,drop = FALSE]
  #     #   Y <- Y[,nzero_b,drop = FALSE]
  #     # }
  #    
  #     
  #     cost_a <- cost_calc(X, X, ground_p)
  #     cost_b <- cost_calc(Y, Y, ground_p)
  #     
  #     cost_a <- cost_a[nzero_a, nzero_a, drop = FALSE]
  #     cost_b <- cost_b[nzero_b, nzero_b, drop = FALSE]
  #     # args <- list(X = X, Y = X, a = a, b = a, p = p, ground_p = ground_p, 
  #     #              method = method, ... )
  #     # args <- args[!duplicated(names(args))]
  #     # argn <- lapply(names(args), as.name)
  #     # f.call <- as.call(stats::setNames(c(as.name("wasserstein_calc_cost"), argn), c("", names(args))))
  #     # loss_a <- eval(f.call, args)
  #     # args$Y <- args$X <- Y
  #     # args$a <- args$b <- b
  #     # 
  #     # loss_b <- eval(f.call, args)
  #   } else {
  #     
  #     cost_a <- dots$cost_a[nzero_a, nzero_a, drop = FALSE]
  #     cost_b <- dots$cost_b[nzero_b, nzero_b, drop = FALSE]
  #     
  #     
  #   }
  #   lambda <- 1 / (median(cost) * eps)
  #   
  #   eps_a  <- 1 / (median(cost_a) * lambda)
  #   eps_b  <- 1 / (median(cost_b) * lambda)
  #   
  #   args.a  <- list(mass_x = mass_x, 
  #                   mass_y = mass_x, 
  #                   p = p, 
  #                   cost = cost_a, 
  #                   method = method, 
  #                   epsilon = eps_a,
  #                   ...)
  #   args.a  <- args.a[!duplicated(names(args.a))]
  #   n.a <-   lapply(names(args.a), as.name)
  #   names(n.a) <- names(args.a)
  #   f.call    <- as.call(c(as.name("transport_plan_given_C"), n.a))
  #   
  #   tplana <- eval(f.call, envir = args.a)
  #   
  #   args.b <- args.a
  #   args.b$mass_x <- args.b$mass_y <- mass_y
  #   args.b$cost <- cost_b
  #   args.b$epsilon <- eps_b
  #   
  #   tplanb <- eval(f.call, envir = args.b)
  #   
  #   loss_a <- wasserstein_(mass_ = tplana$mass, cost_ = cost_a, p = p, from_ = tplana$from, to_ = tplana$to)
  #   loss_b <- wasserstein_(mass_ = tplanb$mass, cost_ = cost_b, p = p, from_ = tplanb$from, to_ = tplanb$to)
  #   
  #   loss_p <- (loss^p - 0.5 * loss_a^p - 0.5 * loss_b^p)
  #   loss <- (loss_p * as.numeric(loss_p > 0))^(1/p)
  # }
  return(loss)
  
}

wasserstein_calc_cost <- function(X, Y, a = NULL, b = NULL, p = 2, ground_p = 2, observation.orientation = c("rowwise","colwise"), 
                         method = transport_options(), ... ) {
  obs <- match.arg(observation.orientation,  c("colwise","rowwise"))
  method <- match.arg(method)
  
  if (missing(X)) stop("Must specify X")
  if (missing(Y)) stop("Must specify Y")
  
  if (!is.matrix(X)) {
    # warning("Attempting to coerce X to a matrix")
    X <- as.matrix(X)
    if (dim(X)[2] == 1 & obs == "colwise") X <- t(X)
  }
  if (!is.matrix(Y)) {
    # warning("Attempting to coerce Y to a matrix")
    Y <- as.matrix(Y)
    if (dim(Y)[2] == 1 & obs == "colwise") Y <- t(Y)
  }
  p <- as.double(p)
  ground_p <- as.double(ground_p)
  
  if (!(p >= 1)) stop("p must be >= 1")
  
  if (obs == "rowwise") {
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
    tp <- transport_plan(X = X, Y = Y, a = a, b = b, p = p, ground_p = ground_p,
                         observation.orientation = obs, method = method, ...)
    # loss <- c((((colSums(abs(X[, tp$tplan$from, drop = FALSE] - Y[, tp$tplan$to, drop=FALSE])^ground_p))^(1/ground_p))^p %*% tp$tplan$mass)^(1/p))
    loss <- tp$cost
  } else if (method == "sliced") {
    
    n1 <- ncol(X)
    n2 <- ncol(Y)
    
    if (missing(a) || is.null(a)) {
      # warning("assuming all points in first group have equal mass")
      a <- as.double(rep(1/n1, n1))
    }
    if (missing(b) || is.null(b)) {
      # warning("assuming all points in first group have equal mass")
      b <- as.double(rep(1/n2, n2))
    }
    
    nzero_a <- a != 0
    nzero_b <- b != 0
    
    a <- a[nzero_a]
    b <- b[nzero_b]
    
    X <- X[, nzero_a, drop = FALSE]
    Y <- Y[, nzero_b, drop = FALSE]
    
    dots <- list(...)
    tplan <- NULL
    nboot <- dots$nsim
    d     <- nrow(X)
    theta <- matrix(stats::rnorm(d * nboot), d, nboot)
    theta <- sweep(theta, 2, STATS = apply(theta,2,function(x) sqrt(sum(x^2))), FUN = "/")
    X_theta <- crossprod(x = X, y = theta)
    Y_theta <- crossprod(x = Y, y = theta)
    # u     <- sort(runif(nboot))
    costs <- sapply(1:nboot, function(i) {
      # x <- quantile(c(X_theta[,i]), probs = u)
      # y <- quantile(c(Y_theta[,i]), probs = u)
      x <- c(X_theta[,i])
      y <- c(Y_theta[,i])
      trans <- general_1d_transport(X = t(x), Y = t(y),
                                    a = a, b = b,
                                    method = "univariate")
      cost <- ((abs(x[trans$from] - y[trans$to])^ground_p)^(1/ground_p))^p %*% trans$mass
      return(cost)
    }
    )
    loss <- mean(costs)^(1/p)
  } else {
    n1 <- ncol(X)
    n2 <- ncol(Y)
    
    if (missing(a) || is.null(a)) {
      # warning("assuming all points in first group have equal mass")
      a <- as.double(rep(1/n1, n1))
    }
    if (missing(b) || is.null(b)) {
      # warning("assuming all points in first group have equal mass")
      b <- as.double(rep(1/n2, n2))
    }
    
    nzero_a <- a != 0
    nzero_b <- b != 0
    
    mass_x <- a[nzero_a]
    mass_y <- b[nzero_b]
    
    X <- X[, nzero_a, drop = FALSE]
    Y <- Y[, nzero_b, drop = FALSE]
    # mass_x <- as.double(rep(1/n1, n1))
    # mass_y <- as.double(rep(1/n2, n2))
    # 
    # cost <- cost_calc(X, Y, ground_p)
    
    # if (method == "exact") {
    if (isTRUE(list(...)$unbiased) && method == "sinkhorn" ) {
      cost <- cost_calc(X,Y, ground_p)
      cost_a <- cost_calc(X,X, ground_p)
      cost_b <- cost_calc(Y,Y, ground_p)
      pot <- sinkhorn_pot(mass_x = mass_x, mass_y = mass_y, p = p, 
                          cost = cost, cost_a = cost_a, cost_b = cost_b, ...)
      
      return((sum(mass_x * pot$f) + sum(mass_y * pot$g))^(1/p))
    }
      
      tp <- transport_plan(X = X, Y = Y, a = a, b = b, p = p, ground_p = ground_p,
                                    observation.orientation = obs, method = method, ...)
      tplan <- tp$tplan
      loss <- wasserstein_(mass_ = tplan$mass, cost_ = tp$cost, p = p, from_ = tplan$from, to_ = tplan$to)
      
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

wasserstein_individual <- function(X,Y, ground_p, observation.orientation = c("colwise","rowwise")) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.matrix(Y)) Y <- as.matrix(Y)
  obs <- match.arg(observation.orientation)
  if (obs == "rowwise") {
    X <- t(X)
    Y <- t(Y)
  }
  
  Xs <- apply(X,2,sort)
  Ys <- apply(Y,2,sort)
  
  loss <- colMeans((Xs - Ys)^ground_p)
  
  return(loss^(1/ground_p))
  
}

general_dist <- function(X, Y) {
  idx_x <- hilbert_proj_(X) + 1
  idx_y <- hilbert_proj_(Y) + 1
  n <- ncol(X)
  m <- ncol(Y)
  
  mass_a <- rep(1/n, n)
  mass_b <- rep(1/m, m)
  cum_a <- c(cumsum(mass_a))[-n]
  cum_b <- c(cumsum(mass_b))[-m]
  mass <- diff(c(0,sort(c(cum_a, cum_b)),1))
  cum_m <- cumsum(mass)
  arep <- table(cut(cum_m, c(-Inf, cum_a, Inf)))
  brep <- table(cut(cum_m, c(-Inf, cum_b, Inf)))
  a_idx <- rep(idx_x, times = arep)
  b_idx <- rep(idx_y, times = brep)
  
  transport <- list(from = a_idx[order(b_idx)], to = sort(b_idx), mass = mass[order(b_idx)])
  
  return(transport)
  # test <- data.frame(from = a_idx, to = b_idx, mass = mass)
}


wasserstein_multimarg <- function(..., p = 2, ground_p = 2, observation.orientation = c("rowwise","colwise"), 
                         method = c("hilbert", "univariate")) {
  
  if (method == "univariate" | method == "hilbert" ) {
    tp <- transport_plan_multimarg(..., p = p, ground_p = ground_p,
                         observation.orientation = observation.orientation, method = method)
    # loss <- c((((colSums(abs(X[, tp$tplan$from, drop = FALSE] - Y[, tp$tplan$to, drop=FALSE])^ground_p))^(1/ground_p))^p %*% tp$tplan$mass)^(1/p))
    loss <- tp$cost
  } else {
    
    stop("Transport method", method, "not currently supported for multimarginal problems.")
    
  }
  
  return(loss)
}