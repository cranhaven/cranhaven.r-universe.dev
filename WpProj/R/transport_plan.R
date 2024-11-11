transport_plan_given_C <- function(mass_x, mass_y, p = 2, 
                           cost=NULL, method = "exact", ...) {
  method <- match.arg(method, c("exact","sinkhorn","greenkhorn" 
                                # , "randkhorn", "gandkhorn", "sinkhorn2"
                                ))
  
  dots <- list(...)
  epsilon <- as.double(dots$epsilon)
  niter <- as.integer(dots$niter)
  stopifnot(all(is.finite(cost)))
  
  if(length(epsilon) == 0) epsilon <- as.double(0.05)
  if(length(niter) == 0) niter <- as.integer(100)
  
  if (is.null(cost) ) stop("Cost matrix must be provided")
  tplan <- if (method == "exact" | method == "greenkhorn" | method == "sinkhorn"
               # method == "randkhorn" | method == "gandkhorn"
               ) {
    
    n1 <- length(mass_x)
    n2 <- length(mass_y)
    
    if(n1 > 1 & n2 > 1) {
      transport_C_(mass_a_ = mass_x, mass_b_ = mass_y, cost_matrix_ = cost^p, 
                   method_ = method, epsilon_ = epsilon, niter_ = niter)
    } else if (n2 == 1) {
      list(from = 1:n1, to = rep(1,n1), mass = mass_x)
    } else if (n1 == 1) {
      list(from = rep(1,n2), to = 1:n2, mass = mass_y)
    } else {
      stop("Some error found in mass_x or mass_y length. Check mass input.")
    }
    
  } else if (method == "sinkhorn2") {
    
    sinkhorn_transport(mass_x = mass_x, mass_y = mass_y, cost = cost^p, 
                       eps = epsilon, niterations = niter)
    
  } else {
    stop( paste0( "Transport method ", method, " not supported" ) )
  }
  return( tplan )
  
}

transport_plan <- function(X, Y, p = 2, ground_p = 2,
                           observation.orientation = c("colwise","rowwise"), 
                           method = c("exact", "sinkhorn", "greenkhorn",
                                      # "randkhorn", "gandkhorn", 
                                      "sinkhorn2",
                                      "hilbert", "rank",
                                      "univariate", 
                                      "univariate.approximation", 
                                      "univariate.approximation.pwr"),... ) {
  
  obs <- match.arg(observation.orientation)
  method <- match.arg(method)
  
  if (!is.matrix(X)) {
    X <- as.matrix(X)
    if(dim(X)[2] == 1) X <- t(X)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
    if(dim(Y)[2] == 1) Y <- t(Y)
  }
  
  
  p <- as.double(p)
  ground_p <- as.double(ground_p)
  
  if(obs == "rowwise"){
    X <- t(X)
    Y <- t(Y)
  }
  stopifnot(all(is.finite(X)))
  stopifnot(all(is.finite(Y)))
  
  cost <- tplan <- NULL
  if (method == "univariate.approximation") {
    tplan <-  list(from = apply(X, 1, order), to = apply(Y,1,order), mass = rep(1/ncol(X), ncol(X)))
    cost <- sapply(1:nrow(X), function(i) 
      sum((X[i, tplan$from[,i],drop=FALSE] - 
             Y[i, tplan$to[,i],drop = FALSE] )^ground_p * tplan$mass )^(1.0/ground_p))
  } else if (method == "univariate.approximation.pwr") {
    dots <- list(...)
    if(is.null(dots$is.X.sorted)) dots$is.X.sorted <- FALSE
    is.A.sorted <- as.logical(dots$is.X.sorted)
    tplan <- transport_(A_ = X, B_ = Y, p = p, ground_p = ground_p, 
                        method_ = method, a_sort = is.A.sorted)
    cost <- sum((X[tplan$from] - 
             Y[tplan$to] )^p * tplan$mass*1/nrow(Y))
  } else if (method == "exact" | method == "sinkhorn" | method == "greenkhorn" | method == "randkhorn" | method == "gandkhorn" | method == "sinkhorn2") {
    # tplan <- transport_(X, Y, p, ground_p, "shortsimplex")
    n1 <- ncol(X)
    n2 <- ncol(Y)
    mass_x <- as.double(rep(1/n1, n1))
    mass_y <- as.double(rep(1/n2, n2))

    cost <- cost_calc(X, Y, ground_p)
    tplan <- transport_plan_given_C(mass_x, mass_y, p, cost, method, ...)
  } else if (method == "univariate" | method == "hilbert" | method == "rank") {
    dots <- list(...)
    if(is.null(dots$is.X.sorted)) dots$is.X.sorted <- FALSE
    is.A.sorted <- as.logical(dots$is.X.sorted)
    tplan <- transport_(A_ = X, B_ = Y, p = p, ground_p = ground_p, 
               method_ = method, a_sort = is.A.sorted, epsilon_ = 0.0, niter_ = 0)
    cost <- c((((colSums(abs(X[, tplan$from, drop=FALSE] - Y[, tplan$to, drop=FALSE])^ground_p))^(1/ground_p))^p %*% tplan$mass)^(1/p))
  } else {
    stop( paste0( "Transport method ", method, " not supported" ) )
  }

  return(list(tplan = tplan, cost = cost ))
  
}
