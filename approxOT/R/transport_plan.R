#
# representation(from="integer",
#                to="integer",
#                mass="numeric")
# setOldClass("transport.plan")

#
# representation(indexes="integer",
#                mass="numeric")
# setOldClass("multi.transport.plan")

#' Covert the 2-dimensional index to 1-dimensional index
#'
#' @param i Index of row
#' @param j Index of column
#' @param n Total number of rows
#' @param m Total number of columns
#'
#' @return a 1d index for easy matrix entry
#' 
#' @keywords internal
dist_2d_to_1d <- function (i, j, n, m) {
  valid <- (i >= 1) & (j >= 1) & (i <= n) & (j <= m)
  k <- (j - 1) * n + i
  k[!valid] <- NA_real_
  return(k)
}

#' Transform transportation plan to transportation matrix
#'
#' @param x An object of class `transport.plan`. See output of (transport_plan)[transport_plan()]
#' @param ... Unused arguments
#'
#' @return A matrix specifying the minimal joint distribution between samples. Margins will be equal to the marginal distributions of the samples
#' @exportS3Method base::as.matrix transport.plan
#'
#' @examples
#' set.seed(203987)
#' n <- 5
#' d <- 2
#' x <- matrix(rnorm(d*n), nrow=d, ncol=n)
#' y <- matrix(rnorm(d*n), nrow=d, ncol=n)
#' #get hilbert sort orders for x in backwards way
#' trans_plan <- transport_plan(X=x, Y=x, ground_p = 2, p = 2, 
#'                          observation.orientation =  "colwise", 
#'                          method = "hilbert")
#' trans_matrix <- as.matrix(trans_plan)
#' print(trans_matrix)
as.matrix.transport.plan <- function(x, ...) {
  stopifnot(is.transport.plan(x))
  n1 <- max(x$from)
  n2 <- max(x$to)
  gamma <- matrix(0.0, n1, n2)
  gamma[dist_2d_to_1d(x$from, x$to, n1, n2)] <- x$mass
  return(gamma)
}

#' Transform transportation matrix to transportation plan
#'
#' @param transport_matrix A matrix that is a transportation matrix, i.e. the minimal joint distribution for two samples.
#' @param ... Unused arguments
#'
#' @return An object of class `transport.plan`. See output of (transport_plan)[transport_plan]
#' @export
#'
#' @examples
#' set.seed(203987)
#' n <- 5
#' d <- 2
#' x <- matrix(stats::rnorm(d*n), nrow=d, ncol=n)
#' y <- matrix(stats::rnorm(d*n), nrow=d, ncol=n)
#' #get hilbert sort orders for x in backwards way
#' trans_plan <- transport_plan(X=x, Y=x, ground_p = 2, p = 2, 
#'                          observation.orientation =  "colwise", 
#'                          method = "hilbert")
#' trans_matrix <- as.matrix(trans_plan$tplan)
#' tplan2 <- as.transport.plan(trans_matrix)
#' all.equal(tplan2, trans_plan$tplan)
as.transport.plan <- function(transport_matrix, ...) {
  n1 <- nrow(transport_matrix)
  n2 <- ncol(transport_matrix)
  tplan <- list(from = NULL,
                to = NULL,
                mass = NULL)
  class(tplan) <- c("transport.plan", "list")
  
  pos.idx <- which(transport_matrix > 0.0, arr.ind = TRUE)
  tplan$from <- pos.idx[,1]
  tplan$to <- pos.idx[,2]
  tplan$mass <- transport_matrix[dist_2d_to_1d(tplan$from,
                                               tplan$to,
                                               n1,
                                               n2)]
  return(tplan)
}

#' Check if function is a transport.plan
#'
#' @param tplan An object of class `transport.plan`. See output of (transport_plan)[transport_plan]
#'
#' @return Logical
#' @export
#'
#' @examples
#' set.seed(203987)
#' n <- 5
#' d <- 2
#' x <- matrix(rnorm(d*n), nrow=d, ncol=n)
#' y <- matrix(rnorm(d*n), nrow=d, ncol=n)
#' #get hilbert sort orders for x in backwards way
#' trans_plan <- transport_plan(X=x, Y=x, ground_p = 2, p = 2, 
#'                          observation.orientation =  "colwise", 
#'                          method = "hilbert")
#' print(is.transport.plan(trans_plan))
is.transport.plan <- function(tplan) {
  inherits(tplan, "transport.plan")
}

#' Optimal transport plans given a pre-specified cost
#'
#' @param mass_x The empirical measure of the first sample
#' @param mass_y The empirical measure of the second sample.
#' @param p The power of the Wasserstein distance
#' @param cost Specify the cost matrix in advance.
#' @param method The transportation method to use, one of "exact", "networkflow","shortsimplex", "sinkhorn", "greenkhorn"
#' @param cost_a The cost matrix for the first sample with itself. Only used for unbiased Sinkhorn
#' @param cost_b The cost matrix for the second sample with itself. Only used for unbiased Sinkhorn
#' @param ... Additional arguments for various methods
#' \itemize{
#' \item "niter": The number of iterations to use for the entropically penalized optimal transport distances
#' \item "epsilon": The multiple of the median cost to use as a penalty in the entropically penalized optimal transport distances
#' \item"unbiased": If using Sinkhorn distances, should the distance be de-biased? (TRUE/FALSE)
#' }
#'
#' @return A transportation plan as an object of class "transport.plan", which is a list with slots "from","to", and "mass".
#' @export
#'
#' @examples
#' n <- 32
#' d <- 5
#' set.seed(293897)
#' A <- matrix(stats::rnorm(n*d),nrow=d,ncol=n)
#' B <- matrix(stats::rnorm(n*d),nrow=d,ncol=n)
#' transp.meth <- "sinkhorn"
#' niter <- 1e2
#' test <- transport_plan_given_C(rep(1/n,n), 
#' rep(1/n,n),  2, cost = cost_calc(A,B,2), 
#' "sinkhorn", niter = niter)
transport_plan_given_C <- function(mass_x, mass_y, p = 2, 
                                   cost=NULL, method = "exact", 
                                   cost_a = NULL, cost_b = NULL, ...) {
  method <- match.arg(method, c("exact","networkflow","shortsimplex","sinkhorn","greenkhorn", 
                                "sinkhorn_log"
                                # , "sinkhorn2"
                                ))
  
  dots <- list(...)
  epsilon <- as.double(dots$epsilon)
  niter <- as.integer(dots$niter)
  unbiased <- isTRUE(as.logical(dots$unbiased))
  threads <- as.integer(dots$threads)
  stopifnot(all(is.finite(cost)))
  entropy.method <- method == "sinkhorn"
  
  if (unbiased && entropy.method && (is.null(cost_a) || is.null(cost_b))) {
    stop("Must specify cost_a and cost_b for sinkhorn unbiased")
  } else if(!unbiased) {
    cost_a <- cost_b <- matrix(0.0,0,0)
  } else {
    cost_a <- cost_b <- matrix(0.0,0,0)
  }
  
  if (length(epsilon) == 0) epsilon <- as.double(0.05)
  if (length(niter) == 0) {
    niter <- if (method == "exact" | method == "networkflow") {
      as.integer(0)
      } else {
        as.integer(100)
      }
  }
  if (length(threads) == 0) threads <- as.integer(1)
  
  if (is.null(cost) ) stop("Cost matrix must be provided")
  tplan <- if (method == "exact" | method == "greenkhorn" | 
               method == "sinkhorn" | method == "sinkhorn_log" |
               # method == "randkhorn" | method == "gandkhorn" | 
               method == "networkflow" |
               method == "shortsimplex") {
    
    n1 <- length(mass_x)
    n2 <- length(mass_y)
    
    if (n1 > 1 & n2 > 1) {
      transport_C_(mass_a_ = mass_x, 
                   mass_b_ = mass_y, 
                   cost_matrix_ = cost^p, 
                   method_ = method,
                   cost_matrix_A_ = cost_a,
                   cost_matrix_B_ = cost_b,
                   epsilon_ = epsilon, 
                   niter_ = niter,
                   unbiased_ = unbiased,
                   threads_ = threads)
    } else if (n2 == 1) {
      list(from = 1:n1, to = rep(1,n1), mass = mass_x)
    } else if (n1 == 1) {
      list(from = rep(1,n2), to = 1:n2, mass = mass_y)
    } else {
      stop("Some error found in mass_x or mass_y length. Check mass input.")
    }
    
  # } else if (method == "sinkhorn2") {
  #   
  #   sinkhorn_transport(mass_x = mass_x, mass_y = mass_y, cost = cost^p, 
  #                      eps = epsilon, niterations = niter)
    
  } else {
    stop( paste0( "Transport method ", method, " not supported" ) )
  }
  
  class(tplan) <- c("transport.plan","list")
  return( tplan )
  
}

#' Optimal transport plans
#'
#' @param X The covariate data of the first sample.
#' @param Y The covariate data of the second sample.
#' @param a Optional. Empirical measure of the first sample
#' @param b Optional. Empirical measure of the second sample
#' @param p The power of the Wasserstein distance
#' @param ground_p The power of the Lp norm
#' @param observation.orientation Are observations by row ("rowwise") or column ("colwise").
#' @param method Which transportation method to use. See [transport_options][transport_options]
#' @param ... Additional arguments for various methods
#' \itemize{
#' \item"niter": The number of iterations to use for the entropically penalized optimal transport distances
#' \item"epsilon": The multiple of the median cost to use as a penalty in the entropically penalized optimal transport distances
#' \item"unbiased": If using Sinkhorn distances, should the distance be de-biased? (TRUE/FALSE)
#' \item"nboot": If using sliced Wasserstein distances, specify the number of Monte Carlo samples
#' }
#'
#' @return a list with slots "tplan" and "cost". "tplan" is the optimal transport plan and "cost" is the optimal transport distance.
#' @export
#'
#' @examples
#' set.seed(203987)
#' n <- 100
#' d <- 10
#' x <- matrix(stats::rnorm(d*n), nrow=d, ncol=n)
#' y <- matrix(stats::rnorm(d*n), nrow=d, ncol=n)
#' #get hilbert sort orders for x in backwards way
#' transx <- transport_plan(X=x, Y=x, ground_p = 2, p = 2, 
#'                          observation.orientation =  "colwise", 
#'                          method = "hilbert")
transport_plan <- function(X, Y, a = NULL, b = NULL, p = 2, ground_p = 2,
                           observation.orientation = c("rowwise", "colwise"), 
                           method = transport_options(), ... ) {
  
  obs <- match.arg(observation.orientation)
  method <- match.arg(method)
  
  if (!is.matrix(X)) {
    X <- as.matrix(X)
    if (dim(X)[2] == 1) X <- t(X)
  }
  if (!is.matrix(Y)) {
    Y <- as.matrix(Y)
    if (dim(Y)[2] == 1) Y <- t(Y)
  }
  
  p <- as.double(p)
  ground_p <- as.double(ground_p)
  
  if (obs == "rowwise") {
    X <- t(X)
    Y <- t(Y)
  }
  stopifnot(all(is.finite(X)))
  stopifnot(all(is.finite(Y)))
  
  n1 <- ncol(X)
  n2 <- ncol(Y)
  
  if (is.null(a)) {
    a <- as.double(rep(1/n1, n1))
  } 
  
  if (is.null(b)) {
    b <- as.double(rep(1/n2, n2))
  }
  mass_x <- a
  mass_y <- b
  
  
  cost <- tplan <- NULL
  if (method == "univariate.approximation") {
    tplan <-  list(from = apply(X, 1, order), to = apply(Y,1,order), mass = mass_x)
    cost <- sapply(1:nrow(X), function(i) 
      sum((X[i, tplan$from[,i],drop = FALSE] - 
             Y[i, tplan$to[,i],drop = FALSE] )^ground_p * tplan$mass )^(1.0/ground_p))
  } else if (method == "univariate.approximation.pwr") {
    dots <- list(...)
    if (is.null(dots$is.X.sorted)) dots$is.X.sorted <- FALSE
    is.A.sorted <- as.logical(dots$is.X.sorted)
    tplan <- transport_(A_ = X, B_ = Y, p = p, ground_p = ground_p, 
                        method_ = method, a_sort = is.A.sorted, unbiased_ = FALSE, threads_ = 1L)
    cost <- sum((X[tplan$from] - 
                   Y[tplan$to] )^p * tplan$mass*1/nrow(Y))
  } else if (method == "networkflow" | method == "shortsimplex" | method == "sinkhorn" | method == "greenkhorn" | 
             # method == "randkhorn" | method == "gandkhorn" | 
             method == "sinkhorn2" | method == "sinkhorn_log") {
    # tplan <- transport_(X, Y, p, ground_p, "shortsimplex")
    
    
    cost <- cost_calc(X, Y, ground_p)
    cost_a <- cost_b <- NULL
    if (isTRUE(list(...)$unbiased) ) {
      cost_a <- cost_calc(X, X, ground_p)
      cost_b <- cost_calc(Y, Y, ground_p)
    } 
    tplan <- transport_plan_given_C(mass_x, mass_y, p, cost, method, cost_a, cost_b, ...)
  } else if (method == "univariate" | method == "hilbert" | method == "rank") {
    dots <- list(...)
    if (is.null(dots$is.X.sorted)) dots$is.X.sorted <- FALSE
    is.A.sorted <- as.logical(dots$is.X.sorted)
    if (isTRUE(all.equal(sort(mass_x), sort(mass_y) )) ) {
      tplan <- transport_(A_ = X, B_ = Y, p = p, ground_p = ground_p, 
                          method_ = method, a_sort = is.A.sorted, epsilon_ = 0.0, niter_ = 0L,
                          threads_ = 1L)
    } else if (method == "hilbert" | method == "univariate") {
      tplan <- general_1d_transport(X, Y, a = mass_x, b = mass_y, method = method)
    } else {
      stop("only measures with same number of atoms supported for rank methods.")
    }
    cost <- c(((colSums(abs(X[, tplan$from, drop = FALSE] - Y[, tplan$to, drop = FALSE])^ground_p)^(1/ground_p))^p %*% tplan$mass)^(1/p))
  } else if (method == "sliced") {
    dots <- list(...)
    tplan <- NULL
    nboot <- as.double(dots$nsim)
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
      tplan <- general_1d_transport(t(x),t(y),"univariate")
      cost <- ((sum(abs(x[tplan$from] - y[tplan$to])^ground_p))^(1/ground_p))^p %*% tplan$mass
      return(cost)
      }
      )
    cost <- mean(costs)^(1/p)
    tplan <- NULL
  } else if (method == "swapping") {
    dots <- list(...)
    epsilon <- as.double(dots$epsilon)
    niter <- as.integer(dots$niter)
    
    if(length(epsilon) == 0) epsilon <- as.double(0.001)
    if(length(niter) == 0) niter <- as.integer(100)
    
    if(ncol(X) > 1e6 | ncol(Y) > 1e6) {
      if(ncol(X) == ncol(Y)) {
        idx_x <- hilbert_proj_(X) 
        idx_y <- hilbert_proj_(Y)
        idxs  <- cbind(idx_x[order(idx_y)], 0:(ncol(Y)-1)) # to make similar
        idxs  <- cbind(idx_x, idx_y)
        mass <- rep(1/ncol(X), ncol(X))
      } else {
        tplan <- general_hilbert_transport(X, Y)
        idxs <- cbind(tplan$from, tplan$to)-1
        mass <- tplan$mass
      }
      if(mode(idxs) != "integer") mode(idxs) <- "integer"
      
      tplan <- transport_swap_(X,
                               Y,
                               idxs,
                               as.double(mass),
                               p, ground_p,
                               epsilon, niter)
      
    } else {
      is.A.sorted <- FALSE
      
      tplan <- transport_(A_ = X, B_ = Y, p = p, ground_p = ground_p, 
                          method_ = method, a_sort = is.A.sorted, epsilon_ = epsilon, niter_ = niter,
                          threads_ = 1L)
    }
    cost <- c((((colSums(abs(X[, tplan$from, drop=FALSE] - Y[, tplan$to, drop=FALSE])^ground_p))^(1/ground_p))^p %*% tplan$mass)^(1/p))
    
  } else {
    stop( paste0( "Transport method ", method, " not supported" ) )
  }
  
  class(tplan) <- c("transport.plan", "list")
  tplan_out <- list(tplan = tplan, cost = cost )
  
  
  return(tplan_out)
  
}

#' One-dimensional optimal transport for measures with more general mass
#'
#' @param X Data for sample one. Should be a vector if method is "univariate" or a matrix if method is "hilbert"
#' @param Y Data for sample two Should be a vector if method is "univariate" or a matrix if method is "hilbert"
#' @param a Empirical measure for sample one.
#' @param b Empirical measure for sample two.
#' @param method One of "hilbert" or "univariate"
#'
#' @return An optimal transportation plan as a list with slots "from", "to", and "mass"
#' @export
#'
#' @examples
#' set.seed(23423)
#' n <- 100
#' d <- 10
#' x <- matrix(stats::rnorm((n + 11)*d), n + 11 , d)
#' y <- matrix(stats::rnorm(n*d), n, d)
#' 
#' trans <- general_1d_transport(t(x), t(y))
#' @keywords internal
general_1d_transport <- function(X, Y, a = NULL, b = NULL, method = c("hilbert", "univariate")) {
  method <- match.arg(method)
  
  if (method == "hilbert") {
    idx_x <- hilbert_proj_(X) + 1L
    idx_y <- hilbert_proj_(Y) + 1L
    
  } else if (method == "univariate") {
    idx_x <- order(X) 
    idx_y <- order(Y) 
  }
  
  n <- ncol(X)
  m <- ncol(Y)
  
  if (is.null(a)) {
    mass_a <- rep(1/n, n)
  } else {
    a <- a[idx_x]
    X <- X[,a > 0, drop = FALSE]
    n <- ncol(X)
    idx_x <- idx_x[a > 0]
    mass_a <- a[a > 0]/sum(a[a > 0])
  }
  if (is.null(b)) {
    mass_b <- rep(1/m, m)
  } else {
    b <- b[idx_y]
    Y <- Y[,b > 0, drop = FALSE]
    m <- ncol(Y)
    idx_y <- idx_y[b > 0]
    mass_b <- b[b > 0]/sum(b[b > 0])
  }
  
  cum_a  <- c(cumsum(mass_a))[-n]
  cum_b  <- c(cumsum(mass_b))[-m]
  mass   <- diff(c(0,unique(sort(c(cum_a, cum_b))),1))
  # mass   <- unique(mass)
  cum_m  <- cumsum(mass)
  arep   <- table(cut(cum_m, c(-Inf, cum_a, Inf)))
  brep   <- table(cut(cum_m, c(-Inf, cum_b, Inf)))
  a_idx  <- unlist(mapply(function(i,r){rep(i, times = r)}, i = idx_x, r = arep, SIMPLIFY = FALSE))
  b_idx  <- unlist(mapply(function(i,r){rep(i, times = r)}, i = idx_y, r = brep, SIMPLIFY = FALSE))
  # rep(idx_y, times = brep)
  
  transport <- list(from = a_idx, 
                    to = b_idx, 
                    mass = mass)
  
  class(transport) <- c("transport.plan", "list")
  return(transport)
  # test <- data.frame(from = a_idx, to = b_idx, mass = mass)
}

general_hilbert_transport <- function(X, Y) {
  idx_x <-  hilbert_proj_(X) + 1L
  idx_y <-  hilbert_proj_(Y) + 1L
  n <- ncol(X)
  m <- ncol(Y)
  
  mass_a <- rep(1/n, n)
  mass_b <- rep(1/m, m)
  cum_a  <- c(cumsum(mass_a))[-n]
  cum_b  <- c(cumsum(mass_b))[-m]
  mass   <- diff(c(0,unique(sort(c(cum_a, cum_b))),1))
  # mass   <- unique(mass)
  cum_m  <- cumsum(mass)
  arep   <- table(cut(cum_m, c(-Inf, cum_a, Inf)))
  brep   <- table(cut(cum_m, c(-Inf, cum_b, Inf)))
  a_idx  <- rep(idx_x, times = arep)
  b_idx  <- rep(idx_y, times = brep)
  
  transport <- list(from = a_idx[order(b_idx)], to = sort(b_idx), mass = mass[order(b_idx)])
  
  class(transport) <- c("transport.plan", "list")
  return(transport)
  # test <- data.frame(from = a_idx, to = b_idx, mass = mass)
}

#' Multimarginal optimal transport plans
#'
#' @param ... Either data matrices as separate arguments or a list of data matrices. Arguments after the data must be specified by name.
#' @param p The power of the Wasserstein distance to use
#' @param ground_p The power of the Euclidean distance to use
#' @param observation.orientation Are observations by rows or columns
#' @param method One of "hilbert", "univariate", or "sliced"
#' @param nsim Number of simulations to use for the sliced method
#'
#' @return transport plan
#' @export
#' 
#' @examples 
#' set.seed(23423)
#' n <- 100
#' d <- 10
#' p <- ground_p <- 2 #euclidean cost, p = 2
#' x <- matrix(stats::rnorm((n + 11)*d), n + 11 , d)
#' y <- matrix(stats::rnorm(n*d), n, d)
#' z <- matrix(stats::rnorm((n +455)*d), n +455, d)
#' 
#' # make data a list
#' data <- list(x,y,z)
#' 
#' tplan <- transport_plan_multimarg(data, p = p, ground_p = ground_p,
#' observation.orientation = "rowwise", method = "hilbert")
#' 
#' #' #transpose data works too
#' datat <- lapply(data, t)
#' 
#' tplan2 <- transport_plan_multimarg(datat, p = p, ground_p = ground_p,
#' observation.orientation = "colwise",method = "hilbert")
transport_plan_multimarg <- function(..., p = 2, ground_p = 2,
                               observation.orientation = c("rowwise", "colwise"), 
                               method = c("hilbert", "univariate", "sliced"),
                               nsim = 1000) {
  obs <- match.arg(observation.orientation)
  method <- match.arg(method)

  if (...length() > 1) {
    data <- list(...)
  } else {
    data <- (...)
  }
  
  data <- lapply(data, function(mm) {
    if (!is.matrix(mm)) {
      mm <- as.matrix(mm)
      if(dim(mm)[2] == 1) mm <- t(mm)
    }
    return(mm) 
    })
  if(obs == "rowwise"){
    data <- lapply(data, t)
  }
  lapply(data, function(X) stopifnot(all(is.finite(X))))
  p <- as.double(p)
  ground_p <- as.double(ground_p)
  ds <- sapply(data, nrow)
  if(all(ds != ds[1])) stop("Dimension of input data is not all the same. Data can have different numbers of observations but must have the same number of covariates.")
  d <- ds[1]
  
  cost <- tplan <- NULL
  
  if(method == "hilbert") {
    idx <- lapply(data, hilbert_proj_)
    idx <- lapply(idx, "+", 1L)
  } else if (method == "univariate") {
    idx <- lapply(data, order)
  } else if (method == "sliced") {
    if(is.null(nsim)) nsim <- 1e3
    nboot <- nsim
    theta <- matrix(stats::rnorm(d * nboot), d, nboot)
    theta <- sweep(theta, 2, STATS=apply(theta,2,function(x) sqrt(sum(x^2))), FUN = "/")
    data_theta <- lapply(data, crossprod, y = theta)
    cost <- (mean(sapply(1:nboot, function(i)  transport_plan_multimarg(lapply(data_theta, function(j) t(j[,i])), 
                                  p = ground_p, ground_p = ground_p,
                                  observation.orientation = "colwise", 
                                  method = "univariate")$cost^p)))^(1.0/p)
    return(list(tplan = NULL, cost = cost))
  }
  
  n     <- sapply(data, ncol)
  cmass <- lapply(n, function(nn) seq(1/nn,(nn-1)/nn, by = 1/nn))
  mass  <- diff(c(0,unique(sort(unlist(cmass))),1))
  cum_m <- cumsum(mass)
  reps  <- lapply(cmass, function(m) table(cut(cum_m, c(-Inf, m, Inf))))
  repidx<- mapply(function(i,r){rep(i, times = r)}, i = idx, r = reps, SIMPLIFY = FALSE)
  names(repidx) <- names(data)
  tplan <- list(indexes = repidx, mass = mass)
  
  cost  <- multi_marg_final_cost_(idx_ = tplan$indexes, 
                                  data_ = data, 
                                  mass_ = tplan$mass,
                                  M = length( tplan$indexes[[1]]),
                                  D = d,
                                  p = p,
                                  ground_p = ground_p
                                  )
  
  class(tplan) <- c("multi.transport.plan", "list")
  return(list(tplan = tplan, cost = cost))
}


#' Return the dual potentials for the Sinkhorn distance
#'
#' @param mass_x The empirical distribution of the first sample
#' @param mass_y The empirical distribution of the second sample
#' @param p The power to raise the cost by
#' @param cost The cost matrix between first and second samples
#' @param cost_a The cost matrix for the first sample
#' @param cost_b The cost matrix for the second sample
#' @param ... Additional arguments including
#' \itemize{
#' \item epsilon: The fraction of the median cost to use as a penalty
#' \item niter: Number of iterations to run the Sinkhorn algorithm
#' \item unbiased: Should the potentials be de-biased TRUE/FALSE
#' }
#'
#' @return A list with slots "f" and "g", the potentials of the rows and margins, respectively.
#' @export
#'
#' @keywords internal
sinkhorn_pot <- function(mass_x, mass_y, p = 2, 
                        cost=NULL,
                        cost_a = NULL, cost_b = NULL, ...) {
  
  dots <- list(...)
  epsilon <- as.double(dots$epsilon)
  niter <- as.integer(dots$niter)
  unbiased <- isTRUE(as.logical(dots$unbiased))
  stopifnot(all(is.finite(cost)))
  
  if (unbiased && (is.null(cost_a) || is.null(cost_b))) {
    stop("Must specify cost_a and cost_b for sinkhorn unbiased")
  } else if(!unbiased) {
    cost_a <- cost_b <- matrix(0.0,0,0)
  }
  
  if (length(epsilon) == 0) epsilon <- as.double(0.05)
  if (length(niter) == 0) {
    niter <- as.integer(100)
  }

  if (is.null(cost) ) stop("Cost matrix must be provided")
  
  
  pot <- sinkhorn_pot_(mass_x, mass_y, 
                        cost^p, 
                        epsilon, niter,
                        unbiased,
                        cost_a^p, 
                        cost_b^p)
  
  return(pot)
  
}
