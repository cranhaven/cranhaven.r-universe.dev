#' p-Wasserstein projections with an L0 penalty
#'
#' @param X matrix of covariates
#' @param Y matrix of predictions
#' @param theta optional matrix of coefficients from original model, if relevant 
#' @param power power of the Wasserstein distance
#' @param method One of "selection.variable" or "projection". Methods decide whether covariate matrix in `theta` is preserved ("selection.variable") or if new projections are generated ("projection")
#' @param transport.method Method for Wasserstein distance calculation. Should be one of the outputs of [transport_options()].
#' @param epsilon hyperparameter for sinkhorn iterations
#' @param OTmaxit max iteration for sinkhorn iterations
#' @param parallel foreach backend
#'
#' @return `WpProj` object
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
# fit <-  WPL0(X=x, Y=t(post_mu), theta = t(post_beta),
#              method = "selection.variable",
#              transport.method = "hilbert"
# )
# }
WPL0 <- function(X, Y = NULL, theta, power = 2,
                 method = c("selection.variable", "projection"),
                 transport.method = transport_options(),
                 epsilon = 0.05, OTmaxit = 100,
                 parallel = NULL, ...) {
  this.call <- as.list(match.call()[-1])
  d <- ncol(X)
  n <- nrow(X)
  method <- match.arg(method)
  transport.method <- match.arg(transport.method, transport_options())
  obs.direction <- "colwise"
  p <- ground_p <- power
  
  # if (grepl("univariate", transport.method) ) {
  #   obs.direction <- "rowwise"
  #   # X_ <- t(X_)
  #   # Y_ <- t(Y_)
  # } else {
  #   obs.direction <- "colwise"
  # }
  
  if(d!=nrow(theta)){
    if(ncol(theta) == ncol(X)) {
      theta <- t(theta)
    } else {
      stop("Columns of theta not match columns of X")
    }
  }
  S <- ncol(theta)
  X_ <- t(X)
  same <- FALSE
  if(is.null(Y)) {
    same <- TRUE
    Y <- crossprod(X_,theta)
  } else {
    if(nrow(Y) != nrow(X)) Y <- t(Y)
    if(all(Y==crossprod(X_, theta))) same <- TRUE
  }
  
  if(method == "projection") {
    xtx <- crossprod(X)
    xty <- crossprod(X,Y)
  } else {
    xtx <- NULL
    xty <- NULL
  }
  
  OToptions <- list(same = same,
                    method = method,
                    transport.method = transport.method, 
                    epsilon = epsilon,
                    niter = OTmaxit)

  combos <- lapply(1:d, function(i) utils::combn(d,i))
  w2 <- lapply(combos, function(i) rep(NA, ncol(i)))
  cur_idx <- NULL
  min_w2 <- Inf
  min_idx <- c(NULL,NULL)
  
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
  w2 <- foreach::foreach(i  = seq_along(combos)) %dorng% {
    w2out <- rep(NA, ncol(combos[[ i]]))
    for(j in 1:ncol(combos[[i]])) {
      cur_idx <- c(combos[[i]][,j])
      w2out[j] <- dist.fun(X = X_, active.idx = cur_idx, theta = theta, Y = Y,
                           xtx = xtx, xty = xty, OToptions = OToptions, 
                           obs.direction = obs.direction, p = p,
                           ground_p = ground_p)
    }
    return(w2out)
  }
  mins <- sapply(w2, min)
  which.comb <- which.min(mins)
  min_idx <- c(which.comb, which.min(w2[[which.comb]]))
  min_w2 <- w2[[min_idx[1]]][min_idx[2]]
  
  minPerActiveLev <- lapply(w2, which.min)
  minCombPerActive <- lapply(1:d, function(i) combos[[i]][,minPerActiveLev[[i]]])
  names(minCombPerActive) <- 1:d
  
  call <- formals(WPL0)
  call[names(this.call)] <- this.call
  output <- list(combinations = combos,
                 w2 = w2,
                 minCombPerActive = minCombPerActive,
                 min_combination = c(combos[[min_idx[1]]][,min_idx[2]]),
                 min_w2 = min_w2,
                 call = call,
                 method = method)
  output$beta <- sapply(output$minCombPerActive, function(idx) {
    temp <- calc.beta(xtx = xtx, xty = xty, active.idx = idx, method = method,
                      OToptions = OToptions, x = X_, theta = theta, Y = Y, niter = 500)
    # temp[idx,] <- theta[idx,,drop=FALSE]
    return(c(temp))
  })
  class(output) <-c("WpProj","L0")
  extraction <- extractTheta(output, theta)
  output$nzero <- extraction$nzero
  output$theta <- extraction$theta
  
  output$eta <- lapply(output$theta, function(tt) crossprod(X_, tt) )
  # output$theta <- lapply(output$minCombPerActive, function(idx) {
  #   temp <- matrix(0, nrow=d, ncol=S)
  #   temp[idx,] <- theta[idx,,drop=FALSE]
  #   return(temp)
  # })
  # output$nzero <- 1:d
  
  
  return(output)
}
