WPRM <- function(X, Y, theta, force = NULL, p = 2, ground_p = 2,
                          direction = c("backward","forward"), 
                          method=c("selection.variable","scale","projection"),
                          transport.method = c("exact", "sinkhorn", "hilbert","rank",
                                               "univariate.approximation.pwr",
                                               "univariate.approximation"),
                          epsilon = 0.05,
                          OTmaxit = 100,
                          calc.theta = TRUE)
{
  this.call <- as.list(match.call()[-1])
  
  d <- ncol(X)
  n <- nrow(X)
  if(ncol(theta) == ncol(X)) {
    theta <- t(theta)
  } 
  
  S <- ncol(theta)
  X_ <- t(X)
  if(is.null(Y)) {
    Y_ <- crossprod(X_,theta)
    same <- TRUE
  } else {
    if(nrow(Y) != n){
      Y_ <- t(Y)
    } else {
      Y_ <- Y
    }
    same <- FALSE
    if(all(Y_==crossprod(X_, theta))) same <- TRUE
  }
  method <- match.arg(method)
  transport.method <- match.arg(transport.method)
  if(!is.null(force)) stopifnot(is.numeric(force))
  
  if(length((unique(force)))==d) stop("forcing all variables into the model")
  remove.idx <- 1:d
  remove.idx <- remove.idx[!(remove.idx %in% force)]
  wp <- sapply(remove.idx, function(i) {
    wasserstein(crossprod(X_[-i,], theta[-i,]), Y_, p = p, ground_p = ground_p, observation.orientation = "colwise",
                transport.method = transport.method, epsilon = epsilon, niter = OTmaxit)
  })
  importance <- remove.idx[order(wp, decreasing = TRUE)]
  indices <- lapply(1:length(importance), function(i) sort(c(force, importance[1:i])))
  
  output <- list(index = indices, importance = importance, wP = wp, call = formals(WPRM))
  output$call[names(this.call)] <- this.call
  output$nzero <- sapply(indices, function(i) length(i))
  class(output) <- c("WpProj","removeone")
  if(calc.theta){
    output$theta  <- lapply(indices, function(i) {
      theta_temp <- matrix(0, d, S)
      theta_temp[i,] <- theta_temp[i,,drop=FALSE] 
      return(theta_temp)
    })
    output$eta <- lapply(output$theta, function(tt) X %*% tt)
  }
  
  
  
  return(output)
}
