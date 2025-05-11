vcov_function <- function(f, Q, theta_hat, X, Y, jacobian_args, hessian_args){
  stopifnot(is.numeric(Y), is.vector(Y), is.numeric(X), is.matrix(X),
            is.numeric(Q), is.numeric(theta_hat), is.vector(theta_hat),
            is.function(f))
  n <- length(Y)
  p <- length(theta_hat)
  if(is.matrix(Q)){
    g <- function(theta) (Q %*% (Y - f2ftheta(f, X, n)(theta)))^2
  } else {
    g <- function(theta) (Q  *  (Y - f2ftheta(f, X, n)(theta)))^2
  }
  psi <- array(dim = c(n,p,p))
  J <- do.call(jacobian, args = combine_lists(list(func = g, x = theta_hat),
                                              jacobian_args))
  for(i in 1:n){
    psi[i,,] <- tcrossprod(J[i,])
  }
  P_psi <- colSums(psi)
  
  psi_dot <- array(dim = c(n,p,p))
  # h <- function(theta, Y, X) (Y - f(X, theta))^2
  # for(i in 1:n){
  #   psi_dot[i,,] <-
  #     do.call(hessian, args = combine_lists(list(func = h, x = theta_hat,
  #                                                Y = Y[i], X = X[i,]),
  #                                           hessian_args))
  # }
  for(i in 1:n){
    g_i <- function(theta) g(theta)[i]
    psi_dot[i,,] <-
      do.call(hessian, args = combine_lists(list(func = g_i, x = theta_hat),
                                            hessian_args))
  }
  
  P_psi_dot_inv <- solve(colSums(psi_dot))
  
  return(P_psi_dot_inv %*% P_psi %*% t(P_psi_dot_inv))
}
