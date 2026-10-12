#' Solve continuous-time Lyapunov equations
#' 
#' \code{clyap} solve the continuous-time Lyapunov equations
#' \deqn{BX + XB' + C=0}
#' Using the Bartels-Stewart algorithm with Hessenberg–Schur 
#' decomposition. Optionally the Hessenberg-Schur 
#' decomposition can be returned. 
#' 
#' @param B Square matrix
#' @param C Square matrix
#' @param Q Square matrix, the orthogonal matrix used 
#' to transform the original equation
#' @param all logical
#' 
#' @return The solution matrix \code{X} if \code{all = FALSE}. If 
#'         \code{all = TRUE} a list with components \code{X}, \code{B}
#'         and \code{Q}. Where \code{B} and \code{Q} are the 
#'         Hessenberg-Schur form of the original matrix \code{B} 
#'         and the orthogonal matrix that performed the transformation.  
#' 
#' @details 
#' 
#' If the matrix \code{Q} is set then the matrix \code{B} 
#' is assumed to be in upper quasi-triangular form 
#' (Hessenberg-Schur canonical form),
#' as required by LAPACK subroutine \code{DTRSYL} and \code{Q} is 
#' the orthogonal matrix associated with the Hessenberg-Schur form 
#' of \code{B}. 
#' Usually the matrix \code{Q} and the appropriate form of \code{B}
#' are obtained by a first call to \code{clyap(B, C, all = TRUE)}
#' 
#' 
#' \code{clyap} uses lapack subroutines: 
#' 
#' * \code{DGEES}
#' * \code{DTRSYL} 
#' * \code{DGEMM}
#' @examples 
#' B <- matrix(data = rnorm(9), nrow = 3)
#' ## make B negative diagonally dominant, thus stable:
#' diag(B) <- - 3 * max(B) 
#' C <- diag(runif(3))
#' X <- clyap(B, C)
#' ## check X is a solution:
#' max(abs(B %*% X + X %*% t(B) + C)) 
#' @useDynLib gclm
#' @export
clyap <- function(B, C, Q = NULL, all = FALSE) {
  N <- ncol(B)
  JOB <-  0
  if (is.null(Q)) Q <- matrix(nrow = N, ncol = N, 0)
  else JOB <- 1
  WK <- rep(0, 5 * N)
  out <- .Fortran('DGELYP', as.integer(N),
                  as.double(B), as.double(-C),
                  as.double(Q),
                  as.double(WK),
                  as.integer(0),
                  as.integer(JOB), as.integer(0),
                  PACKAGE = "gclm")
  if (all){
    list(B = matrix(out[[2]], N, N),
         X = matrix(out[[3]], N, N), 
         Q = matrix(out[[4]], N, N), 
         INFO = out[[6]] )
  }else{
    matrix(out[[3]], N, N) 
  }
}


#' l1 penalized loss estimation for GCLM
#' 
#' Estimates a sparse continuous time Lyapunov 
#' parametrization of a covariance matrix using a lasso
#' (L1) penalty. 
#' 
#' @param Sigma covariance matrix
#' @param B initial B matrix
#' @param C diagonal of initial C matrix
#' @param C0 diagonal of penalization matrix
#' @param loss one of "loglik" (default) or "frobenius"
#' @param eps convergence threshold
#' @param alpha parameter line search 
#' @param maxIter maximum number of iterations
#' @param lambda penalization coefficient for B
#' @param lambdac penalization coefficient for C
#' @param job integer 0,1,10 or 11 
#' @return for \code{gclm}: a list with the result of the optimization
#' @details 
#' \code{gclm} performs proximal gradient descent for the optimization problem
#' \deqn{argmin L(\Sigma(B,C)) + \lambda \rho(B) + \lambda_C ||C - C0||_F^2}
#' subject to \eqn{B} stable and \eqn{C} diagonal, where \eqn{\rho(B)} is the l1 norm 
#' of the off-diagonal element of \eqn{B}.
#' @useDynLib gclm
#' @examples 
#' x <- matrix(rnorm(50*20),ncol=20)
#' S <- cov(x)
#' 
#' ## l1 penalized log-likelihood
#' res <- gclm(S, eps = 0, lambda = 0.1, lambdac = 0.01)
#' 
#' ## l1 penalized log-likelihood with fixed C
#' res <- gclm(S, eps = 0, lambda = 0.1, lambdac = -1)
#' 
#' ## l1 penalized frobenius loss
#' res <- gclm(S, eps = 0, lambda = 0.1, loss = "frobenius")
#' @export
gclm <- function(Sigma, B = - 0.5 * diag(ncol(Sigma)), 
                      C = rep(1, ncol(Sigma)),
                      C0 = rep(1, ncol(Sigma)),
                      loss = "loglik",
                      eps =  1e-2,
                      alpha = 0.5, 
                      maxIter = 100,
                      lambda = 0,
                      lambdac = 0,  
                      job = 0){
  if (loss == "loglik"){
    out <- .Fortran("GCLMLL",as.integer(ncol(Sigma)), as.double(Sigma), 
                    as.double(B), 
                    as.double(C), as.double(C0), 
                    as.double(lambda), as.double(lambdac), 
                    as.double(eps),
                    as.double(alpha),
                    as.integer(maxIter),
                    as.integer(job), 
                    PACKAGE = "gclm")    
  }
  if (loss == "frobenius"){
    out <- .Fortran("GCLMLS",as.integer(ncol(Sigma)), as.double(Sigma), 
                    as.double(B), 
                    as.double(C), as.double(C0), 
                    as.double(lambda), as.double(lambdac), 
                    as.double(eps),
                    as.double(alpha),
                    as.integer(maxIter),
                    as.integer(job), 
                    PACKAGE = "gclm")
  }

  names(out) <- c("N", "Sigma", "B", "C", "C0", "lambda", "lambdac",
                  "diff", "loss", 
                   "iter", "job")
  out$Sigma <- matrix(nrow = out$N, out$Sigma)
  out$B <- matrix(nrow = out$N, out$B)
  return(out)
}


#' @rdname gclm
#' @param lambdas sequence of lambda
#' @param ... additional arguments passed to \code{gclm}
#' @return for \code{gclm.path}: a list of the same length of
#'         \code{lambdas} with the results of the optimization for 
#'         the different \code{lambda} values
#' @details \code{gclm.path} simply calls iteratively \code{gclm}
#' with different \code{lambda} values. Warm start is used, that 
#' is in the i-th call to \code{gclm} the \code{B} and \code{C} 
#' matrices are initialized as the one obtained in the (i-1)th
#' call.    
#' @export
gclm.path <- function(Sigma, lambdas = NULL, 
                      B = - 0.5 * diag(ncol(Sigma)),
                      C = rep(1, ncol(Sigma)),
                      ...){
  if (is.null(lambdas)) {
    lambdas = seq(max(diag(Sigma)) / 2, 0, length = 10)
  }
  results <- list()
  for (i in 1:length(lambdas)){
    results[[i]] <- gclm(Sigma, B = B, C = C,
                         lambda = lambdas[i],  ...)
    B <- results[[i]]$B
    C <- results[[i]]$C
  }
  return(results)
}


#' Recover lower triangular GCLM
#'
#' Recover the only lower triangular stable matrix B such that 
#' \code{Sigma} (\eqn{\Sigma})
#' is the solution of the associated continuous Lyapunov equation:
#' \deqn{B\Sigma + \Sigma B' + C = 0}
#' @param Sigma covariance matrix
#' @param P the inverse of the  covariance matrix
#' @param C symmetric positive definite matrix 
#'
#' @return A stable lower triangular matrix
#'
#' @export
gclm.lowertri <- function(Sigma,
                          P = solve(Sigma),
                          C = diag(nrow = nrow(Sigma))){
  
  p <- nrow(Sigma)
  l <- listInverseBlocks(Sigma)
  blong <- anti_t(0.5 * C %*% P)[ upper.tri(Sigma)]
  blong <- blong[length(blong):1]
  b <- blong[1:(p-1)]
  w <- l[[1]] %*% b
  t <- p
  for (i in 2:length(l)){
    b <- blong[t:(t + p - i - 1)]
    t <- t + p - i
    AA <- matrix(nrow = length(b), ncol = length(w), 0)
    for (j in (i+1):p){
      for (k in 1:(i-1)){
        ix <- (k - 1) * p - (k - 1) * k / 2 + (i - k)
        AA[j - i, ix] <-  P[k, j]
      }
    }
    b <- b + tcrossprod(AA, t(w) )
    w <- c(w, l[[i]] %*% b)
  }
  W <- matrix(nrow = p, ncol = p, 0)
  W[upper.tri(W)] <- w[length(w) : 1]
  W <- anti_t(W)
  W <- W - t(W)
  B <- (W - 0.5 * C) %*% P
  B[upper.tri(B)] <- 0
  return(B)
}