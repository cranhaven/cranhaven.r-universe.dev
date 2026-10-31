#' Optimization on Stiefel manifold
#'
#' Curvilinear search algorithm for optimization on Stiefel manifold developed by Wen and Yin (2013).
#'
#' @param X Initial value to start the optimization. A \eqn{n} by \eqn{k} orthonormal matrix such that \eqn{X^T X = I_k}.
#' @param fun The function that returns the objective function value and its gradient. The syntax for \code{fun} is \code{fun(X, data1, data2)} where \code{data1, data2} are additional data passed to \code{...}.
#' @param opts A list specifying additional user-defined arguments for the curvilinear search algorithm. Some important ones are listed in the following:
#' \itemize{
#'  \item \code{maxiter}: The maximal number of iterations.
#'  \item \code{xtol}: The convergence tolerance for \eqn{X}, e.g., \eqn{||X^{(t)} - X^{(t-1)}||_F/\sqrt{k}}.
#'  \item \code{gtol}: The convergence tolerance for the gradient of the Lagrangian function, e.g., \eqn{||G^{(t)} - X^{(t)} (G^{(t)})^T X^{(t)}||_F}.
#'  \item \code{ftol}: The convergence tolerance for objective function \eqn{F}, e.g., \eqn{|F^{(t)} - F^{(t-1)}|/(1+|F^{(t-1)}|)}. Usually, \code{max{xtol, gtol} > ftol}.
#' }
#' The default values are: \code{maxiter=500; xtol=1e-08; gtol=1e-08; ftol=1e-12.}
#'
#' @param ... Additional input passed to \code{fun}.
#'
#' @return
#' \item{X}{The converged solution of the optimization problem.}
#' \item{out}{Output information, including estimation error, function value, iteration times etc.
#' \itemize{
#' \item{\code{nfe}}: The total number of line search attempts.
#' \item{\code{msg}}: Message: "convergence" | "exceed max iteration".
#' \item{\code{feasi}}: The feasibility of solution: \eqn{||X^TX - I_k||_F}.
#' \item{\code{nrmG}}: The convergence criterion based on the projected gradient \eqn{||G - X G^T X||_F}.
#' \item{\code{fval}}: The objective function value \eqn{F(X)} at termination.
#' \item{\code{iter}}: The number of iterations.
#' }}
#'
#' @details The calling syntax is \code{OptStiefelGBB(X, fun, opts, data1, data2)}, where \code{fun(X, data1, data2)} returns the objective function value and its gradient.
#'
#' For example, for \eqn{n} by \eqn{k} matrix \eqn{X}, the optimization problem is
#' \deqn{min_{X} -tr(X^T W X), \mbox{ such that } X^T X = I_k.}
#' The objective function and its gradient are
#' \deqn{F(X) = -tr(X^T W X), \; G(X) = - 2 W X.}
#' Then we need to provide the function \code{fun(X, W)} which returns \eqn{F(X)} and \eqn{G(X)}. See \strong{Examples} for details.
#'
#' For more details of the termination rules and the tolerances, we refer the interested readers to Section 5.1 of Wen and Yin (2013).
#'
#' @references Wen, Z. and Yin, W., 2013. A feasible method for optimization with orthogonality constraints. Mathematical Programming, 142(1-2), pp.397-434.
#'
#' @examples
#' n <- 1000
#' k <- 6
#'
#' # Randomly generated matrix M
#' W <- matrix(rnorm(n^2), n, n)
#' W <- t(W) %*% W
#'
#' # Randomly generated orthonormal initial matrix
#' X0 <- matrix(rnorm(n*k), n, k)
#' X0 <- qr.Q(qr(X0))
#'
#' # The objective function and its gradient
#' fun <- function(X, W){
#'   F <- - sum(diag(t(X) %*% W %*% X))
#'   G <- - 2*(W %*% X)
#'   return(list(F = F, G = G))
#' }
#'
#' # Options list
#' opts<-list(record = 0, maxiter = 1000, xtol = 1e-5, gtol = 1e-5, ftol = 1e-8)
#'
#' # Main part
#' output <- OptStiefelGBB(X0, fun, opts, W)
#' X <- output$X
#' out <- output$out
#'
#' @export

OptStiefelGBB <- function(X, fun, opts=NULL, ...){
  # Note: The notations follow the ones in algorithm described in Z. Wen and W. Yin. (2013)
  X <- as.matrix(X)
  if(length(X) == 0){
    print("input X is an empty matrix")
  }else{
    n <- dim(X)[1]
    k <- dim(X)[2]
  }

  if (is.null(opts$xtol) || opts$xtol < 0 || opts$xtol > 1) opts$xtol <- 1e-8
  if (is.null(opts$gtol) || opts$gtol < 0 || opts$gtol > 1) opts$gtol <- 1e-8
  if (is.null(opts$ftol) || opts$ftol < 0 || opts$ftol > 1) opts$ftol <- 1e-12
  # parameters for control the linear approximation in line search
  if (is.null(opts$rho) || opts$rho < 0 || opts$rho > 1) opts$rho <- 1e-04
  # factor for decreasing the step size in the backtracking line search
  if (is.null(opts$eta)){
    opts$eta <- 0.2
  }else if (opts$eta < 0 || opts$eta > 1){
    opts$eta <- 0.1
  }
  # parameters for updating C by HongChao, Zhang
  if (is.null(opts$gamma) || opts$gamma < 0 || opts$gamma > 1) opts$gamma <- 0.85
  if (is.null(opts$tau) || opts$tau < 0 || opts$tau > 1) opts$tau <- 1e-03
  #parameters for the  nonmontone line search by Raydan
  if (is.null(opts$STPEPS)) opts$STPEPS <- 1e-10
  if (is.null(opts$projG) || opts$projG != 1 && opts$projG != 2) opts$projG <- 1
  if (is.null(opts$iscomplex) || opts$iscomplex !=0 && opts$iscomplex != 1) opts$iscomplex <- 0
  if (is.null(opts$maxiter) || opts$maxiter < 0 || opts$maxiter > 2^20) opts$maxiter <- 500
  if (is.null(opts$nt) || opts$nt < 0 || opts$nt > 100) opts$nt <- 5
  if (is.null(opts$record)) opts$record <- 0

  # copy parameters
  xtol <- opts$xtol
  gtol <- opts$gtol
  ftol <- opts$ftol
  rho  <- opts$rho
  STPEPS <- opts$STPEPS
  eta <- opts$eta
  gamma <- opts$gamma
  iscomplex <- opts$iscomplex
  record <- opts$record

  nt <- opts$nt
  crit <- matrix(1, opts$maxiter, 3)
  invH <- TRUE
  if (k < n/2){
    invH <- FALSE
    eye2k <- diag(2*k)
  }

  ## Initial function value and gradient
  # prepare for iterations
  args = list(X, ...)
  eva <- do.call(fun, args)
  F <- eva$F; G <- eva$G
  out <- c()
  out$nfe <- 1    # The total number of line search attempts.
  GX <- crossprod(G, X)

  if (invH == TRUE) {
    GXT <- tcrossprod(G, X)
    H <- 0.5*(GXT - t(GXT))
    RX <- H %*% X
  } else {
    if (opts$projG == 1) {
      U <- cbind(G, X)
      V <- cbind(X, -G)
      VU <- crossprod(V, U)
    }else if (opts$projG == 2){
      GB <- G - 0.5* tcrossprod(X) %*% G
      U <- cbind(GB, X)
      V <- cbind(X, -GB)
      VU <- crossprod(V, U)
    }
    VX <- crossprod(V, X)
  }
  dtX <- G - X %*% GX   # dtX = g - X %*% g^T %*% X
  nrmG <- sqrt(sum(dtX^2))   # nrmG = ||g - X %*% g^T %*% X||_F

  Q <- 1
  Cval <- F
  tau <- opts$tau

  ## print iteration header if debug == 1
  if (opts$record == 1){
    cat(paste('------ Gradient Method with Line search ----- ',"\n"),
        sprintf("%4s %8s %8s %10s %10s", 'Iter', 'tau', 'F(X)', 'nrmG', 'XDiff'))
  }

  ## main iteration
  for (itr in 1:opts$maxiter) {
    # Record the values from the last step
    XP <- X
    FP <- F
    GP <- G
    dtXP <- dtX

    # line search: scale step size
    nls <- 1   # The number of line search attempts in each iteration
    deriv <- rho*nrmG^2
    while (TRUE) {
      if (invH == TRUE){
        X <- solve(diag(n) + tau*H, XP - tau*RX)
      } else {
        aa <- solve(eye2k + (0.5*tau)*VU, VX)
        X <- XP - U %*% (tau*aa)
      }
      if(iscomplex == 0 && is.complex(X) == 1) cat("error: X is complex")

      args <- list(X, ...)
      eva <- do.call(fun, args)
      F <- eva$F
      G <- eva$G
      out$nfe <- out$nfe + 1     # The total number of line search attempts.

      if (F <= Cval - tau*deriv || nls >= 5)
        break
      tau <- eta*tau
      nls <- nls + 1
    }

    GX <- crossprod(G, X)
    if(invH == TRUE){
      GXT <- tcrossprod(G, X)
      H <- 0.5*(GXT - t(GXT))
      RX <- H %*% X
    }else{
      if(opts$projG == 1){
        U <- cbind(G, X)
        V <- cbind(X, -G)
        VU <- crossprod(V, U)
      }else if (opts$projG == 2){
        GB <- G - 0.5 * tcrossprod(X) %*% G
        U <- cbind(GB, X)
        V <- cbind(X, -GB)
        VU <- crossprod(V, U)
      }
      VX <- crossprod(V, X)
    }
    dtX <- G - X %*% GX    # dtX = g - X %*% g^T %*% X
    nrmG <- sqrt(sum(dtX^2))   # nrmG = ||g - X %*% g^T %*% X||_F
    S <- X - XP
    XDiff <- sqrt(sum(S^2))/sqrt(n)    # The change of X: ||X^(t) - X^(t-1)||_F/sqrt(k)
    tau <- opts$tau
    FDiff <- abs(FP - F)/(abs(FP) + 1)     # The relative change of the objective function f: |f^(t) - f^(t-1)|/(|f^(t-1)| + 1)

    if (iscomplex == 1) {
      Y <- dtX - dtXP; SY <- abs(sum(Conj(S)*Y))
      if (itr %% 2 == 0) {
        tau <- sum(Conj(S)*S)/SY } else {
          tau <- SY/sum(Conj(Y)*Y)
        }
    } else {
      Y <- dtX - dtXP
      SY <- abs(sum(S*Y))
      if (itr %% 2 == 0){
        tau <- sum(S*S)/SY
        }else{
          tau <- SY/sum(Y*Y)
        }
    }

    if (is.na(tau))
      tau <- opts$tau

    tau <- max(min(tau, 1e+20), 1e-20)

    if (record >= 1)
      sprintf("%4s  %3.2s  %4.3s  %3.2s  %3.2s  %3.2s  %2s",
              'iter', 'tau', 'F', 'nrmG', 'XDiff','FDiff', 'nls')

    crit[itr,] <- cbind(nrmG, XDiff, FDiff)
    r <- length((itr - min(nt, itr)+1):itr)
    tmp <- matrix(crit[(itr - min(nt, itr)+1):itr,], r, 3)
    mcrit <- colMeans(tmp)

    if ((XDiff < xtol && FDiff < ftol) || nrmG < gtol || all(mcrit[2:3] < 10 * c(xtol, ftol))) {
      if (itr <= 2) {
        ftol <- 0.1*ftol;
        xtol <- 0.1*xtol;
        gtol <- 0.1*gtol;
      } else {
        out$msg = "converge"
        break
      }
    }

    Qp <- Q; Q <- gamma*Qp + 1; Cval <- (gamma*Qp*Cval + F)/Q
  }

  if (itr >= opts$maxiter)
    out$msg = "exceed max iteration"

  out$feasi <- sqrt(sum((crossprod(X)- diag(k))^2))    # Check the feasibility of X (i.e., the orthogonality) feasi = ||X^T X - I_k||_F.
  if (out$feasi > 1e-13) {  # If X is not close to identity, then do one more step
    X <- qr.Q(qr(X))
    args <- list(X, ...)
    eva <- do.call(fun, args)
    F <- eva$F; G <- eva$G
    out$nfe <- out$nfe + 1
    out$feasi <- sqrt(sum((crossprod(X)- diag(k))^2))
  }
  out$nrmG <- nrmG
  out$fval <- F
  out$itr <- itr
  return(list(X = X, out = out))
}
