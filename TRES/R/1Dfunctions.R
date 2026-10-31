## The core functions for 1D algorithm based functions: OptM1D, manifold1D, oneD_bic, TRRdim

##################################################
#         1D optimization function               #
##################################################
fun1D <- function(W, M, U){
  f <- log(t(W) %*% M %*% W) + log(t(W) %*% chol2inv(chol(M+U)) %*% W)
  df <- 2*(M %*% W/(as.numeric(t(W) %*% M %*% W))+
             solve(M+U) %*% W/(as.numeric(t(W) %*% chol2inv(chol(M+U)) %*% W)))
  list(F = f, G = df)
}

##################################################
#    get initial value for 1D algorithm          #
##################################################
get_ini1D <- function(M, U){
  p <- dim(U)[2]
  v1 <- eigen(M)$vectors
  v2 <- eigen(M+U)$vectors
  v <- cbind(v1, v2)
  index <- v[1,] < 0
  v[,index] <- -v[,index]
  W0 <- Re(v[, 1]) ## Ensure the real number
  Fw0 <- fun1D(W0, M, U)$F
  for (i in 2:(2*p)) {
    W <- Re(v[, i])
    Fw <- fun1D(W, M, U)$F
    if (Fw < Fw0) {
      W0 <- W
      Fw0 <- Fw
    }
  }
  W0
}

################################################################
#         1D solver for individual objective function          #
#         using function OptManiMulitBallGBB                   #
################################################################
ballGBB1D <- function(M, U, ...) {

  # Options for function OptManiMulitBallGBB
  opts <- list(...)
  W0 <- get_ini1D(M, U)
  if (is.null(opts$xtol) || opts$xtol < 0 || opts$xtol > 1) opts$xtol <- 1e-8
  if (is.null(opts$gtol) || opts$gtol < 0 || opts$gtol > 1) opts$gtol <- 1e-8
  if (is.null(opts$ftol) || opts$ftol < 0 || opts$ftol > 1) opts$ftol <- 1e-12
  # parameters for control the linear approximation in line search
  if (is.null(opts$rho) || opts$rho < 0 || opts$rho > 1) opts$rho <- 1e-04
  # factor for decreasing the step size in the backtracking line search
  if (is.null(opts$eta))
    opts$eta <- 0.2 else if (opts$eta < 0 || opts$eta > 1)
      opts$eta <- 0.1
  # parameters for updating C by HongChao, Zhang
  if (is.null(opts$gamma) || opts$gamma < 0 || opts$gamma > 1) opts$gamma <- 0.85
  if (is.null(opts$tau) || opts$tau < 0 || opts$tau > 1) opts$tau <- 1e-03
  # parameters for the  nonmontone line search by Raydan
  if (is.null(opts$m)) opts$m <- 10
  if (is.null(opts$STPEPS)) opts$STPEPS <- 1e-10
  if (is.null(opts$maxiter) || opts$maxiter < 0 || opts$maxiter > 2^20) opts$maxiter <- 800
  if (is.null(opts$nt) || opts$nt < 0 || opts$nt > 100) opts$nt <- 5
  # if (is.null(opts$record)) opts$record <- 0
  if (is.null(opts$eps)) opts$eps <- 1e-14

  fit <- OptManiMulitBallGBB(W0, fun1D, opts, M, U)
  list(X = fit$X, out = fit$out)
}

###########################################################################
#         Line search algorithm for optimization on manifold              #
###########################################################################
OptManiMulitBallGBB <- function(X, fun, opts=NULL, ...) {
  # Line search algorithm for optimization on manifold:
  #
  #    min f(X), s.t., ||X_i||_2 = 1, where X \in R^{n,p}
  #        g(X) = grad f(X)
  #    X = [X_1, X_2, ..., X_p]
  #    each column of X lies on a unit sphere

  # Input:
  #    X --- initialization. ||X_i||_2 = 1, each column of X lies on a unit sphere
  #    opts --- option structure with fields:
  #       maxiter     max number of iterations
  #       xtol        stop control for ||X^(t) - X^(t-1)||_F/sqrt(k)
  #       gtol        stop control for the projected gradient
  #       ftol        stop control for |F_k - F_{k-1}|/(1+|F_{k-1}|)
  #       usually, max{xtol, gtol} > ftol
  #    fun --- objective function and its gradient:
  #    [X, g, out] = fun(X, data1, data2)
  #
  #    X and g are the objective function value and gradient, repectively
  #    out is the additional iteration information
  #    data1, data2 are addtional data, and can be more.
  #
  # Calling syntax:
  #    OptManiMulitBallGBB(X0, fun, opts, data1, data2);
  #
  # Output:
  #     X --- solution
  #     g --- gradient of X
  #     out --- output information:
  #               nfe: The total number of line search attempts.
  #               msg: "convergence" | "exceed max iteration"
  #               feasi: ||(||X_1||_2^2-1), ..., (||X_k||_2^2 - 1)||_2,
  #               nrmG: ||X_1*<X_1, g_1> - g_1, ..., X_k*<X_k, g_k> - g_k||_F
  #               fval: the objective function value at termination
  #
  # For example, consider the maxcut SDP:
  #     X is n by n matrix
  #     max Tr(C*X), s.t., X_ii = 1, X is PSD.
  #
  #     low rank model is:
  #         X = V'*V, V = [V_1, ..., V_n], V is a p by n matrix
  #         max Tr(C*V'*V), s.t., ||V_i|| = 1,
  #
  #     # Define the function returning objective and the gradient:
  #     maxcut_quad <- function(V, C){
  #       g = 2*(V*C)
  #       f = sum(dot(g,V))/2
  #       return(list(F = f, G = g))
  #     }
  #
  #     # Call function OptManiMulitBallGBB
  #     OptManiMulitBallGBB(x0, maxcut_quad, opts, C);
  #
  #
  #     Reference: Z. Wen and W. Yin (2013), A feasible method for optimization
  #       with orthogonality constraints
  #

  ##Size information
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
  # parameters for the nonmontone line search by Raydan
  if (is.null(opts$m)) opts$m <- 10
  if (is.null(opts$STPEPS)) opts$STPEPS <- 1e-10
  if (is.null(opts$maxiter) || opts$maxiter < 0 || opts$maxiter > 2^20) opts$maxiter <- 800
  if (is.null(opts$nt) || opts$nt < 0 || opts$nt > 100) opts$nt <- 5
  if (is.null(opts$eps)) opts$eps <- 1e-14
  if (is.null(opts$record)) opts$record <- 0

  # copy parameters
  xtol <- opts$xtol
  gtol <- opts$gtol
  ftol <- opts$ftol
  rho  <- opts$rho
  m <- opts$m
  STPEPS <- opts$STPEPS
  eta <- opts$eta
  gamma <- opts$gamma
  eps <- opts$eps
  nt <- opts$nt
  crit <- matrix(1, opts$maxiter, 3)
  record <- opts$record

  # normalize x such that ||x||_2 = 1
  nrmX <- apply(X*X, 2, sum)
  nrmX <- matrix(nrmX, 1, k)  # nrmX = (||X_1||_2^2, .... ||X_k||_2^2)
  if (sqrt(sum((nrmX-1)^2)) > 1e-8) {
    X <- sweep(X, 2, sqrt(nrmX),"/")  # ||X_i||_2 = 1, i = 1, ..., k
  }
  args = list(X, ...)
  eva <- do.call(fun, args)
  f <- eva$F; g <- as.matrix(eva$G)
  out <- c()
  out$nfe <- 1


  Xtg <- apply(X*g, 2, sum)
  Xtg <- matrix(Xtg, 1, k)  # Xtg = (<X_1, g_1>, ..., <X_k, g_k>)
  gg <- apply(g*g, 2, sum)
  gg <- matrix(gg, 1, k)  # gg = (||g_1||_2^2, ..., |g_k||_2^2)
  XX <- apply(X*X, 2, sum)
  XX <- matrix(XX, 1, k)  # XX = (||X_1||_2^2, .... ||X_k||_2^2)
  XXgg <- XX*gg
  temp <- sweep(X, 2, Xtg, "*")  # temp = (X_1*<X_1, g_1>, ..., X_k*<X_k, g_k>)
  dtX <- matrix(temp, n, k) - g   # dtX = (X_1*<X_1, g_1> - g_1, ..., X_k*<X_k, g_k> - g_k)
  nrmG <- sqrt(sum((dtX)^2))  # nrmG = ||X_1*<X_1, g_1> - g_1, ..., X_k*<X_k, g_k> - g_k||_F

  Q <- 1; Cval <- f; tau <- opts$tau

  ## print iteration header if record >= 1
  if (record >= 1){
    cat(paste("\n", '------ Gradient Method with Line search ----- ',"\n"),
        sprintf("%4s %8s %10s %9s %9s %3s", 'Iter', 'tau', 'F(X)', 'nrmG', 'XDiff', 'nls'), "\n")
  }
  if (record == 10) out$fvec = f

  # ##
  # X_list <- vector("list", opts$maxiter)
  # XDiff_list <- vector("list", opts$maxiter)
  # FDiff_list <- vector("list", opts$maxiter)
  # Q_list <- vector("list", opts$maxiter)
  # Cval_list <- vector("list", opts$maxiter)
  # ##

  ##main iteration
  for (itr in 1:opts$maxiter) {
    Xp <- X; fp <- f; gp <- g; dtXP <- dtX   # Record the values from the last step.
    nls <- 1  # The number of line search attempts in each iteration.
    deriv <- rho*nrmG^2

    # Update X via line search
    while (TRUE) {
      tau2 <- tau/2
      beta <- (1 + (tau2^2)*(-(Xtg^2) + XXgg))
      a1 <- ((1 + tau2*Xtg)^2 - (tau2^2)*XXgg)/beta
      a2 <- -tau*XX/beta
      X <- sweep(Xp, 2, a1, "*") + sweep(gp, 2, a2, "*")   # The update of X

      args = list(X, ...)
      eva <- do.call(fun, args)
      f <- eva$F; g <- as.matrix(eva$G)
      out$nfe <- out$nfe + 1    # The total number of line search attempts.

      if (f <= Cval - tau*deriv || nls >= 5)
        break
      tau <- eta * tau   # Adjust the step size "tau"
      nls <- nls + 1
    }

    if (record == 10)
      out$fvec <- rbind(out$fvec,f)

    Xtg <- apply(X*g, 2, sum)
    Xtg <- matrix(Xtg, 1, k)
    gg <- apply(g*g, 2, sum)
    gg <- matrix(gg, 1, k)
    XX <- apply(X*X, 2, sum)
    XX <- matrix(XX, 1, k)
    XXgg <- XX*gg
    temp <- sweep(X, 2, Xtg, "*")
    dtX <- matrix(temp, n, k) - g

    nrmG <- sqrt(sum(dtX^2))
    s <- X - Xp
    XDiff <- sqrt(sum(s^2))/sqrt(n)    # The change of X: ||X^(t) - X^(t-1)||_F/sqrt(k)
    FDiff <- abs(fp - f)/(abs(fp) + 1)  # The relative change of the objective function f: |f^(t) - f^(t-1)|/(|f^(t-1)| + 1)

    if (record >= 1)
      # cat(paste('------ Gradient Method with Line search ----- ',"\n"),
      #     sprintf("%4s %8s %8s %10s %10s %10s", 'Iter', 'tau', 'F(X)', 'nrmG', 'XDiff','nls'))
      cat(sprintf('%4d  %3.2e  %3.2e  %3.2e  %3.2e %2d',
                  itr, tau, f, nrmG, XDiff, nls), "\n")

    crit[itr,] <- cbind(nrmG, XDiff, FDiff)
    r <- length((itr - min(nt, itr)+1):itr)
    temp1 <- matrix(crit[(itr - min(nt, itr)+1):itr,], r, 3)
    mcrit <- apply(temp1, 2, mean)


    if ((XDiff < xtol && FDiff < ftol) || nrmG < gtol
        || all(mcrit[2:3] < 10 * c(xtol, ftol))) {
      out$msg = "converge"
      break
    }

    y <- dtX - dtXP
    sy <- sum(s*y)
    tau <- opts$tau
    sy <- abs(sy)

    if (sy > 0) {
      if (itr %% 2 == 0)
        tau <- sum(s*s)/sy else { tau <- sy/sum(y*y)}

      tau <- max(min(tau, 1e+20), 1e-20)
    }
    Qp <- Q; Q <- gamma*Qp + 1
    Cval <- (gamma*Qp*Cval + f)/Q
  }

  if (itr >= opts$maxiter)
    out$msg <- "exceed max iteration"

  Xn <- apply(X*X, 2, sum)   # Xn = (||X_1||_2^2, ..., ||X_k||_2^2)
  Xn <- matrix(Xn, 1, k)
  out$feasi <- svd(Xn - 1)$d[1]    # The feasibility of the solution: feasi = ||X_n - 1||_2

  if (out$feasi > eps) { # If columns of Xn are not all close to zero, then do one more step
    nrmX <- apply(X*X, 2, sum)
    X <- sweep(X, 2, sqrt(nrmX),"/")
    args = list(X, ...)
    eva <- do.call(fun, args)
    f <- eva$F; g <- as.matrix(eva$G)
    out$nfe <- out$nfe + 1
    nrmX.n <- apply(X*X, 2, sum)
    out$feasi <- svd(nrmX.n - 1)$d[1]
  }

  out$nrmG <- nrmG
  out$fval <- f
  out$itr <- itr

  return(list(X = X, g = g, out = out))
}
