#'  Logistic (Restricted) PCA
#'
#' This function runs:
#' logistic principal component analysis (if X = NULL)
#' logistic reduced rank regression (if X != NULL)
#'
#' @param Y An N times R binary matrix  .
#' @param X An N by P matrix with predictor variables
#' @param S Positive number indicating the dimensionality of the solution
#' @param dim.indic An R by S matrix indicating which response variable pertains to which dimension
#' @param eq Only applicable when dim.indic not NULL; equality restriction on regression weighhts per dimension
#' @param lambda if TRUE does lambda scaling (see Understanding Biplots, p24)
#' @param maxiter maximum number of iterations
#' @param dcrit convergence criterion
#'
#' @return This function returns an object of the class \code{lpca} with components:
#' \item{call}{Call to the function}
#' \item{Y}{Matrix Y from input}
#' \item{Xoriginal}{Matrix X from input}
#' \item{X}{Scaled X matrix}
#' \item{mx}{Mean values of X}
#' \item{sdx}{Standard deviations of X}
#' \item{ynames}{Variable names of responses}
#' \item{xnames}{Variable names of predictors}
#' \item{probabilities}{Estimated values of Y}
#' \item{m}{main effects}
#' \item{U}{matrix with coordinates for row-objects}
#' \item{B}{matrix with regression weight (U = XB)}
#' \item{V}{matrix with vectors for items/responses}
#' \item{iter}{number of main iterations from the MM algorithm}
#' \item{deviance}{value of the deviance at convergence}
#' \item{npar}{number of estimated parameters}
#' \item{AIC}{Akaike's Information Criterion}
#' \item{BIC}{Bayesian Information Criterion}
#'
#' @examples
#' \dontrun{
#' data(dataExample_lpca)
#' Y = as.matrix(dataExample_lpca[, 1:8])
#' X = as.matrix(dataExample_lpca[, 9:13])
#' # unsupervised
#' output = lpca(Y = Y, S = 2)
#' }
#'
#'
#' @importFrom stats plogis
#'
#' @export
lpca <- function(Y, X = NULL, S = 2, dim.indic = NULL, eq = FALSE, lambda = FALSE, maxiter = 65536, dcrit = 1e-6){


  cal = match.call()
  # checks
  if ( is.null( Y ) ) stop( "missing response variable matrix Y")
  if( ! is.null(X)) if ( nrow(Y) != nrow(X) ) stop( "number of rows in X and Y should match")
  if( S < 1) stop("dimensionality (S) should be larger than 1")
  if( maxiter < 1) stop("maximum number of iterations should be a positive number")
  if( dcrit < 0) stop("converence criterion should be a positive number")
  if( is.null(dim.indic) && eq == TRUE) stop("equality restriction can only be imposed when items pertian to dimensions")

  ynames = colnames(Y)

  # Data Coding
  Q = 2 * as.matrix(Y) - 1
  Q[is.na(Q)] <- 0 # passive treatment of missing responses
  N = nrow(Q)
  R = ncol(Q)

  ###############################################################
  # unsupervised analysis
  ###############################################################
  if( is.null(X) ){
    mx = NULL
    sdx = NULL
    B = NULL
    Xoriginal = NULL
    xnames = NULL

    # starting values
    m = colMeans(4 * Q)
    U = matrix(0, N, S)
    V = matrix(0, R, S)

    npar = (N + R - S) * S

    # compute deviance
    theta = outer(rep(1, N), m)
    dev.old <- -2 * sum(log(plogis(Q * theta)))

    for (iter in 1:maxiter) {
      # compute working response
      Z = as.matrix(theta + 4 * Q * (1 - plogis(Q * theta)))

      # update main effects
      m = as.numeric(colMeans(Z - U %*% t(V)))

      # update U and V
      udv = svd(scale(Z, center = m, scale = FALSE))
      U = matrix(udv$u[, 1:S], N, S) %*% diag(udv$d[1:S], nrow = S, ncol = S)
      V = matrix(udv$v[, 1:S], R, S)

      # compute deviance
      theta = outer(rep(1, N), m) + U %*% t(V)
      dev.new <- -2 * sum(log(plogis(Q * theta)))

      if ( ((dev.old - dev.new)/dev.old) < dcrit ) break
      if (iter == maxiter) warning("Maximum number of iterations reached - not converged (yet)")
      dev.old = dev.new
    }
    if(lambda){
      # lambda-scaling - see Gower/Le Roux & Lubbe, page 24
      ssqu = sum(U^2)
      ssqv = sum(V^2)
      lambda = (N/R) * ssqv/ssqu
      U = lambda*U
      V = V/lambda
    }
  }
  ###############################################################
  # supervised analysis
  ###############################################################
  if( !is.null(X) ){

    xnames = colnames(X)

    # items pertain to specified dimensions
    if( !is.null(dim.indic) ){
      if(eq){
        V = dim.indic
        iVVV= t(solve(t(V) %*% V) %*% t(V))
      }
      dim.indic = matrix(as.logical(dim.indic), R, S)
    }

    Xoriginal = X
    outx = procx(X)
    X = outx$X
    mx = outx$mx
    sdx = outx$sdx
    P = ncol(X)

    #
    eig.out = eigen(t(X) %*% X)
    Rx = eig.out$vectors %*% diag(sqrt(eig.out$values)) %*% t(eig.out$vectors)
    iXXX = solve(t(X) %*% X) %*% t(X)
    iRx = solve(Rx)
    iRxX = iRx %*% t(X)
	npar = (P + R - S) * S


    # starting values
    m = colMeans(4 * Q)
    B = matrix(0, P, S)
    V = matrix(0, R, S)

    # compute deviance
    theta = outer(rep(1, N), m)
    dev.old <- -2 * sum(log(plogis(Q * theta)))

    for (iter in 1:maxiter) {
      # compute working response
      Z = as.matrix(theta + 4 * Q * (1 - plogis(Q * theta)))

      # update main effects
      m = as.numeric(colMeans(Z - X %*% B %*% t(V)))

      # update B and V
      if( is.null(dim.indic) ){
        udv = svd(iRxX %*% scale(Z, center = m, scale = FALSE))
        B = iRx %*% matrix(udv$u[, 1:S], P, S) * sqrt(N)
        V = matrix(udv$v[, 1:S], R, S) %*% diag(udv$d[1:S], nrow = S, ncol = S) / sqrt(N)
      }

      if( !is.null(dim.indic) ){
        if(!eq){
          for(s in 1:S){
            Ztilde = scale(Z, center = m, scale = FALSE) - X%*%B[ , -s] %*% t(V[ , -s])
            udv = svd(iRxX %*% Ztilde[, dim.indic[ ,s]])
            B[ , s] = iRx %*% matrix(udv$u[, 1], P, 1) * sqrt(N)
            V[dim.indic[, s], s] = (udv$v[, 1] * udv$d[1]) / sqrt(N)
          }
        }
        if(eq) B = iXXX %*% scale(Z, center = m, scale = FALSE) %*% iVVV #old: if(eq) B = iXXX %*% scale(Z, center = mu, scale = FALSE) %*% iVVV
      }

      # compute deviance
      theta = outer(rep(1, N), m) + X %*% B %*% t(V)
      dev.new <- -2 * sum(log(plogis(Q * theta)))

      if (((dev.old - dev.new)/dev.old) < dcrit) break
      if (iter == maxiter) warning("Maximum number of iterations reached - not converged (yet)")
      dev.old = dev.new
    }
    U = X %*% B
  }

  # create output object of class "lpca"
  output = list(
	call = cal,
    Y = Y,
    Xoriginal = Xoriginal,
    X = X,
    mx = mx,
    sdx = sdx,
    ynames = ynames,
    xnames = xnames,
    probabilities = plogis(theta),
    m = m,
    U = U,
    B = B,
    V = V,
    iter = iter,
    deviance = dev.new,
    npar = npar,
    AIC = dev.new + 2 * npar,
    BIC = dev.new + log(N) * npar
  )
  class(output) = "lpca"
  return(output)
}
