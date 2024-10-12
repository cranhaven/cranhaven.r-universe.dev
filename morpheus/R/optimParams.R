#' optimParams
#'
#' Wrapper function for OptimParams class
#'
#' @name optimParams
#'
#' @param X Data matrix of covariables
#' @param Y Output as a binary vector
#' @param K Number of populations.
#' @param link The link type, 'logit' or 'probit'.
#' @param M the empirical cross-moments between X and Y (optional)
#' @param nc Number of cores (default: 0 to use all)
#'
#' @return An object 'op' of class OptimParams, initialized so that
#'   \code{op$run(theta0)} outputs the list of optimized parameters
#'   \itemize{
#'     \item p: proportions, size K
#'     \item beta: regression matrix, size dxK
#'     \item b: intercepts, size K
#'   }
#'   theta0 is a list containing the initial parameters. Only beta is required
#'   (p would be set to (1/K,...,1/K) and b to (0,...0)).
#'
#' @seealso \code{multiRun} to estimate statistics based on beta, and
#'   \code{generateSampleIO} for I/O random generation.
#'
#' @examples
#' # Optimize parameters from estimated mu
#' io <- generateSampleIO(100,
#'   1/2, matrix(c(1,-2,3,1),ncol=2), c(0,0), "logit")
#' mu <- computeMu(io$X, io$Y, list(K=2))
#' o <- optimParams(io$X, io$Y, 2, "logit")
#' \dontrun{
#' theta0 <- list(p=1/2, beta=mu, b=c(0,0))
#' par0 <- o$run(theta0)
#' # Compare with another starting point
#' theta1 <- list(p=1/2, beta=2*mu, b=c(0,0))
#' par1 <- o$run(theta1)
#' # Look at the function values at par0 and par1:
#' o$f( o$linArgs(par0) )
#' o$f( o$linArgs(par1) )}
#'
#' @export
optimParams <- function(X, Y, K, link=c("logit","probit"), M=NULL, nc=0)
{
  # Check arguments
  if (!is.matrix(X) || any(is.na(X)))
    stop("X: numeric matrix, no NAs")
  if (!is.numeric(Y) || any(is.na(Y)) || any(Y!=0 & Y!=1))
    stop("Y: binary vector with 0 and 1 only")
  link <- match.arg(link)
  if (!is.numeric(K) || K!=floor(K) || K < 2 || K > ncol(X))
    stop("K: integer >= 2, <= d")

  if (is.null(M))
  {
    # Precompute empirical moments
    Mtmp <- computeMoments(X, Y)
    M1 <- as.double(Mtmp[[1]])
    M2 <- as.double(Mtmp[[2]])
    M3 <- as.double(Mtmp[[3]])
    M <- c(M1, M2, M3)
  }
  else
    M <- c(M[[1]], M[[2]], M[[3]])

  # Build and return optimization algorithm object
  methods::new("OptimParams", "li"=link, "X"=X,
    "Y"=as.integer(Y), "K"=as.integer(K), "Mhat"=as.double(M), "nc"=as.integer(nc))
}

# Encapsulated optimization for p (proportions), beta and b (regression parameters)
#
# Optimize the parameters of a mixture of logistic regressions model, possibly using
# \code{mu <- computeMu(...)} as a partial starting point.
#
# @field li Link function, 'logit' or 'probit'
# @field X Data matrix of covariables
# @field Y Output as a binary vector
# @field Mhat Vector of empirical moments
# @field K Number of populations
# @field n Number of sample points
# @field d Number of dimensions
# @field nc Number of cores (OpenMP //)
# @field W Weights matrix (initialized at identity)
#
setRefClass(
  Class = "OptimParams",

  fields = list(
    # Inputs
    li = "character", #link function
    X = "matrix",
    Y = "numeric",
    Mhat = "numeric", #vector of empirical moments
    # Dimensions
    K = "integer",
    n = "integer",
    d = "integer",
    nc = "integer",
    # Weights matrix (generalized least square)
    W = "matrix"
  ),

  methods = list(
    initialize = function(...)
    {
      "Check args and initialize K, d, W"

      callSuper(...)
      if (!hasArg("X") || !hasArg("Y") || !hasArg("K")
        || !hasArg("li") || !hasArg("Mhat") || !hasArg("nc"))
      {
        stop("Missing arguments")
      }

      n <<- nrow(X)
      d <<- ncol(X)
      # W will be initialized when calling run()
    },

    expArgs = function(v)
    {
      "Expand individual arguments from vector v into a list"

      list(
        # p: dimension K-1, need to be completed
        "p" = c(v[1:(K-1)], 1-sum(v[1:(K-1)])),
        "beta" = t(matrix(v[K:(K+d*K-1)], ncol=d)),
        "b" = v[(K+d*K):(K+(d+1)*K-1)])
    },

    linArgs = function(L)
    {
      "Linearize vectors+matrices from list L into a vector"

      # beta linearized row by row, to match derivatives order
      c(L$p[1:(K-1)], as.double(t(L$beta)), L$b)
    },

    # TODO: relocate computeW in utils.R
    computeW = function(theta)
    {
      "Compute the weights matrix from a parameters list"

      require(MASS)
      dd <- d + d^2 + d^3
      M <- Moments(theta)
      Omega <- matrix( .C("Compute_Omega",
        X=as.double(X), Y=as.integer(Y), M=as.double(M),
        pnc=as.integer(nc), pn=as.integer(n), pd=as.integer(d),
        W=as.double(W), PACKAGE="morpheus")$W, nrow=dd, ncol=dd )
      MASS::ginv(Omega)
    },

    Moments = function(theta)
    {
      "Compute the vector of theoretical moments (size d+d^2+d^3)"

      p <- theta$p
      beta <- theta$beta
      lambda <- sqrt(colSums(beta^2))
      b <- theta$b

      # Tensorial products beta^2 = beta2 and beta^3 = beta3 must be computed from current beta1
      beta2 <- apply(beta, 2, function(col) col %o% col)
      beta3 <- apply(beta, 2, function(col) col %o% col %o% col)

      c(
        beta  %*% (p * .G(li,1,lambda,b)),
        beta2 %*% (p * .G(li,2,lambda,b)),
        beta3 %*% (p * .G(li,3,lambda,b)))
    },

    f = function(theta)
    {
      "Function to minimize: t(hat_Mi - Mi(theta)) . W . (hat_Mi - Mi(theta))"

      L <- expArgs(theta)
      A <- as.matrix(Mhat - Moments(L))
      t(A) %*% W %*% A
    },

    grad_f = function(theta)
    {
      "Gradient of f: vector of size (K-1) + d*K + K = (d+2)*K - 1"

      L <- expArgs(theta)
      -2 * t(grad_M(L)) %*% W %*% as.matrix(Mhat - Moments(L))
    },

    grad_M = function(theta)
    {
      "Gradient of the moments vector: matrix of size d+d^2+d^3 x K-1+K+d*K"

      p <- theta$p
      beta <- theta$beta
      lambda <- sqrt(colSums(beta^2))
      mu <- sweep(beta, 2, lambda, '/')
      b <- theta$b

      res <- matrix(nrow=nrow(W), ncol=0)

      # Tensorial products beta^2 = beta2 and beta^3 = beta3 must be computed from current beta1
      beta2 <- apply(beta, 2, function(col) col %o% col)
      beta3 <- apply(beta, 2, function(col) col %o% col %o% col)

      # Some precomputations
      G1 = .G(li,1,lambda,b)
      G2 = .G(li,2,lambda,b)
      G3 = .G(li,3,lambda,b)
      G4 = .G(li,4,lambda,b)
      G5 = .G(li,5,lambda,b)

      # Gradient on p: K-1 columns, dim rows
      km1 = 1:(K-1)
      res <- cbind(res, rbind(
        sweep(as.matrix(beta [,km1]), 2, G1[km1], '*') - G1[K] * beta [,K],
        sweep(as.matrix(beta2[,km1]), 2, G2[km1], '*') - G2[K] * beta2[,K],
        sweep(as.matrix(beta3[,km1]), 2, G3[km1], '*') - G3[K] * beta3[,K] ))

      for (i in 1:d)
      {
        # i determines the derivated matrix dbeta[2,3]

        dbeta_left <- sweep(beta, 2, p * G3 * beta[i,], '*')
        dbeta_right <- matrix(0, nrow=d, ncol=K)
        block <- i
        dbeta_right[block,] <- dbeta_right[block,] + 1
        dbeta <- dbeta_left + sweep(dbeta_right, 2,  p * G1, '*')

        dbeta2_left <- sweep(beta2, 2, p * G4 * beta[i,], '*')
        dbeta2_right <- do.call( rbind, lapply(1:d, function(j) {
          sweep(dbeta_right, 2, beta[j,], '*')
        }) )
        block <- ((i-1)*d+1):(i*d)
        dbeta2_right[block,] <- dbeta2_right[block,] + beta
        dbeta2 <- dbeta2_left + sweep(dbeta2_right, 2, p * G2, '*')

        dbeta3_left <- sweep(beta3, 2, p * G5 * beta[i,], '*')
        dbeta3_right <- do.call( rbind, lapply(1:d, function(j) {
          sweep(dbeta2_right, 2, beta[j,], '*')
        }) )
        block <- ((i-1)*d*d+1):(i*d*d)
        dbeta3_right[block,] <- dbeta3_right[block,] + beta2
        dbeta3 <- dbeta3_left + sweep(dbeta3_right, 2, p * G3, '*')

        res <- cbind(res, rbind(dbeta, dbeta2, dbeta3))
      }

      # Gradient on b
      res <- cbind(res, rbind(
        sweep(beta,  2, p * G2, '*'),
        sweep(beta2, 2, p * G3, '*'),
        sweep(beta3, 2, p * G4, '*') ))

      res
    },

    # userW allows to bypass the W optimization by giving a W matrix
    run = function(theta0, userW=NULL)
    {
      "Run optimization from theta0 with solver..."

      if (!is.list(theta0))
        stop("theta0: list")
      if (is.null(theta0$beta))
        stop("At least theta0$beta must be provided")
      if (!is.matrix(theta0$beta) || any(is.na(theta0$beta))
        || nrow(theta0$beta) != d || ncol(theta0$beta) != K)
      {
        stop("theta0$beta: matrix, no NA, nrow = d, ncol = K")
      }
      if (is.null(theta0$p))
        theta0$p = rep(1/K, K-1)
      else if (!is.numeric(theta0$p) || length(theta0$p) != K-1
        || any(is.na(theta0$p)) || sum(theta0$p) > 1)
      {
        stop("theta0$p: length K-1, no NA, positive integers, sum to <= 1")
      }
      # NOTE: [["b"]] instead of $b because $b would match $beta (in pkg-cran)
      if (is.null(theta0[["b"]]))
        theta0$b = rep(0, K)
      else if (!is.numeric(theta0$b) || length(theta0$b) != K || any(is.na(theta0$b)))
        stop("theta0$b: length K, no NA")

      # (Re)Set W to identity, to allow several run from the same object
      W <<- if (is.null(userW)) diag(d+d^2+d^3) else userW

      # NOTE: loopMax = 3 seems to not improve the final results.
      loopMax <- ifelse(is.null(userW), 2, 1)
      x_init <- linArgs(theta0)
      for (loop in 1:loopMax)
      {
        op_res <- constrOptim( x_init, .self$f, .self$grad_f,
          ui=cbind(
            rbind( rep(-1,K-1), diag(K-1) ),
            matrix(0, nrow=K, ncol=(d+1)*K) ),
          ci=c(-1,rep(0,K-1)) )
        if (loop < loopMax) #avoid computing an extra W
          W <<- computeW(expArgs(op_res$par))
        #x_init <- op_res$par #degrades performances (TODO: why?)
      }

      expArgs(op_res$par)
    }
  )
)

# Compute vectorial E[g^{(order)}(<beta,x> + b)] with x~N(0,Id) (integral in R^d)
#                 = E[g^{(order)}(z)] with z~N(b,diag(lambda))
# by numerically evaluating the integral.
#
# @param link Link, 'logit' or 'probit'
# @param order Order of derivative
# @param lambda Norm of columns of beta
# @param b Intercept
#
.G <- function(link, order, lambda, b)
{
  # NOTE: weird "integral divergent" error on inputs:
  # link="probit"; order=2; lambda=c(531.8099,586.8893,523.5816); b=c(-118.512674,-3.488020,2.109969)
  # Switch to pracma package for that (but it seems slow...)
  sapply( seq_along(lambda), function(k) {
    res <- NULL
    tryCatch({
      # Fast code, may fail:
      res <- stats::integrate(
        function(z) .deriv[[link]][[order]](lambda[k]*z+b[k]) * exp(-z^2/2) / sqrt(2*pi),
        lower=-Inf, upper=Inf )$value
    }, error = function(e) {
      # Robust slow code, no fails observed:
      sink("/dev/null") #pracma package has some useless printed outputs...
      res <- pracma::integral(
        function(z) .deriv[[link]][[order]](lambda[k]*z+b[k]) * exp(-z^2/2) / sqrt(2*pi),
        xmin=-Inf, xmax=Inf, method="Kronrod")
      sink()
    })
    res
  })
}

# Derivatives list: g^(k)(x) for links 'logit' and 'probit'
#
.deriv <- list(
  "probit"=list(
    # 'probit' derivatives list;
    # NOTE: exact values for the integral E[g^(k)(lambdaz+b)] could be computed
    function(x) exp(-x^2/2)/(sqrt(2*pi)),                     #g'
    function(x) exp(-x^2/2)/(sqrt(2*pi)) *  -x,               #g''
    function(x) exp(-x^2/2)/(sqrt(2*pi)) * ( x^2 - 1),        #g^(3)
    function(x) exp(-x^2/2)/(sqrt(2*pi)) * (-x^3 + 3*x),      #g^(4)
    function(x) exp(-x^2/2)/(sqrt(2*pi)) * ( x^4 - 6*x^2 + 3) #g^(5)
  ),
  "logit"=list(
    # Sigmoid derivatives list, obtained with http://www.derivative-calculator.net/
    # @seealso http://www.ece.uc.edu/~aminai/papers/minai_sigmoids_NN93.pdf
    function(x) {e=exp(x); .zin(e                                    /(e+1)^2)}, #g'
    function(x) {e=exp(x); .zin(e*(-e   + 1)                         /(e+1)^3)}, #g''
    function(x) {e=exp(x); .zin(e*( e^2 - 4*e    + 1)                /(e+1)^4)}, #g^(3)
    function(x) {e=exp(x); .zin(e*(-e^3 + 11*e^2 - 11*e   + 1)       /(e+1)^5)}, #g^(4)
    function(x) {e=exp(x); .zin(e*( e^4 - 26*e^3 + 66*e^2 - 26*e + 1)/(e+1)^6)}  #g^(5)
  )
)

# Utility for integration: "[return] zero if [argument is] NaN" (Inf / Inf divs)
#
# @param x Ratio of polynoms of exponentials, as in .S[[i]]
#
.zin <- function(x)
{
  x[is.nan(x)] <- 0.
  x
}
