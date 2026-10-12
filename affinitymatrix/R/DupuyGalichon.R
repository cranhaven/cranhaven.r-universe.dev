#' Estimate Dupuy and Galichon's model
#'
#' This function estimates the affinity matrix of the matching model of Dupuy
#' and Galichon (2014), performs the saliency analysis and the rank tests. The
#' user must supply a \emph{matched sample} that is treated as the equilibrium
#' matching of a bipartite one-to-one matching model without frictions and with
#' Transferable Utility. For the sake of clarity, in the documentation we take
#' the example of the marriage market and refer to "men" as the observations on
#' one side of the market and to "women" as the observations on the other side.
#' Other applications may include matching between CEOs and firms, firms and
#' workers, buyers and sellers, etc.
#'
#' @param X The matrix of men's traits. Its rows must be ordered so that the
#'     i-th man is matched with the i-th woman: this means that \code{nrow(X)}
#'     must be equal to \code{nrow(Y)}. Its columns correspond to the different
#'     matching variables: \code{ncol(X)} can be different from \code{ncol(Y)}.
#'     For the sake of clarity of exposition when using descriptive tools such
#'     as \code{\link{show.correlations}}, it is recommended assigning the same
#'     matching variable to the k-th column of \code{X} and to the k-th column
#'     of \code{Y}, whenever possible. If \code{X} has more matching variables
#'     than \code{Y}, then those variables that appear in \code{X} but no in {Y}
#'     should be found in the last columns of \code{X} (and vice versa). The
#'     matrix is demeaned and rescaled before the start of the estimation
#'     algorithm.
#' @param Y The matrix of women's traits. Its rows must be ordered so that the
#'     i-th woman is matched with the i-th man: this means that \code{nrow(Y)}
#'     must be equal to \code{nrow(X)}.  Its columns correspond to the different
#'     matching variables: \code{ncol(Y)} can be different from \code{ncol(X)}.
#'     The matrix is demeaned and rescaled before the start of the estimation
#'     algorithm.
#' @param w A vector of sample weights with length \code{nrow(X)}. Defaults to
#'     uniform weights.
#' @param A0 A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'     corresponding to the initial values of the affinity matrix to be fed to
#'     the estimation algorithm. Optional. Defaults to matrix of zeros.
#' @param lb A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'     corresponding to the lower bounds of the elements of the affinity matrix.
#'     Defaults to \code{-Inf} for all parameters.
#' @param ub A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'     corresponding to the upper bounds of the elements of the affinity matrix.
#'     Defaults to \code{Inf} for all parameters.
#' @param pr A probability indicating the significance level used to compute
#'     bootstrap two-sided confidence intervals for \code{U}, \code{V} and
#'     \code{lambda}. Defaults to 0.05.
#' @param max_iter An integer indicating the maximum number of iterations in the
#'     Maximum Likelihood Estimation. See \code{\link[stats]{optim}} for the
#'     \code{"L-BFGS-B"} method. Defaults to 10000.
#' @param tol_level A positive real number indicating the tolerance level in the
#'     Maximum Likelihood Estimation. See \code{\link[stats]{optim}} for the
#'     \code{"L-BFGS-B"} method. Defaults to 1e-6.
#' @param scale A positive real number indicating the scale of the model.
#'     Defaults to 1.
#' @param nB An integer indicating the number of bootstrap replications used to
#'     compute the confidence intervals of \code{U}, \code{V} and \code{lambda}.
#'     Defaults to 2000.
#' @param verbose If \code{TRUE}, the function displays messages to keep track
#'     of its progress. Defaults to \code{TRUE}.
#'
#' @return The function returns a list with elements: \code{X}, the demeaned and
#'     rescaled matrix of men's traits; \code{Y}, the demeaned and rescaled
#'     matrix of men's traits; \code{fx}, the empirical marginal distribution of
#'     men; \code{fy}, the empirical marginal distribution of women;
#'     \code{Aopt}, the estimated affinity matrix; \code{sdA}, the standard
#'     errors of \code{Aopt}; \code{tA}, the Z-test statistics of \code{Aopt};
#'     \code{VarCovA}, the full variance-covariance matrix of \code{Aopt};
#'     \code{rank.tests}, a list with all the summaries of the rank tests on
#'     \code{Aopt}; \code{U}, whose columns are the left-singular vectors of
#'     \code{Aopt}; \code{V}, whose columns are the right-singular vectors of
#'     \code{Aopt}; \code{lambda}, whose elements are the singular values of
#'     \code{Aopt}; \code{UCI}, whose columns are the lower and the upper bounds
#'     of the confidence intervals of \code{U}; \code{VCI}, whose columns are
#'     the lower and the upper bounds of the confidence intervals of \code{V};
#'     \code{lambdaCI}, whose columns are the lower and the upper bounds of the
#'     confidence intervals of \code{lambda}; \code{df.bootstrap}, a data frame
#'     resulting from the \code{nB} bootstrap replications and used to infer the
#'     empirical distribution of the estimated objects.
#'
#' @seealso \strong{Dupuy, Arnaud, and Alfred Galichon}. "Personality traits and
#'     the marriage market." \emph{Journal of Political Economy} 122, no. 6
#'     (2014): 1271-1319.
#'
#' @examples
#'
#' # Parameters
#' Kx = 4; Ky = 4; # number of matching variables on both sides of the market
#' N = 200 # sample size
#' mu = rep(0, Kx+Ky) # means of the data generating process
#' Sigma = matrix(c(1, 0.326, 0.1446, -0.0668, 0.5712, 0.4277, 0.1847, -0.2883,
#'                  0.326, 1, -0.0372, 0.0215, 0.2795, 0.8471, 0.1211, -0.0902,
#'                  0.1446, -0.0372, 1, -0.0244, 0.2186, 0.0636, 0.1489,
#'                  -0.1301, -0.0668, 0.0215, -0.0244, 1, 0.0192, 0.0452,
#'                  -0.0553, 0.2717, 0.5712, 0.2795, 0.2186, 0.0192, 1, 0.3309,
#'                  0.1324, -0.1896, 0.4277, 0.8471, 0.0636, 0.0452, 0.3309, 1,
#'                  0.0915, -0.1299, 0.1847, 0.1211, 0.1489, -0.0553, 0.1324,
#'                  0.0915, 1, -0.1959, -0.2883, -0.0902, -0.1301, 0.2717,
#'                  -0.1896, -0.1299, -0.1959, 1),
#'                nrow=Kx+Ky) # (normalized) variance-covariance matrix of the
#'                # data generating process
#' labels_x = c("Educ.", "Age", "Height", "BMI") # labels for men's matching variables
#' labels_y = c("Educ.", "Age", "Height", "BMI") # labels for women's matching variables
#'
#' # Sample
#' data = MASS::mvrnorm(N, mu, Sigma) # generating sample
#' X = data[,1:Kx]; Y = data[,Kx+1:Ky] # men's and women's sample data
#' w = sort(runif(N-1)); w = c(w,1) - c(0,w) # sample weights
#'
#' # Main estimation
#' res = estimate.affinity.matrix(X, Y, w = w, nB = 500)
#'
#' # Summarize results
#' show.affinity.matrix(res, labels_x = labels_x, labels_y = labels_y)
#' show.diagonal(res, labels = labels_x)
#' show.test(res)
#' show.saliency(res, labels_x = labels_x, labels_y = labels_y,
#'               ncol_x = 2, ncol_y = 2)
#' show.correlations(res, labels_x = labels_x, labels_y = labels_y,
#'                   label_x_axis = "Husband", label_y_axis = "Wife", ndims = 2)
#'
#' @export
estimate.affinity.matrix <- function(X,
                                     Y,
                                     w = rep(1, N),
                                     A0 = matrix(0, nrow=Kx, ncol=Ky),
                                     lb = matrix(-Inf, nrow=Kx, ncol=Ky),
                                     ub = matrix(Inf, nrow=Kx, ncol=Ky),
                                     pr = .05,
                                     max_iter = 10000,
                                     tol_level = 1e-6,
                                     scale = 1,
                                     nB = 2000,
                                     verbose = TRUE)
{

  # Number of types and rescaling
  if (verbose) message("Setup...")
  X = as.matrix(X); Y = as.matrix(Y)
  X = rescale.data(X); Y = rescale.data(Y)
  Nx = nrow(X); Kx = ncol(X); Ny = nrow(Y); Ky = ncol(Y)
  if (Nx!=Ny) stop("Number of wives does not match number of husbands")
  N = Nx; K = min(Kx,Ky);
  if (length(w)!=N) stop("Weight vector inconsistent")

  # Marginals
  fx = w/sum(w); fy = w/sum(w)

  # Empirical covariance
  pixy_hat = diag(w)/sum(w); sigma_hat = t(X)%*%pixy_hat%*%Y
  #Cov = supercov(X, Y)

  # Optimization problem
  if (verbose) message("Main estimation...")
  sol = stats::optim(c(A0),
                     function(A) objective(A, X, Y, fx, fy, sigma_hat,
                                           scale = scale),
                     gr = function(A) gradient(A, X, Y, fx, fy, sigma_hat,
                                               scale = scale),
                     hessian = TRUE, method = "L-BFGS-B",
                     lower = c(lb), upper = c(ub),
                     control = list("maxit" = max_iter, factr = tol_level))
  #print(sol$message)
  Aopt = matrix(sol$par, nrow = Kx, ncol = Ky)/scale
  InvFish = solve(sol$hessian)
  VarCov = InvFish/N/scale # export it for tests
  sdA  = matrix(sqrt(diag(VarCov)), nrow = Kx, ncol = Ky)
  tA = Aopt/sdA

  # Saliency analysis
  if (verbose) message("Saliency analysis...")
  saliency = svd(Aopt, nu=Kx, nv=Ky)
  lambda = saliency$d[1:K]
  U = saliency$u[,1:K] # U/scaleX gives weights for unscaled data
  V = saliency$v[,1:K]

  # Test rank
  if (verbose) message("Rank tests...")
  tests = list()
  for (p in 1:(K-1)) {
    tests[[p]] = rank.test(saliency$u, saliency$v, saliency$d, VarCov, p)
  }

  # Inference on U, V and lambda: bootstrap
  if (verbose) message("Saliency analysis (bootstrap)...")
  omega_0 = rbind(U, V)
  df.bootstrap = data.frame(matrix(0, nrow = nB,
                                   ncol = Kx*Ky + K + Kx*K + Ky*K))
  for (i in 1:nB) {
    #print(sprintf("%d of %d", i, B))
    A_b = matrix(MASS::mvrnorm(n = 1, c(Aopt), VarCov), nrow = Kx, ncol = Ky)
    saliency_b = svd(A_b, nu=K, nv=K)
    d_b = saliency_b$d
    U_b = saliency_b$u # U/scaleX gives weights for unscaled data
    V_b = saliency_b$v
    omega_b = rbind(U_b, V_b)
    saliency_rotation = svd(t(omega_b)%*%omega_0)
    Q = saliency_rotation$u%*%t(saliency_rotation$v)
    df.bootstrap[i,] = c(c(A_b), c(d_b), c(U_b%*%Q), c(V_b%*%Q))
  }
  #VarCov_b = matrix(0, nrow=K*K, ncol=K*K) # can be computed just as a check
  #for (k in 1:(K*K))
  #  for (l in 1:(K*K)) {
  #    VarCov_b[k,l] = sum((df.bootstrap[,k] - mean(df.bootstrap[,k]))*
  #                        (df.bootstrap[,l] - mean(df.bootstrap[,l])))/(B-1)
  #  }
  #}
  #sdA_b  = matrix(sqrt(diag(VarCov_b)), nrow = K, ncol = K)
  lambdaCI = matrix(0,nrow=K,ncol=2);
  for (k in 1:K) lambdaCI[k,] = stats::quantile(df.bootstrap[,Kx*Ky+k],
                                                c(pr/2, 1-pr/2))
  UCI = matrix(0,nrow=Kx*K,ncol=2);
  for (k in 1:(Kx*K)) UCI[k,] = stats::quantile(df.bootstrap[,Kx*Ky+K+k],
                                                c(pr/2, 1-pr/2))
  VCI = matrix(0,nrow=Ky*K,ncol=2);
  for (k in 1:(Ky*K)) VCI[k,] = stats::quantile(df.bootstrap[,Kx*Ky+K+Kx*K+k],
                                                c(pr/2, 1-pr/2))

  est_results = list("X" = X, "Y" = Y, "fx" = fx, "fy" = fy, "Aopt" = Aopt,
                     "sdA" = sdA, "tA" = tA, "VarCovA" = VarCov,
                     "rank.tests" = tests, "U" = U, "V" = V, "lambda" = lambda,
                     "df.bootstrap" = df.bootstrap, "lambdaCI" = lambdaCI,
                     "UCI" = UCI, "VCI" = VCI)

  return(est_results)

}

# IPFP algorithm
piopt <- function(Kxy, fx, fy) {

  # Run IPFP loop
  N = nrow(Kxy)
  ax = exp(rep(-1,N)); by = exp(rep(1,N))
  iter = 1; tol = 1.
  while (iter < 2000 & tol > 1e-03) {
    Ax=ax
    by = fy/(t(Kxy)%*%Ax)
    By=by
    ax = fx/(Kxy%*%By)
    tol = max(abs(by*(t(Kxy)%*%ax) - fy)/fy)
    iter = iter + 1
  }

  return(Kxy * (ax%*%t(by)))

}

# Function to rescale data
rescale.data <- function(X, Xref = X) {

  X = as.matrix(X)
  N = nrow(X); K = ncol(X)
  mX = colMeans(X, dims=1, na.rm=TRUE)
  scaleX = sqrt(diag(stats::cov(X, use="pairwise.complete.obs")))
  for (k in 1:K) X[,k] = (X[,k] - mX[k])/scaleX[k]

  return(X)

}

# Likelihood function
objective <- function(A, X, Y, fx, fy, sigma_hat, scale = 1) {

  # Number of types
  N = nrow(X); Kx = ncol(X); Ky = ncol(Y)
  Amat = matrix(A, nrow = Kx, ncol = Ky)

  # Auxiliary variable
  Kxy = exp(X%*%Amat%*%t(Y)/scale)
  normConst = sum(Kxy)
  Kxy = Kxy / normConst

  # Optimal pi
  pixy = piopt(Kxy, fx, fy)

  # Total surplus
  systsurplus = sum((X%*%Amat%*%t(Y))*pixy)
  entropy = sum(pixy*log(pmax(pixy,1e-250)))
  twistedtrace = sum(sigma_hat*Amat)
  f = systsurplus - scale*entropy - twistedtrace
  #print(f)

  return(f)

}

# Gradient of likelihood function
gradient <- function(A, X, Y, fx, fy, sigma_hat, scale = 1) {

  # Number of types
  N = nrow(X); Kx = ncol(X); Ky = ncol(Y)
  Amat = matrix(A, nrow = Kx, ncol = Ky)

  # Auxiliary variable
  Kxy = exp(X%*%Amat%*%t(Y)/scale)
  normConst = sum(Kxy)
  Kxy = Kxy / normConst

  # Optimal pi
  pixy = piopt(Kxy, fx, fy)

  # Gradient
  G = c(t(X)%*%pixy%*%Y - sigma_hat)

  return(G)

}

# Test for rank(A)=p
rank.test <- function(U, V, lambda, VarCov, p) {

  Vp = t(V); Kx = dim(U)[1]; Ky = dim(V)[1]; K = length(lambda);
  S = diag(lambda);
  if (Kx>K) S = rbind(S, matrix(0, ncol=K, nrow=Kx-K))
  if (Ky>K) S = cbind(S, matrix(0, nrow=K, ncol=Ky-K))

  U11 = U[1:p,1:p]
  U21 = U[1:p,(p+1):Kx]
  U12 = U[(p+1):Kx,1:p]
  U22 = U[(p+1):Kx,(p+1):Kx]

  Vp11 = Vp[1:p,1:p]
  Vp12 = Vp[1:p,(p+1):Ky]
  Vp21 = Vp[(p+1):Ky,1:p]
  Vp22 = Vp[(p+1):Ky,(p+1):Ky]

  S1 = S[1:p,1:p]
  S2 = S[(p+1):Kx,(p+1):Ky]

  Tp = solve(expm::sqrtm(U22%*%t(U22)))%*%U22%*%S2%*%Vp22%*%expm::sqrtm(solve(t(Vp22)%*%Vp22))
  Ap = U[1:Kx,(p+1):Kx]%*%solve(U22)%*%expm::sqrtm(U22%*%t(U22))
  Bp = expm::sqrtm(t(Vp22)%*%Vp22)%*%solve(Vp22)%*%Vp[(p+1):Ky,1:Ky]
  Op = kronecker(Bp,t(Ap))%*%VarCov%*%t(kronecker(Bp,t(Ap)))
  st = t(c(Tp))%*%solve(Op)%*%c(Tp)
  df = (Kx-p)*(Ky-p)

  return(list("p" = p, "chi2" = st, "df" = df))

}

# Variance-covariance of X and Y stacked
supercov <- function(X, Y) {

  N = dim(X)[1]; Kx = dim(X)[2]; Ky = dim(Y)[2];

  phi_xy = matrix(0, nrow=N, ncol=Kx*Ky)
  for (i in 1:Kx) {
    for (j in 1:Ky) {
      k = (i-1)*Ky + j
      phi_xy[,k] = X[,i]*Y[,j]
    }
  }

  phi_xx = matrix(0, nrow=N, ncol=Kx*Ky)
  for (i in 1:Kx) {
    for (j in 1:Ky) {
      k = (i-1)*Kx + j
      if (i==j) phi_xx[,k] = X[,i]*X[,j]
    }
  }

  phi_yy = matrix(0, nrow=N, ncol=Kx*Ky)
  for (i in 1:Kx) {
    for (j in 1:Ky) {
      k = (i-1)*Ky + j
      if (i==j) phi_yy[,k] = Y[,i]*Y[,j]
    }
  }

  Cov_XY_XY = matrix(0, nrow=Kx*Ky, ncol=Kx*Ky)
  for (k in 1:(Kx*Ky)) {
    for (l in 1:(Kx*Ky)) {
      Cov_XY_XY[k,l] = t(phi_xy[,k])%*%phi_xy[,l]/N
    }
  }

  Cov_XX_XX = matrix(0, nrow=Kx*Kx, ncol=Kx*Kx)
  for (k in 1:(Kx*Kx)) {
    for (l in 1:(Kx*Kx)) {
      Cov_XX_XX[k,l] = t(phi_xx[,k])%*%phi_xx[,l]/N
    }
  }

  Cov_YY_YY = matrix(0, nrow=Ky*Ky, ncol=Ky*Ky)
  for (k in 1:(Ky*Ky)) {
    for (l in 1:(Ky*Ky)) {
      Cov_YY_YY[k,l] = t(phi_yy[,k])%*%phi_yy[,l]/N
    }
  }

  Cov_XX_YY = matrix(0, nrow=Kx*Kx, ncol=Ky*Ky)
  for (k in 1:(Kx*Kx)) {
    for (l in 1:(Ky*Ky)) {
      Cov_XX_YY[k,l] = t(phi_xx[,k])%*%phi_yy[,l]/N
    }
  }

  return(list("Cov_XY_XY" = Cov_XY_XY, "Cov_XX_XX" = Cov_XX_XX,
              "Cov_YY_YY" = Cov_YY_YY, "Cov_XX_YY" = Cov_XX_YY))

}

