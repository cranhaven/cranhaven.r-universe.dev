#' Estimate Ciscato, Galichon and Gousse's model
#'
#' This function estimates the affinity matrix of the matching model of Ciscato
#' Gousse and Galichon (2020), performs the saliency analysis and the rank
#' tests. The user must supply a \emph{matched sample} that is treated as the
#' equilibrium matching of a bipartite one-to-one matching model without
#' frictions and with Transferable Utility. The model differs from the original
#' Dupuy and Galichon (2014) since all agents are pooled in one group and can
#' match within the group. For the sake of clarity, in the documentation we take
#' the example of the same-sex marriage market and refer to "first partner" and
#' "second partner" in order to distinguish between the arbitrary partner order
#' in a database (e.g., survey respondent and partner of the respondent). Note
#' that in this case the variable "sex" is treated as a matching variable rather
#' than a criterion to assign partners to one side of the market as in the
#' bipartite case. Other applications may include matching between coworkers,
#' roommates or teammates.
#'
#' @param X The matrix of traits of the first partner. Its rows must be ordered
#'   so that the i-th individual in \code{X} is matched with the i-th partner in
#'   \code{Y}: this means that \code{nrow(X)} must be equal to \code{nrow(Y)}.
#'   Its columns correspond to the different matching variables: \code{ncol(X)}
#'   must be equal to \code{ncol(Y)} and the variables must be sorted in the
#'   same way in both matrices. The matrix is demeaned and rescaled before the
#'   start of the estimation algorithm.
#' @param Y The matrix of traits of the second partner. Its rows must be ordered
#'   so that the i-th individual in \code{Y} is matched with the i-th partner in
#'   \code{X}: this means that \code{nrow(Y)} must be equal to \code{nrow(X)}.
#'   Its columns correspond to the different matching variables: \code{ncol(Y)}
#'   must be equal to \code{ncol(X)} and the variables must be sorted in the
#'   same way in both matrices. The matrix is demeaned and rescaled before the
#'   start of the estimation algorithm.
#' @param w A vector of sample weights with length \code{nrow(X)}. Defaults to
#'   uniform weights.
#' @param A0 A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'   corresponding to the initial values of the affinity matrix to be fed to the
#'   estimation algorithm. Optional. Defaults to a matrix of zeros.
#' @param lb A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'   corresponding to the lower bounds of the elements of the affinity matrix.
#'   Defaults to \code{-Inf} for all parameters.
#' @param ub A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'   corresponding to the upper bounds of the elements of the affinity matrix.
#'   Defaults to \code{Inf} for all parameters.
#' @param pr A probability indicating the significance level used to compute
#'   bootstrap two-sided confidence intervals for \code{U}, \code{V} and
#'   \code{lambda}. Defaults to 0.05.
#' @param max_iter An integer indicating the maximum number of iterations in the
#'   Maximum Likelihood Estimation. See \code{\link[stats]{optim}} for the
#'   \code{"L-BFGS-B"} method. Defaults to 10000.
#' @param tol_level A positive real number indicating the tolerance level in the
#'   Maximum Likelihood Estimation. See \code{\link[stats]{optim}} for the
#'   \code{"L-BFGS-B"} method. Defaults to 1e-6.
#' @param scale A positive real number indicating the scale of the model.
#'   Defaults to 1.
#' @param nB An integer indicating the number of bootstrap replications used to
#'   compute the confidence intervals of \code{U}, \code{V} and \code{lambda}.
#'   Defaults to 2000.
#' @param verbose If \code{TRUE}, the function displays messages to keep track
#'   of its progress. Defaults to \code{TRUE}.
#'
#' @return The function returns a list with elements: \code{X}, the demeaned and
#'   rescaled matrix of traits of the first partner; \code{Y}, the demeaned and
#'   rescaled matrix of traits of the second partner; \code{fx}, the empirical
#'   marginal distribution of first partners; \code{fy}, the empirical marginal
#'   distribution of second partners; \code{Aopt}, the estimated affinity
#'   matrix; \code{sdA}, the standard errors of \code{Aopt}; \code{tA}, the
#'   Z-test statistics of \code{Aopt}; \code{VarCovA}, the full
#'   variance-covariance matrix of \code{Aopt}; \code{rank.tests}, a list with
#'   all the summaries of the rank tests on \code{Aopt}; \code{U}, whose columns
#'   are the left-singular vectors of \code{Aopt}; \code{V}, whose columns are
#'   the right-singular vectors of \code{Aopt}; \code{lambda}, whose elements
#'   are the singular values of \code{Aopt}; \code{UCI}, whose columns are the
#'   lower and the upper bounds of the confidence intervals of \code{U};
#'   \code{VCI}, whose columns are the lower and the upper bounds of the
#'   confidence intervals of \code{V}; \code{lambdaCI}, whose columns are the
#'   lower and the upper bounds of the confidence intervals of \code{lambda};
#'   \code{df.bootstrap}, a data frame resulting from the \code{nB} bootstrap
#'   replications and used to infer the empirical distribution of the estimated
#'   objects.
#'
#' @seealso \strong{Ciscato, Edoardo, Alfred Galichon, and Marion Gousse}. "Like
#'   attract like? a structural comparison of homogamy across same-sex and
#'   different-sex households." \emph{Journal of Political Economy} 128, no. 2
#'   (2020): 740-781. \strong{Dupuy, Arnaud, and Alfred Galichon}. "Personality
#'   traits and the marriage market." \emph{Journal of Political Economy} 122,
#'   no. 6 (2014): 1271-1319.
#'
#' @examples
#'
#' # Parameters
#' K = 4 # number of matching variables
#' N = 100 # sample size
#' mu = rep(0, 2*K) # means of the data generating process
#' Sigma = matrix(c(1, -0.0992, 0.0443, -0.0246, -0.8145, 0.083, -0.0438,
#'     0.0357, -0.0992, 1, 0.0699, -0.0043, 0.083, 0.8463, 0.0699, -0.0129, 0.0443,
#'     0.0699, 1, -0.0434, -0.0438, 0.0699, 0.5127, -0.0383, -0.0246, -0.0043,
#'     -0.0434, 1, 0.0357, -0.0129, -0.0383, 0.6259, -0.8145, 0.083, -0.0438,
#'     0.0357, 1, -0.0992, 0.0443, -0.0246, 0.083, 0.8463, 0.0699, -0.0129, -0.0992,
#'     1, 0.0699, -0.0043, -0.0438, 0.0699, 0.5127, -0.0383, 0.0443, 0.0699, 1,
#'     -0.0434, 0.0357, -0.0129, -0.0383, 0.6259, -0.0246, -0.0043, -0.0434, 1),
#'                nrow=K+K) # (normalized) variance-covariance matrix of the
#'                # data generating process with a block symmetric structure
#' labels = c("Sex", "Age", "Educ.", "Black") # labels for matching variables
#'
#' # Sample
#' data = MASS::mvrnorm(N, mu, Sigma) # generating sample
#' X = data[,1:K]; Y = data[,K+1:K] # men's and women's sample data
#' w = sort(runif(N-1)); w = c(w,1) - c(0,w) # sample weights
#'
#' # Main estimation
#' res = estimate.affinity.matrix.unipartite(X, Y, w = w, nB = 500)
#'
#' # Summarize results
#' show.affinity.matrix(res, labels_x = labels, labels_y = labels)
#' show.diagonal(res, labels = labels)
#' show.test(res)
#' show.saliency(res, labels_x = labels, labels_y = labels,
#'               ncol_x = 2, ncol_y = 2)
#' show.correlations(res, labels_x = labels, labels_y = labels,
#'                   label_x_axis = "First partner",
#'                   label_y_axis = "Second partner", ndims = 2)
#'
#' @export
estimate.affinity.matrix.unipartite <- function(X,
                                                Y,
                                                w = rep(1, N),
                                                A0 = matrix(0, nrow=K, ncol=K),
                                                lb = matrix(-Inf, nrow=K, ncol=K),
                                                ub = matrix(Inf, nrow=K, ncol=K),
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
  Nx = nrow(X); Kx = ncol(X); Ny = nrow(Y); Ky = ncol(Y)
  if (Nx!=Ny) stop("Number of partners does not coincide")
  if (Kx!=Ky) stop("Number of matching variables must be the same for both partners")
  N = Nx; K = Kx
  if (length(w)!=N) stop("Weight vector inconsistent")

  # "Clone" the populations
  X1 = rbind(X, Y); X2 = rbind(Y, X)
  X1 = rescale.data(X1); X2 = rescale.data(X2)

  # Marginals
  fx1 = .5*rep(w,2)/sum(w); fx2 = .5*rep(w,2)/sum(w)

  # Empirical covariance
  pixx_hat = .5*diag(rep(w,2))/sum(w); sigma_hat = t(X1)%*%pixx_hat%*%X2
  #Cov = supercov(X1, X2)

  # Optimization problem
  if (verbose) message("Main estimation...")
  sol = stats::optim(c(A0),
                     function(A) objective(A, X1, X2, fx1, fx2, sigma_hat,
                                           scale = scale),
                     gr = function(A) gradient(A, X1, X2, fx1, fx2, sigma_hat,
                                               scale = scale),
                     hessian = TRUE, method = "L-BFGS-B",
                     lower = c(lb), upper = c(ub),
                     control = list("maxit" = max_iter, factr = tol_level))
  #print(sol$message)
  Aopt = matrix(sol$par, nrow = K, ncol = K)/scale
  InvFish = solve(sol$hessian)
  VarCov = InvFish/N/scale # export it for tests
  # NB: N is the number of real observations, not the number of rows of X1)
  sdA  = matrix(sqrt(diag(VarCov)), nrow = K, ncol = K)
  tA = Aopt/sdA

  # Saliency analysis
  if (verbose) message("Saliency analysis...")
  saliency = svd(Aopt, nu=K, nv=K)
  lambda = saliency$d[1:K]
  U = saliency$u[,1:K] # U/scaleX gives weights for unscaled data
  V = saliency$v[,1:K] # U and V are the same, up to some computational approx.

  # Test rank
  if (verbose) message("Rank tests...")
  tests = list()
  for (p in 1:(K-1)) {
    tests[[p]] = rank.test(U, V, lambda, VarCov, p)
  }

  # Inference on U, V and lambda: bootstrap
  if (verbose) message("Saliency analysis (bootstrap)...")
  omega_0 = rbind(U, V)
  df.bootstrap = data.frame(matrix(0, nrow = nB, ncol = K*K + K + K*K + K*K))
  for (i in 1:nB) {
    #print(sprintf("%d of %d", i, B))
    A_b = matrix(MASS::mvrnorm(n = 1, c(Aopt), VarCov), nrow = K, ncol = K)
    saliency_b = svd(A_b)
    d_b = saliency_b$d
    U_b = saliency_b$u # U/scaleX gives weights for unscaled data
    V_b = saliency_b$v
    omega_b = rbind(U_b, V_b)
    saliency_rotation = svd(t(omega_b)%*%omega_0)
    Q = saliency_rotation$u%*%t(saliency_rotation$v)
    df.bootstrap[i,] = c(c(A_b), c(d_b), c(U_b%*%Q), c(V_b%*%Q))
  }
  #VarCov_b = matrix(0, nrow=K*K, ncol=K*K)  # can be computed just as a check
  #for (k in 1:(K*K))
  #  for (l in 1:(K*K)) {
  #    VarCov_b[k,l] = sum((df.bootstrap[,k] - mean(df.bootstrap[,k]))*
  #                        (df.bootstrap[,l] - mean(df.bootstrap[,l])))/(B-1)
  #  }
  #}
  #sdA_b  = matrix(sqrt(diag(VarCov_b)), nrow = K, ncol = K)
  lambdaCI = matrix(0,nrow=K,ncol=2);
  for (k in 1:K) lambdaCI[k,] = stats::quantile(df.bootstrap[,K*K+k],
                                                c(pr/2, 1-pr/2))
  UCI = matrix(0,nrow=K*K,ncol=2);
  for (k in 1:(K*K)) UCI[k,] = stats::quantile(df.bootstrap[,K*K+K+k],
                                               c(pr/2, 1-pr/2))
  VCI = matrix(0,nrow=K*K,ncol=2);
  for (k in 1:(K*K)) VCI[k,] = stats::quantile(df.bootstrap[,K*K+K+K*K+k],
                                               c(pr/2, 1-pr/2))

  est_results = list("X" = X1[1:N,], "Y" = X2[1:N,], "fx" = fx1[1:N],
                     "fy" = fx2[1:N], "Aopt" = Aopt, "sdA" = sdA, "tA" = tA,
                     "VarCovA" = VarCov, "rank.tests" = tests, "U" = U, "V" = V,
                     "lambda" = lambda, "df.bootstrap" = df.bootstrap,
                     "lambdaCI" = lambdaCI, "UCI" = UCI, "VCI" = VCI)

  return(est_results)

}
