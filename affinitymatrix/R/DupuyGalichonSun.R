#' Estimate Dupuy and Galichon's model
#'
#' This function estimates the affinity matrix of the matching model of Dupuy
#' and Galichon (2014) under a rank restriction on the affinity matrix, as
#' suggested by Dupuy, Galichon and Sun (2019). In their own words, "to
#' accommodate high dimensionality of the data, they propose a novel method that
#' incorporates a nuclear norm regularization which effectively enforces a rank
#' constraint on the affinity matrix." This function also performs the saliency
#' analysis and the rank tests. The user must supply a \emph{matched sample}
#' that is treated as the equilibrium matching of a bipartite one-to-one
#' matching model without frictions and with Transferable Utility. For the sake
#' of clarity, in the documentation we take the example of the marriage market
#' and refer to "men" as the observations on one side of the market and to
#' "women" as the observations on the other side. Other applications may include
#' matching between CEOs and firms, firms and workers, buyers and sellers, etc.
#'
#' @param X The matrix of men's traits. Its rows must be ordered so that the
#'   i-th man is matched with the i-th woman: this means that \code{nrow(X)}
#'   must be equal to \code{nrow(Y)}. Its columns correspond to the different
#'   matching variables: \code{ncol(X)} can be different from \code{ncol(Y)}.
#'   For the sake of clarity of exposition when using descriptive tools such as
#'   \code{\link{show.correlations}}, it is recommended assigning the same
#'   matching variable to the k-th column of \code{X} and to the k-th column of
#'   \code{Y}, whenever possible. If \code{X} has more matching variables than
#'   \code{Y}, then those variables that appear in \code{X} but no in {Y} should
#'   be found in the last columns of \code{X} (and vice versa). The matrix is
#'   demeaned and rescaled before the start of the estimation algorithm.
#' @param Y The matrix of women's traits. Its rows must be ordered so that the
#'   i-th woman is matched with the i-th man: this means that \code{nrow(Y)}
#'   must be equal to \code{nrow(X)}.  Its columns correspond to the different
#'   matching variables: \code{ncol(Y)} can be different from \code{ncol(X)}.
#'   The matrix is demeaned and rescaled before the start of the estimation
#'   algorithm.
#' @param w A vector of sample weights with length \code{nrow(X)}. Defaults to
#'   uniform weights.
#' @param A0 A vector or matrix with \code{ncol(X)*ncol(Y)} elements
#'   corresponding to the initial values of the affinity matrix to be fed to the
#'   estimation algorithm. Optional. Defaults to matrix of zeros.
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
#'   proximal gradient descent algorithm. Defaults to 10000.
#' @param tol_level A positive real number indicating the tolerance level in the
#'   proximal gradient descent algorithm. Defaults to 1e-8.
#' @param tau A positive real number indicating a sensitivity parameter in the
#'   proximal gradient descent algorithm. Defaults to 1 and should not be
#'   changed unless computational problems arise.
#' @param scale A positive real number indicating the scale of the model.
#'   Defaults to 1.
#' @param cross_validation If \code{TRUE}, the function looks for a rank
#'   restriction through cross validation. The cross validation exercise aims to
#'   minimize the covariance mismatch: in other words, it avoids overfitting
#'   without excessively reducing the number of free parameters. Defaults to
#'   \code{TRUE}.
#' @param manual_lambda A positive real number indicating the user-supply
#'   \code{lambda} when \code{cross_validation==FALSE}. The higher
#'   \code{lambda}, the tighter the rank restriction. Defaults to 0.
#' @param lambda_min A positive real number indicating minimum value for
#'   \code{lambda} considered during the cross validation. We recommend using 0,
#'   but with a high number of matching variables relatively to the sample size
#'   it is reasonable to set \code{lambda_min} to a higher value. Defaults to 0.
#' @param Nfolds An integer indicating the number of folds in the cross
#'   validation. Defaults to 5 and can be increased with a large sample size.
#' @param nB An integer indicating the number of bootstrap replications used to
#'   compute the confidence intervals of \code{Aopt}, \code{U}, \code{V} and
#'   \code{lambda}. Defaults to 2000.
#' @param verbose If \code{TRUE}, the function displays messages to keep track
#'   of its progress. Defaults to \code{TRUE}.
#'
#' @return The function returns a list with elements: \code{X}, the demeaned and
#'   rescaled matrix of men's traits; \code{Y}, the demeaned and rescaled matrix
#'   of men's traits; \code{fx}, the empirical marginal distribution of men;
#'   \code{fy}, the empirical marginal distribution of women; \code{Aopt}, the
#'   estimated affinity matrix; \code{sdA}, the standard errors of \code{Aopt};
#'   \code{tA}, the Z-test statistics of \code{Aopt}; \code{VarCovA}, the full
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
#'   objects; \code{lambda.rank.restriction}, a positive real number indicating
#'   the value of the Lagrange multiplier of the nuclear norm constraint of the
#'   affinity matrix, either chosen by the user or through Cross Validation;
#'   \code{df.cross.validation}, a data frame containing the detailed results of
#'   the cross validation exercise.
#'
#' @seealso \strong{Dupuy, Arnaud, Alfred Galichon, and Yifei Sun}. "Estimating
#'   matching affinity matrices under low-rank constraints." \emph{Information
#'   and Inference: A Journal of the IMA} 8, no. 4 (2019): 677-689.
#'   \strong{Dupuy, Arnaud, and Alfred Galichon}. "Personality traits and the
#'   marriage market." \emph{Journal of Political Economy} 122, no. 6 (2014):
#'   1271-1319.
#'
#' @examples
#'
#' # Parameters
#' Kx = 2; Ky = 2; # number of matching variables on both sides of the market
#' N = 100 # sample size
#' mu = rep(0, Kx+Ky) # means of the data generating process
#' Sigma = matrix(c(1, -0.0244, 0.1489, -0.1301, -0.0244, 1, -0.0553, 0.2717,
#'                  0.1489, -0.0553, 1, -0.1959, -0.1301, 0.2717, -0.1959, 1),
#'                  nrow=Kx+Ky)
#'     # (normalized) variance-covariance matrix of the data generating process
#' labels_x = c("Height", "BMI") # labels for men's matching variables
#' labels_y = c("Height", "BMI") # labels for women's matching variables
#'
#' # Sample
#' data = MASS::mvrnorm(N, mu, Sigma) # generating sample
#' X = data[,1:Kx]; Y = data[,Kx+1:Ky] # men's and women's sample data
#' w = sort(runif(N-1)); w = c(w,1) - c(0,w) # sample weights
#'
#' # Main estimation
#' res = estimate.affinity.matrix.lowrank(X, Y, w = w, tol_level = 1e-03,
#'                                        nB = 50, Nfolds = 2)
#'
#' # Summarize results
#' show.affinity.matrix(res, labels_x = labels_x, labels_y = labels_y)
#' show.diagonal(res, labels = labels_x)
#' show.test(res)
#' show.saliency(res, labels_x = labels_x, labels_y = labels_y,
#'               ncol_x = 2, ncol_y = 2)
#' show.cross.validation(res)
#' show.correlations(res, labels_x = labels_x, labels_y = labels_y,
#'                   label_x_axis = "Husband", label_y_axis = "Wife", ndims = 2)
#'
#' @export
estimate.affinity.matrix.lowrank <- function(X,
                                             Y,
                                             w = rep(1, N),
                                             A0 = matrix(0, nrow=Kx, ncol=Ky),
                                             lb = matrix(-Inf, nrow=Kx, ncol=Ky),
                                             ub = matrix(Inf, nrow=Kx, ncol=Ky),
                                             pr = .05,
                                             max_iter = 10000,
                                             tol_level = 1e-8,
                                             tau = 1,
                                             scale = 1,
                                             cross_validation = TRUE,
                                             manual_lambda = 0.,
                                             lambda_min = 0,
                                             Nfolds = 5,
                                             nB = 2000,
                                             verbose = TRUE)
{

  # Number of types and rescaling
  if (verbose) message("Setup...")
  X = as.matrix(X); Y = as.matrix(Y)
  X = rescale.data(X); Y = rescale.data(Y)
  Nx = nrow(X); Kx = ncol(X); Ny = nrow(Y); Ky = ncol(Y)
  if (Nx!=Ny) stop("Number of wives does not match number of husbands")
  N = Nx; K = min(Kx,Ky)
  if (length(w)!=N) stop("Weight vector inconsistent")

  # Marginals
  fx = w/sum(w); fy = w/sum(w)

  # Empirical covariance
  pixy_hat = diag(w)/sum(w); sigma_hat = t(X)%*%pixy_hat%*%Y
  #Cov = supercov(X, Y)

  # Determine lambda_max (lambda s.t. rank of Aopt is 1)
  if (verbose) message("Cross Validation...")
  if (cross_validation) {
    Amat = matrix(A0, nrow = Kx, ncol = Ky);
    lambda_max = 0.1; R = 10; iterR = 0
    while (R>1 && iterR<= 50) {
      if(iterR>0) lambda_max = lambda_max + .1
      res = proximal_gradient_descent(Amat, lambda_max, X, Y, fx, fy, sigma_hat,
                                      lb = lb, ub = ub,
                                      max_iter = max_iter,
                                      tol_level = 1e-03,
                                      tau = tau)
      Amat = res$Aopt # using stored results as next initial values
      R = qr(Amat)$rank
      iterR = iterR + 1
      #print(c(R,lambda_max))
    }
    # Sample partition
    df = data.frame()
    index = rep(1:Nfolds, ceiling(N/Nfolds)); index = index[sample(1:length(index))][1:N]
    # Covariance mismatch fold by fold
    for (f in 1:Nfolds) {
      X_f = X[index==f,]; Y_f = Y[index==f,]
      pixy_f = pixy_hat[index==f,index==f]; pixy_f = pixy_f / sum(pixy_f)
      sigma_f = t(X_f)%*%pixy_f%*%Y_f
      X_nf = X[index!=f,]; Y_nf = Y[index!=f,]
      fx_nf = fx[index!=f]; fx_nf = fx_nf/sum(fx_nf)
      fy_nf = fy[index!=f]; fy_nf = fy_nf/sum(fy_nf)
      pixy_nf = pixy_hat[index!=f,index!=f]; pixy_nf = pixy_nf / sum(pixy_nf)
      sigma_nf = t(X_nf)%*%pixy_nf%*%Y_nf
      iterR = 0; lambda_run = 1000
      Amat = matrix(A0, nrow = Kx, ncol = Ky)
      while (lambda_run>lambda_min) {
        if(iterR>0) lambda_run = max(lambda_run-0.01, 0) else lambda_run =
            round(lambda_max, digits=2)
        res = proximal_gradient_descent(Amat, lambda_run,
                                        X_nf, Y_nf, fx_nf, fy_nf, sigma_nf,
                                        lb = lb, ub = ub,
                                        max_iter = max_iter,
                                        tol_level = 1e-03,
                                        tau = tau)
        Amat = res$Aopt # using stored results as next initial values
        R = qr(Amat)$rank
        iterR = iterR + 1
        cov_err = sqrt(sum((sigma_f - res$sigma)^2))
        #print(c(f,lambda_run,R,cov_err))
        df = rbind(df, cbind(f,
                             iterR,
                             lambda_run,
                             R,
                             cov_err))
      }
    }

    # Covariance mismatch dataset
    colnames(df) = c("fold", "iter", "lambda", "rank", "cov_err")
    df_mean = stats::aggregate(df, list(df$lambda), mean)
    df_sd = stats::aggregate(df, list(df$lambda), function(x) sqrt(stats::var(x)))
    lambda_opt = df_mean$lambda[which.min(df_mean$cov_err)]
  } else {
    lambda_opt = manual_lambda
  }

  #print(paste0("Lambda_opt: ",lambda_opt))

  # Point estimates
  if (verbose) message("Main estimation...")
  res = proximal_gradient_descent(Amat, lambda_opt, X, Y, fx, fy, sigma_hat,
                                  lb = lb, ub = ub,
                                  max_iter = max_iter,
                                  tol_level = tol_level,
                                  tau = tau)
  Aopt = res$Aopt/scale

  # Saliency analysis
  if (verbose) message("Saliency analysis...")
  saliency = svd(Aopt, nu=Kx, nv=Ky)
  lambda = saliency$d[1:K]
  U = saliency$u[,1:K] # U/scaleX gives weights for unscaled data
  V = saliency$v[,1:K]

  # Inference
  if (verbose) message("Inference (bootstrap)...")
  omega_0 = rbind(U, V)
  df.bootstrap = data.frame(matrix(0, nrow = nB, ncol = Kx*Ky + K + Kx*K + Ky*K))
  for (i in 1:nB) {
    #print(sprintf("%d of %d", i, nB))
    w_b = sort(stats::runif(N-1)); w_b = c(w_b,1) - c(0,w_b)
    sigma_b = t(X)%*%diag(w_b)%*%Y
    sol_b = proximal_gradient_descent(Aopt, lambda_opt, X, Y, w_b, w_b, sigma_b,
                                      lb = lb, ub = ub,
                                      max_iter = max_iter,
                                      tol_level = 1e-03,
                                      tau = tau)
    A_b = sol_b$Aopt/scale
    saliency_b = svd(A_b, nu=K, nv=K)
    d_b = saliency_b$d
    U_b = saliency_b$u # U/scaleX gives weights for unscaled data
    V_b = saliency_b$v
    omega_b = rbind(U_b, V_b)
    saliency_rotation = svd(t(omega_b)%*%omega_0)
    Q = saliency_rotation$u%*%t(saliency_rotation$v)
    df.bootstrap[i,] = c(A_b, d_b, U_b%*%Q, V_b%*%Q)
  }
  VarCov = matrix(0, nrow=Kx*Ky, ncol=Kx*Ky)
  for (k in 1:(Kx*Ky)) {
    for (l in 1:(Kx*Ky)) {
      VarCov[k,l] = sum((df.bootstrap[,k] - mean(df.bootstrap[,k]))*
                          (df.bootstrap[,l] - mean(df.bootstrap[,l])))/(nB-1)
    }
  }
  sdA  = matrix(sqrt(diag(VarCov)), nrow = Kx, ncol = Ky)
  tA = Aopt/sdA # long, but easier to compute than inverting the Hessian
  lambdaCI = matrix(0,nrow=K,ncol=2);
  for (k in 1:K) lambdaCI[k,] = stats::quantile(df.bootstrap[,Kx*Ky+k],
                                                c(pr/2, 1-pr/2))
  UCI = matrix(0,nrow=Kx*K,ncol=2);
  for (k in 1:(Kx*K)) UCI[k,] = stats::quantile(df.bootstrap[,Kx*Ky+K+k],
                                                c(pr/2, 1-pr/2))
  VCI = matrix(0,nrow=Ky*K,ncol=2);
  for (k in 1:(Ky*K)) VCI[k,] = stats::quantile(df.bootstrap[,Kx*Ky+K+Kx*K+k],
                                                c(pr/2, 1-pr/2))

  # Test rank
  if (verbose) message("Rank tests...")
  tests = list()
  for (p in 1:(K-1)) {
    tests[[p]] = rank.test(saliency$u, saliency$v, saliency$d, VarCov, p)
  }

  est_results = list("X" = X, "Y" = Y, "fx" = fx, "fy" = fy, "Aopt" = Aopt,
                     "sdA" = sdA, "tA" = tA, "VarCovA" = VarCov,
                     "rank.tests" = tests,
                     "lambda.rank.restriction" = lambda_opt,
                     "df.cross.validation" = df, "U" = U, "V" = V,
                     "lambda" = lambda, "df.bootstrap" = df.bootstrap,
                     "lambdaCI" = lambdaCI, "UCI" = UCI, "VCI" = VCI)

  return(est_results)

}

# Algorithm to compute the estimator under rank restriction
# NB: the larger lambda, the tighter the restriction
proximal_gradient_descent <- function(A0, lambda, X, Y, fx, fy, sigma_hat,
                                      lb = matrix(-Inf, nrow=Kx, ncol=Ky),
                                      ub = matrix(Inf, nrow=Kx, ncol=Ky),
                                      max_iter = 10000,
                                      tol_level = 1e-5,
                                      tau = 1e+00,
                                      scale = 1) {

  # Optimization problem
  tol = 1e+10; iter = 0
  N = nrow(X); Kx = ncol(X); Ky = ncol(Y)
  Amat = matrix(A0, nrow = Kx, ncol = Ky)
  lb = matrix(lb, nrow=Kx, ncol=Ky)
  ub = matrix(ub, nrow=Kx, ncol=Ky)
  while(tol > tol_level && iter < max_iter) {

    # Auxiliary variable
    A0 = Amat
    A0 = pmin(A0, ub); A0 = pmax(A0, lb)
    Kxy = exp(X%*%Amat%*%t(Y)/scale)
    normConst = sum(Kxy)
    Kxy = Kxy / normConst

    # Optimal pi
    pixy = piopt(Kxy, fx, fy)

    # Step
    Amat = Amat - tau*(t(X)%*%pixy%*%Y - sigma_hat)
    saliency = svd(Amat)
    Amat = saliency$u%*%diag(pmax(saliency$d - tau*lambda,0))%*%t(saliency$v)
    tol = max((abs(Amat-A0)/A0))
    iter = iter+1

  }

  if(iter>=max_iter) warning("WARNING: proximal gradient descent reached max_iter")

  return(list("Aopt" = Amat, "pixy" = pixy, "sigma" = t(X)%*%pixy%*%Y))

}
