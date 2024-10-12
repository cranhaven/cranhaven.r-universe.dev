#' Compute mu
#'
#' Estimate the normalized columns mu of the beta matrix parameter in a mixture of
#' logistic regressions models, with a spectral method described in the package vignette.
#'
#' @name computeMu
#'
#' @param X Matrix of input data (size nxd)
#' @param Y Vector of binary outputs (size n)
#' @param optargs List of optional argument:
#'   \itemize{
#'     \item 'jd_method', joint diagonalization method from the package jointDiag:
#'       'uwedge' (default) or 'jedi'.
#'     \item 'jd_nvects', number of random vectors for joint-diagonalization
#'       (or 0 for p=d, canonical basis by default)
#'     \item 'M', moments of order 1,2,3: will be computed if not provided.
#'     \item 'K', number of populations (estimated with rank of M2 if not given)
#'   }
#'
#' @return The estimated normalized parameters as columns of a matrix mu of size dxK
#'
#' @seealso \code{multiRun} to estimate statistics based on mu,
#'   and \code{generateSampleIO} for I/O random generation.
#'
#' @examples
#' io <- generateSampleIO(10000, 1/2, matrix(c(1,0,0,1),ncol=2), c(0,0), "probit")
#' mu <- computeMu(io$X, io$Y, list(K=2)) #or just X and Y for estimated K
#'
#' @export
computeMu <- function(X, Y, optargs=list())
{
  if (!is.matrix(X) || !is.numeric(X) || any(is.na(X)))
    stop("X: real matrix, no NA")
  n <- nrow(X)
  d <- ncol(X)
  if (!is.numeric(Y) || length(Y)!=n || any(Y!=0 & Y!=1))
    stop("Y: vector of 0 and 1, size nrow(X), no NA")
  if (!is.list(optargs))
    stop("optargs: list")

  # Step 0: Obtain the empirically estimated moments tensor, estimate also K
  M <- if (is.null(optargs$M)) computeMoments(X,Y) else optargs$M
  K <- optargs$K
  if (is.null(K))
  {
    # TODO: improve this basic heuristic
    Sigma <- svd(M[[2]])$d
    large_ratio <- ( abs(Sigma[-d] / Sigma[-1]) > 3 )
    K <- if (any(large_ratio)) max(2, which.min(large_ratio)) else d
  }
  else if (K > d)
    stop("K: integer >= 2, <= d")

  # Step 1: generate a family of d matrices to joint-diagonalize to increase robustness
  d <- ncol(X)
  fixed_design <- FALSE
  jd_nvects <- ifelse(!is.null(optargs$jd_nvects), optargs$jd_nvects, 0)
  if (jd_nvects == 0)
  {
    jd_nvects <- d
    fixed_design <- TRUE
  }
  M2_t <- array(dim=c(d,d,jd_nvects))
  for (i in seq_len(jd_nvects))
  {
    rho <- if (fixed_design) c(rep(0,i-1),1,rep(0,d-i)) else normalize( rnorm(d) )
    M2_t[,,i] <- .T_I_I_w(M[[3]],rho)
  }

  # Step 2: obtain factors u_i (and their inverse) from the joint diagonalisation of M2_t
  jd_method <- ifelse(!is.null(optargs$jd_method), optargs$jd_method, "uwedge")
  V <-
    if (jd_nvects > 1) {
      # NOTE: increasing itermax does not help to converge, thus we suppress warnings
      suppressWarnings({jd = jointDiag::ajd(M2_t, method=jd_method)})
      if (jd_method=="uwedge") jd$B else MASS::ginv(jd$A)
    }
    else
      eigen(M2_t[,,1])$vectors

  # Step 3: obtain final factors from joint diagonalisation of T(I,I,u_i)
  M2_t <- array(dim=c(d,d,K))
  for (i in seq_len(K))
    M2_t[,,i] <- .T_I_I_w(M[[3]],V[,i])
  suppressWarnings({jd = jointDiag::ajd(M2_t, method=jd_method)})
  U <- if (jd_method=="uwedge") MASS::ginv(jd$B) else jd$A
  mu <- normalize(U[,1:K])

  # M1 also writes M1 = sum_k coeff_k * mu_k, where coeff_k >= 0
  # ==> search decomposition of vector M1 onto the (truncated) basis mu (of size dxK)
  # This is a linear system mu %*% C = M1 with C of size K ==> C = psinv(mu) %*% M1
  C <- MASS::ginv(mu) %*% M[[1]]
  mu[,C < 0] <- - mu[,C < 0]
  mu
}
