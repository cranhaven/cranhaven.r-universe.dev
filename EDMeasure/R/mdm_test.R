#' Mutual Independence Tests
#'
#' \code{mdm_test} tests mutual independence of all components in \code{X},
#' where each component contains one variable (univariate) or more variables (multivariate).
#' All tests are implemented as permutation tests.
#'
#' @param X A matrix or data frame, where rows represent samples, and columns represent variables.
#' @param dim_comp The numbers of variables contained by all components in \code{X}.
#'   If omitted, each component is assumed to contain exactly one variable.
#' @param num_perm The number of permutation samples drawn to approximate the asymptotic distributions
#'   of mutual dependence measures. If omitted, an adaptive number is used.
#' @param type The type of mutual dependence measures, including
#' \itemize{
#'   \item \code{asym_dcov}: asymmetric measure \eqn{\mathcal{R}_n} based on distance covariance
#'     \eqn{\mathcal{V}_n}; 
#'   \item \code{sym_dcov}: symmetric measure \eqn{\mathcal{S}_n} based on distance covariance
#'     \eqn{\mathcal{V}_n}; 
#'   \item \code{comp}: complete measure \eqn{\mathcal{Q}_n} based on complete V-statistics;
#'   \item \code{comp_simp}: simplified complete measure \eqn{\mathcal{Q}_n^\star} based on
#'     incomplete V-statistics; 
#'   \item \code{asym_comp}: asymmetric measure \eqn{\mathcal{J}_n} based on complete measure
#'     \eqn{\mathcal{Q}_n}; 
#'   \item \code{asym_comp_simp}: simplified asymmetric measure \eqn{\mathcal{J}_n^\star} based on
#'     simplified complete measure \eqn{\mathcal{Q}_n^\star}; 
#'   \item \code{sym_comp}: symmetric measure \eqn{\mathcal{I}_n} based on complete measure
#'     \eqn{\mathcal{Q}_n}; 
#'   \item \code{sym_comp_simp}: simplified symmetric measure \eqn{\mathcal{I}_n^\star} based on
#'     simplified complete measure \eqn{\mathcal{Q}_n^\star}.
#' }
#' From experiments, \code{asym_dcov}, \code{sym_dcov}, \code{comp_simp} are recommended.
#'
#' @return \code{mdm_test} returns a list including the following components:
#' \item{stat}{The value of the mutual dependence measure.}
#' \item{pval}{The p-value of the mutual independence test.}
#'
#' @references Jin, Z., and Matteson, D. S. (2017).
#'   Generalizing Distance Covariance to Measure and Test Multivariate Mutual Dependence.
#'   arXiv preprint arXiv:1709.02532.
#'   \url{https://arxiv.org/abs/1709.02532}.
#'
#' @include mdm.R
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # X is a 10 x 3 matrix with 10 samples and 3 variables
#' X <- matrix(rnorm(10 * 3), 10, 3)
#'
#' # assume X = (X1, X2) where X1 is 1-dim, X2 is 2-dim
#' mdm_test(X, dim_comp = c(1, 2), type = "asym_dcov")
#'
#' # assume X = (X1, X2) where X1 is 2-dim, X2 is 1-dim
#' mdm_test(X, dim_comp = c(2, 1), type = "sym_dcov")
#'
#' # assume X = (X1, X2, X3) where X1 is 1-dim, X2 is 1-dim, X3 is 1-dim
#' mdm_test(X, dim_comp = c(1, 1, 1), type = "comp_simp")
#' }

mdm_test <- function(X, dim_comp = NULL, num_perm = NULL, type = "comp_simp") {
  X <- as.matrix(X)
  num_obs <- nrow(X)
  num_dim <- ncol(X)

  if (is.null(dim_comp)) {
    dim_comp <- rep(1, num_dim)
  }

  if (num_dim != sum(dim_comp)) {
    stop("The dimensions of X and components do not agree.")
  }

  num_comp <- length(dim_comp)

  out_sample <- mdm(X, dim_comp = dim_comp, dist_comp = TRUE, type = type)
  mdm_sample <- out_sample$stat
  dist_sample <- out_sample$dist

  if (is.null(num_perm)) {
    num_perm <- 200 + 5000 %/% num_obs
  }

  count <- 0
  for (i in 1:num_perm) {
    index_perm <- matrix(NA, num_comp, num_obs)

    # keep the order of the first component
    index_perm[1, ] <- 1:num_obs

    # permute the orders of other components
    for (j in 2:num_comp) {
      index_perm[j, ] <- sample(num_obs)
    }

    index_perm <- as.vector(index_perm) - 1

    if (type == "asym_dcov") {
      mdm_perm <- .C("dCov_asymmetric_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "sym_dcov") {
      mdm_perm <- .C("dCov_symmetric_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "comp") {
      mdm_perm <- .C("MDM_complete_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "comp_simp") {
      mdm_perm <- .C("MDM_complete_simple_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "asym_comp") {
      mdm_perm <- .C("MDM_asymmetric_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "asym_comp_simp") {
      mdm_perm <- .C("MDM_asymmetric_simple_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "sym_comp") {
      mdm_perm <- .C("MDM_symmetric_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else if (type == "sym_comp_simp") {
      mdm_perm <- .C("MDM_symmetric_simple_perm",
                     D = as.double(dist_sample),
                     V = as.double(numeric(1)),
                     NOBS = as.integer(num_obs),
                     NCOMP = as.integer(num_comp),
                     IPERM = as.integer(index_perm),
                     PACKAGE = "EDMeasure")$V
    } else {
      stop("Invalid type. Read ?mdm_test for proper syntax.")
    }

    if (mdm_perm >= mdm_sample) {
      count <- count + 1
    }
  }

  return(list(stat = mdm_sample, pval = count / num_perm))
}

