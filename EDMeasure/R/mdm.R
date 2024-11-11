#' Mutual Dependence Measures
#'
#' \code{mdm} measures mutual dependence of all components in \code{X},
#' where each component contains one variable (univariate) or more variables (multivariate).
#'
#' @param X A matrix or data frame, where rows represent samples, and columns represent variables.
#' @param dim_comp The numbers of variables contained by all components in \code{X}.
#'   If omitted, each component is assumed to contain exactly one variable.
#' @param dist_comp Logical. If \code{TRUE}, the distances between all components from all samples
#'   in \code{X} will be returned.
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
#' @return \code{mdm} returns a list including the following components:
#' \item{stat}{The value of the mutual dependence measure.}
#' \item{dist}{The distances between all components from all samples.}
#'
#' @references Jin, Z., and Matteson, D. S. (2017).
#'   Generalizing Distance Covariance to Measure and Test Multivariate Mutual Dependence.
#'   arXiv preprint arXiv:1709.02532.
#'   \url{https://arxiv.org/abs/1709.02532}.
#'
#' @export
#'
#' @examples
#' # X is a 10 x 3 matrix with 10 samples and 3 variables
#' X <- matrix(rnorm(10 * 3), 10, 3)
#'
#' # assume X = (X1, X2) where X1 is 1-dim, X2 is 2-dim
#' mdm(X, dim_comp = c(1, 2), type = "asym_dcov")
#'
#' # assume X = (X1, X2) where X1 is 2-dim, X2 is 1-dim
#' mdm(X, dim_comp = c(2, 1), type = "sym_dcov")
#'
#' # assume X = (X1, X2, X3) where X1 is 1-dim, X2 is 1-dim, X3 is 1-dim
#' mdm(X, dim_comp = c(1, 1, 1), type = "comp_simp")

mdm <- function(X, dim_comp = NULL, dist_comp = FALSE, type = "comp_simp") {
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
  index_comp <- cumsum(c(1, dim_comp)) - 1
  X <- as.vector(t(X))

  if (type == "asym_dcov") {
    out <- .C("dCov_asymmetric",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "sym_dcov") {
    out <- .C("dCov_symmetric",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "comp") {
    out <- .C("MDM_complete",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "comp_simp") {
    out <- .C("MDM_complete_simple",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "asym_comp") {
    out <- .C("MDM_asymmetric",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "asym_comp_simp") {
    out <- .C("MDM_asymmetric_simple",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "sym_comp") {
    out <- .C("MDM_symmetric",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else if (type == "sym_comp_simp") {
    out <- .C("MDM_symmetric_simple",
              X = as.double(X),
              D = as.double(numeric(num_comp * num_obs * num_obs)),
              V = as.double(numeric(1)),
              NOBS = as.integer(num_obs),
              NDIM = as.integer(num_dim),
              NCOMP = as.integer(num_comp),
              ICOMP = as.integer(index_comp),
              PACKAGE = "EDMeasure")
  } else {
    stop("Invalid type. Read ?mdm for proper syntax.")
  }

  if (dist_comp) {
    return(list(stat = out$V, dist = out$D))
  } else {
    return(list(stat = out$V))
  }
}

