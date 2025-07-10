#' 2 dimensional gof test of Scaillet (2007)
#' 
#' \code{gofKernel} tests a 2 dimensional dataset with the Scaillet test for a
#' copula. The possible copulae are \code{"normal"}, \code{"t"}, 
#' \code{"clayton"}, \code{"gumbel"}, \code{"frank"}, \code{"joe"}, 
#' \code{"amh"}, \code{"galambos"}, \code{"huslerReiss"}, \code{"tawn"}, 
#' \code{"tev"}, \code{"fgm"} and \code{"plackett"}. The parameter
#' estimation is performed with pseudo maximum likelihood method. In case the
#' estimation fails, inversion of Kendall's tau is used. The approximate
#' p-values are computed with a parametric bootstrap, which computation can be
#' accelerated by enabling in-build parallel computation.
#' 
#' The Scaillet test is a kernel-based goodness-of-fit test with a fixed
#' smoothing parameter. For the copula density \eqn{c(\mathbf{u}, \theta)}{c(u,
#' theta)}, the corresponding kernel estimator is given by
#' \deqn{c_n(\mathbf{u}) = \frac{1}{n} \sum_{i=1}^n K_H[\mathbf{u} - (U_{i1},
#' \dots, U_{id})^{\top}], }{c_n(u) = 1/n sum(K_H[u - (U[i1], ..., U[id])^T],
#' i=1 ,..., n), } where \eqn{U_{ij}}{U[ij]} for \eqn{i = 1, \dots,n}{i = 1,
#' ...,n}; \eqn{j = 1, \dots,d}{j = 1, ...,d} are the pseudo observations,
#' \eqn{\mathbf{u} \in [0,1]^d}{u in [0,1]^d} and \eqn{K_H(y) =
#' K(H^{-1}y)/\det(H)}{KH(y) = K(H^(-1)y)/det(H)} for which a bivariate
#' quadratic kernel is used, as in Scaillet (2007). The matrix of smoothing
#' parameters is \eqn{H = 2.6073n^{-1/6} \hat{\Sigma}^{1/2}}{H = 2.6073n^{-1/6}
#' {Sigma_hat}^{1/2}} with \eqn{\hat{\Sigma}}{Sigma_hat} the sample covariance
#' matrix.  The test statistic is then given by \deqn{T = \int_{[0,1]^d}
#' \{c_n(\mathbf{u}) - K_H * c(\mathbf{u}, \theta_n)\} \omega(\mathbf{u}) d
#' \mathbf{u}, }{int_([0,1]^d) {c_n(u) - K_H * c(u, theta_n)} omega(u) d u,}
#' where \eqn{*}{*} denotes the convolution operator and \eqn{\omega}{omega} is
#' a weight function, see Zhang et al. (2015). The bivariate Gauss-Legendre
#' quadrature method is used to compute the integral in the test statistic
#' numerically, see Scaillet (2007).
#' 
#' The approximate p-value is computed by the formula \deqn{\sum_{b=1}^M
#' \mathbf{I}(|T_b| \geq |T|) / M,}{sum(|T[b]| >= |T|, b=1, .., M) / M,}
#' 
#' For small values of \code{M}, initializing the parallelisation via
#' \code{processes} does not make sense. The registration of the parallel
#' processes increases the computation time. Please consider to enable
#' parallelisation just for high values of \code{M}.
#' 
#' @param copula The copula to test for. Possible are \code{"normal"}, 
#' \code{"t"}, \code{"clayton"}, \code{"gumbel"}, \code{"frank"}, \code{"joe"}, 
#' \code{"amh"}, \code{"galambos"}, \code{"huslerReiss"}, \code{"tawn"}, 
#' \code{"tev"}, \code{"fgm"} and \code{"plackett"}.
#' @param x A matrix containing the data with rows being observations and
#' columns being variables.
#' @param param The parameter to be used.
#' @param param.est Shall be either \code{TRUE} or \code{FALSE}. \code{TRUE}
#' means that \code{param} will be estimated with a maximum likelihood
#' estimation.
#' @param df Degrees of freedom, if not meant to be estimated. Only necessary
#' if tested for \code{"t"}-copula.
#' @param df.est Indicates if \code{df} shall be estimated. Has to be either
#' \code{FALSE} or \code{TRUE}, where \code{TRUE} means that it will be
#' estimated.
#' @param margins Specifies which estimation method for the margins shall be
#' used. The default is \code{"ranks"}, which is the standard approach to
#' convert data in such a case. Alternatively the following distributions can
#' be specified: \code{"beta"}, \code{"cauchy"}, Chi-squared (\code{"chisq"}),
#' \code{"f"}, \code{"gamma"}, Log normal (\code{"lnorm"}), Normal
#' (\code{"norm"}), \code{"t"}, \code{"weibull"}, Exponential (\code{"exp"}).
#' Input can be either one method, e.g. \code{"ranks"}, which will be used for
#' estimation of all data sequences. Also an individual method for each margin
#' can be specified, e.g. \code{c("ranks", "norm", "t")} for 3 data sequences.
#' If one does not want to estimate the margins, set it to \code{NULL}.
#' @param flip The control parameter to flip the copula by 90, 180, 270 degrees
#' clockwise. Only applicable for bivariate copula. Default is 0 and possible 
#' inputs are 0, 90, 180, 270 and NULL.
#' @param M Number of bootstrapping loops.
#' @param MJ Size of bootstrapping sample.
#' @param dispstr A character string specifying the type of the symmetric
#' positive definite matrix characterizing the elliptical copula. Implemented
#' structures are "ex" for exchangeable and "un" for unstructured, see package
#' \code{copula}.
#' @param delta.J Scaling parameter for the matrix of smoothing parameters.
#' @param nodes.Integration Number of knots of the bivariate Gauss-Legendre
#' quadrature.
#' @param lower Lower bound for the maximum likelihood estimation of the copula
#' parameter. The constraint is also active in the bootstrapping procedure. The
#' constraint is not active when a switch to inversion of Kendall's tau is
#' necessary. Default \code{NULL}.
#' @param upper Upper bound for the maximum likelihood estimation of the copula
#' parameter. The constraint is also active in the bootstrapping procedure. The
#' constraint is not active when a switch to inversion of Kendall's tau is
#' necessary. Default \code{NULL}.
#' @param seed.active Has to be either an integer or a vector of M+1 integers.
#' If an integer, then the seeds for the bootstrapping procedure will be
#' simulated. If M+1 seeds are provided, then these seeds are used in the
#' bootstrapping procedure. Defaults to \code{NULL}, then \code{R} generates
#' the seeds from the computer runtime. Controlling the seeds is useful for
#' reproducibility of a simulation study to compare the power of the tests or
#' for reproducibility of an empirical study.
#' @param processes The number of parallel processes which are performed to
#' speed up the bootstrapping. Shouldn't be higher than the number of logical
#' processors. Please see the details.
#' @return An object of the \code{class} gofCOP with the components
#' \item{method}{a character which informs about the performed analysis}
#' \item{copula}{the copula tested for} \item{margins}{the method used to
#' estimate the margin distribution.} \item{param.margins}{the parameters of
#' the estimated margin distributions. Only applicable if the margins were not
#' specified as \code{"ranks"} or \code{NULL}.} \item{theta}{dependence
#' parameters of the copulae} \item{df}{the degrees of freedem of the copula.
#' Only applicable for t-copula.} \item{res.tests}{a matrix with the p-values
#' and test statistics of the hybrid and the individual tests}
#' @references Zhang, S., Okhrin, O., Zhou, Q., and Song, P.. Goodness-of-fit
#' Test For Specification of Semiparametric Copula Dependence Models.
#' \emph{Journal of Econometrics, 193, 2016, pp. 215-233}
#' \doi{10.1016/j.jeconom.2016.02.017} \cr \cr Scaillet, O.
#' (2007). Kernel based goodness-of-fit tests for copulas with fixed smoothing
#' parameters. \emph{Journal of Multivariate Analysis, 98:533-543}
#' @examples
#' 
#' data(IndexReturns2D)
#' 
#' gofKernel("normal", IndexReturns2D, M = 5, MJ = 5)
#' 
#' @export gofKernel
gofKernel <- function(copula = c("normal", "t", "clayton", "gumbel", "frank", 
                                 "joe", "amh", "galambos", "huslerReiss", 
                                 "tawn", "tev", "fgm", "plackett"), x, 
                      param = 0.5, param.est = TRUE, df = 4, df.est = TRUE, 
                      margins = "ranks", flip = 0, M = 1000, MJ = 100, 
                      dispstr = "ex", delta.J = 0.5, nodes.Integration = 12, 
                      lower = NULL, upper = NULL, seed.active = NULL, 
                      processes = 1) {
  if (is.matrix(x) == FALSE) {
    stop("x must be a matrix")
  }
  if (length(copula) > 1) {
stop(
"'copula' has to be a vector of length 1. Please select only one copula."
)
  }
  if (dim(x)[2] != 2) {
    stop("x must be of dimension 2")
  }
  if (is.element(copula, c("normal", "gaussian", "t", "clayton", "gumbel", 
                           "frank", "joe", "amh", "galambos", "huslerReiss", 
                           "tawn", "tev", "fgm", "plackett")) == FALSE) {
    stop("This copula is not implemented for gofKernel.")
  }
  if (!is.element(dispstr, c("ex", "un"))) {
stop(
"dispstr has to be either 'ex' or 'un'. See documentation for more information."
)
  }
  if (!is.numeric(processes)) {
    stop("The argument 'processes' has to be a numeric.")
  }
  if (processes %% 1 != 0 | processes < 1) {
    stop("The argument 'processes' has to be a positive integer.")
  }
  if (!is.numeric(M)) {
    stop("The argument 'M' has to be a numeric.")
  }
  if (M %% 1 != 0 | M < 0) {
    stop("The argument 'M' has to be a positive integer.")
  }
  if (!is.numeric(param)) {
    stop("The argument 'param' has to be a numeric.")
  }
  if (!is.numeric(df)) {
    stop("The argument 'df' has to be a numeric.")
  }
  if (!is.numeric(delta.J)) {
    stop("The argument 'delta.J' has to be a numeric.")
  }
  if (delta.J <= 0) {
    stop("The argument 'delta.J' has to be larger 0.")
  }
  if (!is.numeric(nodes.Integration)) {
    stop("The argument 'nodes.Integration' has to be a numeric.")
  }
  if (nodes.Integration %% 1 != 0 | nodes.Integration < 0) {
    stop("The argument 'nodes.Integration' has to be a positive integer.")
  }
  if (!is.numeric(MJ)) {
    stop("The argument 'MJ' has to be a numeric.")
  }
  if (MJ %% 1 != 0 | MJ < 0) {
    stop("The argument 'MJ' has to be a positive integer.")
  }
  if (!inherits(param.est, "logical")) {
    stop("The argument 'param.est' has to be either 'TRUE' or 'FALSE'.")
  }
  if (!is.null(lower) & !is.numeric(lower) | !is.null(upper) & 
      !is.numeric(upper)) {
    stop("The arguments 'upper' and 'lower' have to be either NULL or numeric.")
  }
  if (copula == "gumbel" & param.est == FALSE & param <= 1) {
    param <- 1.5
warning(
"When copula is 'gumbel', 'param' has to be larger 1. Because 'param.est' was 
set to 'FALSE', 'param' was set to 1.5 as default value for 'gumbel' copula."
)
  }
  if (!inherits(df.est, "logical")) {
    stop("The argument 'df.est' has to be either 'TRUE' or 'FALSE'.")
  }
  if (!is.null(seed.active) & length(seed.active) != 1 & 
      length(seed.active) != (M + 1)) {
    stop("The seed has to be an integer or a vector of M+1 seeds.")
  }
  if (!is.null(seed.active) & length(seed.active) == 1) {
    set.seed(seed.active)
    RNGsetting <- RNGkind()
    RNGkind(sample.kind = "default")
    on.exit(RNGkind(sample.kind = RNGsetting[3]))
    seed.active <- sample(x = 2147483647, size = M + 1)
  }
  if (!is.null(seed.active) & all(!vapply(seed.active, 
                                          function(x) x %% 1 == 0, TRUE))) {
stop(
"All seeds have to be whole numbers. Please check seed.active for non-whole 
numbers."
)
  }
  add.parameters <- list(nodes.Integration, MJ, delta.J)

  # estimation of margins and copula parameters
  erg <- .margins.param.est(copula = copula, margins = margins, x = x, 
                            param = param, param.est = param.est, df = df, 
                            df.est = df.est, dispstr = dispstr, lower = lower, 
                            upper = upper, flip = flip)
  # test with parametric bootstrap. Switch to Kendall's Tau if Maximum
  # Likelihood estimation fails
  res <- try(.gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "Kernel", estim.method = "mpl", 
                          processes = processes, 
                          add.parameters = add.parameters, 
                          param.est = param.est, df.est = erg[[5]], 
                          dispstr = dispstr, param.margins = erg[[4]], 
                          margins = margins, seed.active = seed.active, 
                          lower = lower, upper = upper, flip = erg[[6]]), 
             silent = TRUE)
  if (inherits(res, "try-error")) {
warning(
"Pseudo Maximum Likelihood estimation of the parameters while the bootstrapping 
procedure failed. The estimation was performed with inversion of Kendall's Tau. 
Therefore df.est was set to FALSE for the bootstrapping."
)
    res.f <- .gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "Kernel", estim.method = "itau", 
                          processes = processes, 
                          add.parameters = add.parameters, 
                          param.est = param.est, df.est = FALSE, 
                          dispstr = dispstr, param.margins = erg[[4]], 
                          margins = margins, seed.active = seed.active, 
                          lower = lower, upper = upper, flip = erg[[6]])
    return(res.f)
  } else {
    return(res)
  }
}
