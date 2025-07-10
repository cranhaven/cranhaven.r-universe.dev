#' 2 dimensional gof tests based on White's information matrix equality.
#' 
#' \code{\link{gofWhite}} tests a given 2 dimensional dataset for a copula with
#' the gof test based on White's information matrix equality. The possible
#' copulae are \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"} and \code{"joe"}. See for reference Schepsmeier et al. (2015). 
#' The parameter estimation is performed with pseudo maximum likelihood method. 
#' In case the estimation fails, inversion of Kendall's tau is used. The margins
#' can be estimated by a bunch of distributions and the time which is necessary
#' for the estimation can be given. The approximate p-values are computed with
#' a parametric bootstrap, which computation can be accelerated by enabling
#' in-build parallel computation. The computation of the test statistic and
#' p-values is performed by corresponding functions from the \code{VineCopula}
#' package.
#' 
#' The details are obtained from Schepsmeier et al. (2015) who states that this
#' test uses the information matrix equality of White (1982). Under correct
#' model specification is the Fisher Information equivalently calculated as
#' minus the expected Hessian matrix or as the expected outer product of the
#' score function. The null hypothesis is \deqn{H_0 : \mathbf{V}(\theta) +
#' \mathbf{S}(\theta) = 0}{H0 : V(theta) + S(theta) = 0} where
#' \eqn{\mathbf{V}(\theta)}{V(theta)} is the expected Hessian matrix and
#' \eqn{\mathbf{S}(\theta)}{S(theta)} is the expected outer product of the
#' score function.
#' 
#' The test statistic is derived by \deqn{T_n = n(\bar{d}(\theta_n))^\top
#' A_{\theta_n}^{-1} \bar{d}(\theta_n)}{T_n = n(dbar(theta_n))^T
#' A_(theta_n)^(-1) dbar(theta_n)} with \deqn{\bar{d}(\theta_n) = \frac{1}{n}
#' \sum_{i=1}^n vech(\mathbf{V}_n(\theta_n|\mathbf{u}) +
#' \mathbf{S}_n(\theta_n|\mathbf{u})),}{dbar(theta_n) = 1/n
#' sum(vech(V(theta_n|u) + S(theta_n|u)), i=1, ..., n),}
#' 
#' \deqn{d(\theta_n) = vech(\mathbf{V}_n(\theta_n|\mathbf{u}) +
#' \mathbf{S}_n(\theta_n|\mathbf{u})),}{d(theta_n) = vech(V(theta_n|u) +
#' S(theta_n|u)),}
#' 
#' \deqn{A_{\theta_n} = \frac{1}{n} \sum_{i=1}^n (d(\theta_n) - D_{\theta_n}
#' \mathbf{V}_n(\theta_n)^{-1} \delta l(\theta_n))(d(\theta_n) - D_{\theta_n}
#' \mathbf{V}_n(\theta_n)^{-1} \delta l(\theta_n))^\top}{Atheta_n] = 1/n
#' sum(d(theta_n) - D(theta_n) V_n(theta_n)^(-1) d l(theta_n)d(theta_n) -
#' D(theta_n) V_n(theta_n)^(-1) d l(theta_n)^T, i=1, ..., n)} and
#' \deqn{D_{\theta_n} = \frac{1}{n} \sum_{i=1}^n [\delta_{\theta_k}
#' d_l(\theta_n)]_{l=1, \dots, \frac{p(p+1)}{2}, k=1, \dots, p}}{D(theta_n =
#' 1/n sum(d_(theta_k) d[l](theta_n)_(l=1, ...,(p(p+1))/2, k=1, ..., p), i=1,
#' ..., n)} where \eqn{l(\theta_n)}{l(theta_n)} represents the log likelihood
#' function and \eqn{p} is the length of the parameter vector
#' \eqn{\theta}{theta}.
#' 
#' The test statistic will be rejected if \deqn{T > (1 - \alpha)
#' (\chi^2_{p(p+1)/2})^{-1}.}{T > (1 - alpha) (chi^2_(p(p+1)/2))^(-1).}
#' 
#' For small values of \code{M}, initializing the parallelisation via
#' \code{processes} does not make sense. The registration of the parallel
#' processes increases the computation time. Please consider to enable
#' parallelisation just for high values of \code{M}.
#' 
#' Please note, the test gofWhite may be unstable for t-copula. Please handle
#' the results carefully.
#' 
#' @param copula The copula to test for. Possible are the copulae
#' \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"} and \code{"joe"}. 
#' @param x A matrix containing the data with rows being observations and
#' columns being variables.
#' @param param The parameter to be used.
#' @param param.est Shall be either \code{TRUE} or \code{FALSE}. \code{TRUE}
#' means that \code{param} will be estimated with a maximum likelihood
#' estimation.
#' @param df The degrees of freedom, if not meant to be estimated. Only
#' necessary if tested for \code{"t"}-copula.
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
#' @param M Number of bootstrap samples.
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
#' @references Ulf Schepsmeier, Jakob Stoeber, Eike Christian Brechmann,
#' Benedikt Graeler (2015). VineCopula: Statistical Inference of Vine Copulas.
#' \emph{R package version 1.4.}.
#' \url{https://cran.r-project.org/package=VineCopula} \cr \cr Schepsmeier, U.
#' and J. Stoeber (2014). Derivatives and Fisher information of bivariate
#' copulas. \emph{Statistical Papers, 55(2), 525-542.}
#' \url{https://link.springer.com/article/10.1007/s00362-013-0498-x} \cr \cr
#' Stoeber, J. and U. Schepsmeier (2013). Estimating standard errors in regular
#' vine copula models \emph{Computational Statistics, 28 (6), 2679-2707} \cr
#' \cr Schepsmeier, U. (2015). Efficient information based goodness-of-fit
#' tests for vine copula models with fixed margins. \emph{Journal of
#' Multivariate Analysis 138, 34-52.} Schepsmeier, U. (2014). A goodness-of-fit
#' test for regular vine copula models.
#' @examples
#' 
#' data(IndexReturns2D)
#' 
#' gofWhite("normal", IndexReturns2D, M = 10)
#' 
#' @export gofWhite
gofWhite <- function(copula = c("normal", "t", "clayton", "gumbel", 
                                "frank", "joe"), x, param = 0.5, 
                     param.est = TRUE, df = 4, df.est = TRUE, 
                     margins = "ranks", flip = 0, M = 1000, lower = NULL, 
                     upper = NULL, seed.active = NULL, processes = 1) {
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
  if (is.element(copula, c("amh", "galambos", "huslerReiss", "tawn", "tev", 
                           "fgm", "plackett"))) {
    stop("This copula is not implemented for the White test.")
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
  if (!inherits(param.est, "logical")) {
    stop("The argument 'param.est' has to be either 'TRUE' or 'FALSE'.")
  }
  if (!is.null(lower) & !is.numeric(lower) | !is.null(upper) & 
      !is.numeric(upper)) {
stop(
"The arguments 'upper' and 'lower' have to be either NULL or numeric."
)
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

  if (copula == "independence") {
    fam <- 0
  } else if (copula == "normal" || copula == "gaussian") {
    fam <- 1
  } else if (copula == "t") {
    fam <- 2
  } else if (copula == "clayton") {
    fam <- 3
  } else if (copula == "gumbel") {
    fam <- 4
  } else if (copula == "frank") {
    fam <- 5
  } else if (copula == "joe") {
    fam <- 6
  } else {
    stop("This copula is not implemented for gofWhite.")
  }
  add.parameters <- list(fam)

  # estimation of margins and copula parameters
  erg <- .margins.param.est(copula = copula, margins = margins, x = x, 
                            param = param, param.est = param.est, df = df, 
                            df.est = df.est, dispstr = "ex", lower = lower, 
                            upper = upper, flip = flip)
  # test with parametric bootstrap. Switch to Kendall's Tau if Maximum
  # Likelihood estimation fails
  res <- try(.gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "White", estim.method = "mpl", 
                          processes = processes, 
                          add.parameters = add.parameters, 
                          param.est = param.est, df.est = erg[[5]], 
                          dispstr = "ex", param.margins = erg[[4]], 
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
                          method = "White", estim.method = "itau", 
                          processes = processes, 
                          add.parameters = add.parameters, 
                          param.est = param.est, df.est = FALSE, dispstr = "ex", 
                          param.margins = erg[[4]], margins = margins, 
                          seed.active = seed.active, lower = lower, 
                          upper = upper, flip = erg[[6]])
    return(res.f)
  } else {
    return(res)
  }
}
