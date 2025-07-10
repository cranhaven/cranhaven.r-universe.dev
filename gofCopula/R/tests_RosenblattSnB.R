#' The SnB test based on the Rosenblatt transformation
#' 
#' \code{\link{gofRosenblattSnB}} contains the SnB gof test for copulae from
#' Genest (2009) and compares the empirical copula against a parametric
#' estimate of the copula derived under the null hypothesis. The margins can be
#' estimated by a bunch of distributions and the time which is necessary for
#' the estimation can be given. The approximate p-values are computed with a
#' parametric bootstrap, which computation can be accelerated by enabling
#' in-build parallel computation. The gof statistics are computed with the
#' function \code{\link[copula]{gofTstat}} from the package copula. It is possible to
#' insert datasets of all dimensions above 1 and the possible copulae are
#' \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"}, \code{"joe"}, \code{"amh"}, \code{"galambos"}, 
#' \code{"fgm"} and \code{"plackett"}. The parameter estimation is performed 
#' with pseudo maximum likelihood method. In case the estimation fails, 
#' inversion of Kendall's tau is used.
#' 
#' This test is based on the Rosenblatt probability integral transform which
#' uses the mapping \eqn{\mathcal{R}: (0,1)^d \rightarrow (0,1)^d}{R : (0,1)^d
#' -> (0,1)^d} to test the \eqn{H_0}{H0} hypothesis \deqn{C \in
#' \mathcal{C}_0}{C in Ccal0} with \eqn{\mathcal{C}_0}{Ccal0} as the true class
#' of copulae under \eqn{H_0}{H0}. Following Genest et al. (2009) ensures this
#' transformation the decomposition of a random vector \eqn{\mathbf{u} \in
#' [0,1]^d}{u in [0,1]^d} with a distribution into mutually independent
#' elements with a uniform distribution on the unit interval. The mapping
#' provides pseudo observations \eqn{E_i}{E[i]}, given by \deqn{E_1 =
#' \mathcal{R}(U_1), \dots, E_n = \mathcal{R}(U_n).}{E_1 = R(U_1), ..., E_n =
#' R(U_n).} The mapping is performed by assigning to every vector
#' \eqn{\mathbf{u}}{u} for \eqn{e_1 = u_1}{e[1] = u[1]} and for \eqn{i \in \{2,
#' \dots, d\}}{i in {2, ..., d}}, \deqn{e_i = \frac{\partial^{i-1} C(u_1,
#' \dots, u_i, 1, \dots, 1)}{\partial u_1 \cdots \partial u_{i-1}} /
#' \frac{\partial^{i-1} C(u_1, \dots, u_{i-1}, 1, \dots, 1)}{\partial u_1
#' \cdots \partial u_{i-1}}.}{e[i] = (d^(i-1) C(u[1], ..., u[i], 1, ..., 1))/(d
#' u[1] ... d u[i-1]) / (d^(i-1) C(u[1], ..., u[i-1], 1, ..., 1))/(d u[1] ... d
#' u[i-1]).} The resulting independence copula is given by
#' \eqn{C_{\bot}(\mathbf{u}) = u_1 \cdot \dots \cdot u_d}{Cind(u) = u[1] x ...
#' x u[d]}.
#' 
#' The test statistic \eqn{T} is then defined as
#' 
#' \deqn{T = n \int_{[0,1]^d} \{ D_n(\mathbf{u}) - C_{\bot}(\mathbf{u}) \}^2 d
#' (\mathbf{u})}{n int_{[0,1]^d} ( {Dn(u) - Cind(u)}^2 )du} with
#' \eqn{D_n(\mathbf{u}) = \frac{1}{n} \sum_{i = 1}^n \mathbf{I}(E_i \leq
#' \mathbf{u})}{Dn(u) = 1/n sum(E[i] <= u, i = 1, ..., n)}.
#' 
#' The approximate p-value is computed by the formula, see \pkg{copula},
#' 
#' \deqn{\sum_{b=1}^M \mathbf{I}(|T_b| \geq |T|) / M,}{sum(|T[b]| >= |T|, b=1,
#' .., M) / M,}
#' 
#' where \eqn{T} and \eqn{T_b}{T[b]} denote the test statistic and the
#' bootstrapped test statistc, respectively.
#' 
#' For small values of \code{M}, initializing the parallelisation via
#' \code{processes} does not make sense. The registration of the parallel
#' processes increases the computation time. Please consider to enable
#' parallelisation just for high values of \code{M}.
#' 
#' @param copula The copula to test for. Possible are \code{"normal"}, 
#' \code{"t"}, \code{"clayton"}, \code{"gumbel"}, \code{"frank"}, \code{"joe"}, 
#' \code{"amh"}, \code{"galambos"}, \code{"fgm"} and \code{"plackett"}.
#' @param x A matrix containing the data with rows being observations and
#' columns being variables.
#' @param param The copula parameter to use, if it shall not be estimated.
#' @param param.est Shall be either \code{TRUE} or \code{FALSE}. \code{TRUE}
#' means that \code{param} will be estimated.
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
#' @param dispstr A character string specifying the type of the symmetric
#' positive definite matrix characterizing the elliptical copula. Implemented
#' structures are "ex" for exchangeable and "un" for unstructured, see package
#' \code{copula}.
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
#' @references Christian Genest, Bruno Remillard, David Beaudoin (2009).
#' Goodness-of-fit tests for copulas: A review and a power study.
#' \emph{Insurance: Mathematics and Economics, Volume 44, Issue 2, April 2009,
#' Pages 199-213, ISSN 0167-6687}.
#' \doi{10.1016/j.insmatheco.2007.10.005}\cr \cr Marius
#' Hofert, Ivan Kojadinovic, Martin Maechler, Jun Yan (2014). copula:
#' Multivariate Dependence with Copulas. \emph{R package version 0.999-15.}.
#' \url{https://cran.r-project.org/package=copula} \cr \cr
#' @examples
#' 
#' data(IndexReturns2D)
#' 
#' gofRosenblattSnB("normal", IndexReturns2D, M = 10)
#' 
#' @export gofRosenblattSnB
gofRosenblattSnB <- function(copula = c("normal", "t", "clayton", "gumbel", 
                                        "frank", "joe", "amh", "galambos", 
                                        "fgm", "plackett"), x, param = 0.5, 
                             param.est = TRUE, df = 4, df.est = TRUE, 
                             margins = "ranks", flip = 0, M = 1000, 
                             dispstr = "ex", lower = NULL, upper = NULL, 
                             seed.active = NULL, processes = 1) {
  if (is.matrix(x) == FALSE) {
    stop("x must be a matrix")
  }
  if (length(copula) > 1) {
stop(
"'copula' has to be a vector of length 1. Please select only one copula."
)
  }
  if (is.element(copula, c("normal", "gaussian", "t", "clayton", "gumbel", 
                           "frank", "joe", "amh", "galambos", "fgm", 
                           "plackett")) == FALSE) {
    stop("This copula is not implemented for gofRosenblattSnB.")
  }
  if (is.element(copula, c("amh", "galambos", "fgm", "plackett")) & 
      dim(x)[2] > 2) {
    stop("This copula is not implemented for dimensions larger 2.")
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
  if (!inherits(param.est, "logical")) {
    stop("The argument 'param.est' has to be either 'TRUE' or 'FALSE'.")
  }
  if (!is.null(lower) & !is.numeric(lower) | !is.null(upper) & 
      !is.numeric(upper)) {
    stop("The arguments 'upper' and 'lower' have to be either NULL 
         or numeric.")
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
  if (!is.null(seed.active) & 
      all(!vapply(seed.active, function(x) x %% 1 == 0, TRUE))) {
stop(
"All seeds have to be whole numbers. Please check seed.active for non-whole 
numbers."
)
  }
  
  # estimation of margins and copula parameters
  erg <- .margins.param.est(copula = copula, margins = margins, x = x, 
                            param = param, param.est = param.est, df = df, 
                            df.est = df.est, dispstr = dispstr, lower = lower, 
                            upper = upper, flip = flip)
  # test with parametric bootstrap. Switch to Kendall's Tau if Maximum
  # Likelihood estimation fails
  res <- try(.gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "SnB", estim.method = "mpl", 
                          processes = processes, param.est = param.est, 
                          df.est = erg[[5]], dispstr = dispstr, 
                          param.margins = erg[[4]], margins = margins, 
                          seed.active = seed.active, lower = lower, 
                          upper = upper, flip = erg[[6]]), silent = TRUE)
  if (inherits(res, "try-error")) {
warning(
"Pseudo Maximum Likelihood estimation of the parameters while the bootstrapping 
procedure failed. The estimation was performed with inversion of Kendall's Tau. 
Therefore df.est was set to FALSE for the bootstrapping."
)
    res.f <- .gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "SnB", estim.method = "itau", 
                          processes = processes, param.est = param.est, 
                          df.est = FALSE, dispstr = dispstr, 
                          param.margins = erg[[4]], margins = margins, 
                          seed.active = seed.active, lower = lower, 
                          upper = upper, flip = erg[[6]])
    return(res.f)
  } else {
    return(res)
  }
}
