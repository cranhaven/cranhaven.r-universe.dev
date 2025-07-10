#' 2 and 3 dimensional gof test based on the in-and-out-of-sample approach
#' 
#' \code{gofPIOSTn} tests a 2 or 3 dimensional dataset with the PIOS test for a
#' copula. The possible copulae are \code{"normal"}, \code{"t"}, 
#' \code{"clayton"}, \code{"gumbel"}, \code{"frank"}, \code{"joe"}, 
#' \code{"amh"}, \code{"galambos"}, \code{"fgm"} and \code{"plackett"}. 
#' The parameter estimation is performed with pseudo maximum likelihood method. 
#' In case the estimation fails, inversion of Kendall's tau is used. The 
#' approximate p-values are computed with a semiparametric bootstrap, which 
#' computation can be accelerated by enabling in-build parallel computation.
#' 
#' The "Tn" test is introduced in Zhang et al. (2015). It tests the
#' \eqn{H_0}{H0} hypothesis \deqn{H_0 : C_0 \in \mathcal{C}.}{H0 : C0 in Ccal.}
#' For the test blocks of length \code{m} are constructed out of the data. The
#' test compares then the pseudo likelihood of the data in each block with the
#' overall parameter and with the parameter by leaving out the data in the
#' block. By this procedure can be determined if the data in the block
#' influence the parameter estimation significantly. The test statistic is
#' defined as \deqn{T = \sum_{b=1}^M \sum_{k=1}^m [l\{U_k^b;\theta_n \} -
#' l\{U_k^b;\theta_n^{-b} \}]}{T = sum(sum(l(U_k^b;theta_n ) -
#' l(U_k^b;theta_n^(-b) ), k=1, ...,m ), b=1, ...,B)}
#' 
#' with the pseudo observations \eqn{U_{ij}}{U[ij]} for \eqn{i = 1, \dots,n}{i
#' = 1, ...,n}; \eqn{j = 1, \dots,d}{j = 1, ...,d} and \deqn{\theta_n = \arg
#' \min_{\theta} \sum_{i=1}^n l(U_i; \theta)}{theta_n = arg max_theta
#' sum(l(U_i; theta), i=1, ..., n)} and \deqn{\theta_n^{-b} = \arg
#' \min_{\theta} \sum_{b^{'} \neq b}^M \sum_{i=1}^m l(U_i^{b^{'}}; \theta),
#' b=1, \dots, M.}{theta_n^(-b) = arg max_theta sum(sum(l(U_i^(b^'); theta),
#' i=1, ..., m), b^'=1, ..., M, b^' != b), b = 1, ..., M.}
#' 
#' The approximate p-value is computed by the formula \deqn{\sum_{b=1}^M
#' \mathbf{I}(|T_b| \geq |T|) / M,}{sum(|T[b]| >= |T|, b=1, .., M) / M,}
#' 
#' The applied estimation method is the two-step pseudo maximum likelihood
#' approach, see Genest and Rivest (1995).
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
#' @param param The parameter to be used.
#' @param param.est Shall be either \code{TRUE} or \code{FALSE}. \code{TRUE}
#' means that \code{param} will be estimated with a maximum likelihood
#' estimation.
#' @param df Degrees of freedom, if not meant to be estimated. Only necessary
#' if tested for \code{"t"}-copula. For computational reasons the entry is
#' limited to 60 degrees of freedom.
#' @param df.est Indicates if \code{df} shall be estimated. Has to be either
#' \code{FALSE} or \code{TRUE}, where \code{TRUE} means that it will be
#' estimated. For computational reasons the estimate is limited to 60 degrees
#' of freedom.
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
#' @param m Length of blocks.
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
#' \doi{10.1016/j.jeconom.2016.02.017} \cr \cr Genest, C., K.
#' G. and Rivest, L.-P. (1995). A semiparametric estimation procedure of
#' dependence parameters in multivariate families of distributions.
#' \emph{Biometrika, 82:534-552}
#' @examples
#' 
#' data(IndexReturns2D)
#' 
#' gofPIOSTn("normal", IndexReturns2D, M = 10)
#' 
#' @export gofPIOSTn
gofPIOSTn <- function(copula = c("normal", "t", "clayton", "gumbel", "frank", 
                                 "joe", "amh", "galambos", "fgm", "plackett"), 
                      x, param = 0.5, 
                      param.est = TRUE, df = 4, df.est = TRUE, 
                      margins = "ranks", flip = 0, M = 1000, dispstr = "ex", 
                      m = 1, lower = NULL, upper = NULL, seed.active = NULL, 
                      processes = 1) {
  if (is.matrix(x) == FALSE) {
    stop("x must be a matrix")
  }
  if (length(copula) > 1) {
stop(
"'copula' has to be a vector of length 1. Please select only one copula."
)
  }
  if (dim(x)[2] < 2 || dim(x)[2] > 3) {
    stop("x must be of dimension 2 or 3")
  }
  if (is.element(copula, c("normal", "gaussian", "t", "clayton", "gumbel", 
                           "frank", "joe", "amh", "galambos", "fgm", 
                           "plackett")) == FALSE) {
    stop("This copula is not implemented for gofPIOSTn.")
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
  if (!is.numeric(m)) {
    stop("The argument 'm' has to be a numeric.")
  }
  n <- dim(x)[1]
  if (n %% m != 0 | m < 1) {
stop(
"The length of the blocks, 'm', has to be larger 1 and a divisor of the length 
of the data sequence."
)
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
  B <- n / m
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
  
cat(
"The estimation is running. For the PIOSTn test, depending on the 
dimensionality and complexity of the data, this might take a while.", 
fill = TRUE)
  
  # estimation of margins and copula parameters
  erg <- .margins.param.est(copula = copula, margins = margins, x = x, 
                            param = param, param.est = param.est, df = df, 
                            df.est = df.est, dispstr = dispstr, lower = lower, 
                            upper = upper, flip = flip)
  add.parameters <- list(B, m, "mpl")
  if (copula == "t") {
    erg[[1]]@parameters[2] <- min(erg[[1]]@parameters[2], 60)
  }
  # test with parametric bootstrap. Switch to Kendall's Tau if Maximum
  # Likelihood estimation fails
  res <- try(.gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "Tn", estim.method = "mpl", 
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
    add.parameters <- list(B, m, "itau")
    res.f <- .gofCopulapb(copula = erg[[1]], x = erg[[2]], M = M, 
                          method = "Tn", estim.method = "itau", 
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
