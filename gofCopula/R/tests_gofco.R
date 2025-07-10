#' Interface with copula class
#' 
#' \code{\link{gofco}} is an interface with the \code{copula} package. It reads
#' out the information from a copula class object and hands them over to a
#' specified gof test or set of tests.
#' 
#' The function reads out the arguments in the copula class object. If the
#' dependence parameter is not specified in the object, it is estimated. In
#' case that the object describes a "t"-copula, then the same holds for the
#' degrees of freedom. The dimension is not extracted from the object. It is
#' obtained from the inserted dataset.
#' 
#' When more than one test shall be performed, the hybrid test is computed too.
#' 
#' For small values of \code{M}, initializing the parallelisation via
#' \code{processes} does not make sense. The registration of the parallel
#' processes increases the computation time. Please consider to enable
#' parallelisation just for high values of \code{M}.
#' 
#' @param copulaobject An object with of class \code{copula} from the copula
#' package.
#' @param x A matrix containing the data with rows being observations and
#' columns being variables.
#' @param tests A character vector which indicates the tests to use. Possible
#' choices are the individual tests implemented in this package.
#' @param customTests A character vector which indicates the customized test to
#' use, if any. The test has to be loaded into the workspace. Currently the
#' function containing the test has to have 2 arguments, the first one for the
#' dataset and the second one for the copula to test for. The arguments have to
#' be named "x" and "copula" respectively.
#' @param margins Specifies which estimation method for the margins shall be
#' used. The default is \code{"ranks"}, which is the standard approach to
#' convert data in such a case. Alternatively can the following distributions
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
#' @param M Number of bootstrapping samples in the single tests.
#' @param MJ Size of bootstrapping sample. Only necessary if the test
#' \code{gofKernel} is part of \code{testset}.
#' @param dispstr A character string specifying the type of the symmetric
#' positive definite matrix characterizing the elliptical copula. Implemented
#' structures are "ex" for exchangeable and "un" for unstructured, see package
#' \code{copula}.
#' @param m Length of blocks. Only necessary if the test \code{gofPIOSTn} is
#' part of \code{testset}.
#' @param delta.J Scaling parameter for the matrix of smoothing parameters.
#' Only necessary if the test \code{gofKernel} is part of \code{testset}.
#' @param nodes.Integration Number of knots of the bivariate Gauss-Legendre
#' quadrature. Only necessary if the test \code{gofKernel} is part of
#' \code{testset}.
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
#' processors.
#' @return An object of the \code{class} gofCOP with the components
#' \item{method}{a character which informs about the performed analysis}
#' \item{copula}{the copula tested for} \item{margins}{the method used to
#' estimate the margin distribution.} \item{param.margins}{the parameters of
#' the estimated margin distributions. Only applicable if the margins were not
#' specified as \code{"ranks"} or \code{NULL}.} \item{theta}{dependence
#' parameters of the copulae} \item{df}{the degrees of freedem of the copula.
#' Only applicable for t-copula.} \item{res.tests}{a matrix with the p-values
#' and test statistics of the hybrid and the individual tests}
#' @references Yan, Jun. Enjoy the joy of copulas: with a package copula.
#' Journal of Statistical Software 21.4 (2007): 1-21.
#' @examples
#' 
#' data(IndexReturns2D)
#' copObject = normalCopula(param = 0.5)
#' 
#' gofco(copObject, x = IndexReturns2D, tests = c("gofPIOSRn", "gofKernel"), 
#'       M = 20)
#' 
#' @export gofco
gofco <- function(copulaobject, x, tests = c("gofPIOSRn", "gofKernel"), 
                  customTests = NULL, margins = "ranks", flip = 0,
                  M = 1000, MJ = 100, dispstr = "ex",
                  m = 1, delta.J = 0.5, nodes.Integration = 12,
                  lower = NULL, upper = NULL,
                  seed.active = NULL, processes = 1) {
  if (is.matrix(x) == FALSE) {
    stop("x must be a matrix")
  }
  if (!is.null(tests) & any(!is.element(tests, 
                                        gofTest4Copula(as.character(substr(
                                          class(copulaobject), 1, 
                                          nchar(class(copulaobject)) - 6)), 
                                          dim(x)[2]))) || 
      !is.null(customTests) & any(!is.element(customTests, ls(".GlobalEnv")))) {
stop(
"At least one of the tests in 'tests' is not implemented, cannot handle a 
dataset of this dimension or at least one of the tests in 'customTests' does not 
match any function in the global workspace. Please check if it is correctly 
spelled in the function call."
)
  }
  if (any(!vapply(customTests, 
                  function(x) all(names(formals(x)) %in% c("x", "copula")), 
                  TRUE))) {
stop(
"At least one function in 'customTest' does not follow the requirements for the 
arguments. The first argument for the dataset has to be called 'x', the second 
one for the copula has to be called 'copula'."
)
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
  if (!is.numeric(m)) {
    stop("The argument 'm' has to be a numeric.")
  }
  n <- dim(x)[1]
  if (n %% m != 0 | m < 1) {
    stop("The length of the blocks, 'm', has to be larger 1 and a divisor 
         of the length of the data sequence.")
  }
  if (!is.null(lower) & !is.numeric(lower) | !is.null(upper) & 
      !is.numeric(upper)) {
    stop("The arguments 'upper' and 'lower' have to be either NULL or numeric.")
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
  
  # estimate margins
  if (length(margins) > 1 & length(margins) != dim(x)[2]) {
stop(paste(
"If length(margins)>1, then the number of entries has to fit the number of data 
sequences. You included ", length(margins), " distributions, though ", 
dim(x)[2], " data sequences are used. Please amend and run the function again."
))
  }
  if (any(!is.element(margins, c("ranks", "beta", "cauchy", "chisq", "f", 
                                 "gamma", "lnorm", "norm", "t", "weibull", 
                                 "exp")))) {
stop(paste(
"At least one of the distributions in `margins' is not implemented. Please amend 
and run the function again. It has to be either of `ranks', `beta', `cauchy', 
`chisq', `f', `gamma', `lnorm', `norm', `t', `weibull', `exp'."
))
  }
  
  param.margins <- NULL
  if (!is.null(margins)) {
    cat(paste("The margins will be estimated as: ", 
              paste0(margins, collapse = ", "), sep = ""), fill = TRUE)
    
    res.margins <- .margins(x, margins)
    param.margins <- list()
    if (length(margins) == 1) {
      margins.dummy <- rep(margins, dim(x)[2])
    } else {
      margins.dummy <- margins
    }
    for (i in seq_along(margins.dummy)) {
      if (margins.dummy[i] == "ranks") {
        x[, i] <- res.margins[[i]][[1]]
      } else {
        param.margins[[i]] <- res.margins[[i]][[1]]
        x[, i] <- res.margins[[i]][[2]]
      }
    }
  } else {
    if (any(x > 1) || any(x < 0)) {
cat(
"The observations aren't in [0,1]. This will lead to errors while the 
estimation. Please set 'margins' to any of the incorporated functions.", 
fill = TRUE)
    }
  }
  
  # extracting the relevant parameters from the copula object
  copRes = .extract.cop.object(copulaobject)
  # derivation of the selected tests with parametric bootstrap. 
  # Switch to Kendall's Tau if Maximum Likelihood estimation fails
  res.f <- .gofHybrid(copula = copRes[[1]], x = x, tests = tests, 
                      customTests = customTests, margins = margins, flip = flip, 
                      dispstr = copRes[[6]], M = M, param = copRes[[2]], 
                      param.est = copRes[[3]], df = copRes[[4]],
                      df.est = copRes[[5]], m = m, 
                      MJ = MJ, delta.J = delta.J, 
                      nodes.Integration = nodes.Integration, lower = lower, 
                      upper = upper, seed.active = seed.active, 
                      processes = processes)
  return(res.f)
}
