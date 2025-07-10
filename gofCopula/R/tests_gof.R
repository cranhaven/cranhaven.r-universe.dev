#' Combining function for tests
#' 
#' \code{\link{gof}} computes for a given dataset and based on the choices of
#' the user different tests for different copulae. If copulae are given, all
#' the implemented tests for those copulae are calculated. If tests are given,
#' all the implemented copulae for every test are used. If both copulae and
#' tests are given, all possible combinations are calculated.
#' 
#' If a character vector is given for the argument \code{copula} and nothing
#' for \code{tests}, then all tests are performed for which the given copulae
#' are implemented. If \code{tests} contains a character vector of tests and
#' \code{copula = NULL}, then this tests will be performed for all implemented
#' copulae. If character vectors are given for \code{copula} and \code{tests},
#' then the tests are performed with the given copulae. If \code{tests = NULL}
#' and \code{copula = NULL}, then the argument \code{priority} catches in and
#' defines the procedure.
#' 
#' For small values of \code{M}, initializing the parallelisation via
#' \code{processes} does not make sense. The registration of the parallel
#' processes increases the computation time. Please consider to enable
#' parallelisation just for high values of \code{M}.
#' 
#' Note that this function does not display \code{warning()} messages. Due to
#' the large amount of tests run at once, the messages are not tracable to the
#' situation when they appeared. Hence they are omitted for this function.
#' 
#' @param x A matrix containing the data with rows being observations and
#' columns being variables.
#' @param priority A character string which is either \code{"tests"} or
#' \code{"copula"}. \code{"tests"} indicates that all implemented tests are
#' performed for all copulae which the tests share. These are e.g.
#' \code{"normal"} and \code{"clayton"}. If \code{"copula"} is chosen, 
#' all copula are chosen and only the tests are performend which these copula 
#' share. If one of the arguments \code{tests} or \code{copula} is not 
#' \code{NULL}, then \code{priority} doesn't affect the choice of the copulae 
#' and tests.
#' @param copula A character vector which indicates the copula to test for.
#' Possible are \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"}, \code{"joe"}, \code{"amh"}, \code{"galambos"}, 
#' \code{"huslerReiss"}, \code{"tawn"}, \code{"tev"}, \code{"fgm"} and 
#' \code{"plackett"}.
#' @param tests A character vector which indicates the tests to use. Possible
#' choices are the individual tests implemented in this package.
#' @param customTests A character vector which indicates the customized test to
#' use, if any. The test has to be loaded into the workspace. Currently the
#' function containing the test has to have 2 arguments, the first one for the
#' dataset and the second one for the copula to test for. The arguments have to
#' be named "x" and "copula" respectively.
#' @param param The copulae parameters to use for each test, if it shall not be
#' estimated.
#' @param param.est Shall be either \code{TRUE} or \code{FALSE}. \code{TRUE}
#' means that \code{param} will be estimated.
#' @param df The degrees of freedom, if not meant to be estimated. Only
#' necessary if tested for \code{"t"}-copula. For the \code{"gofPIOSTn"} test
#' the entry is limited to 60 degrees of freedom for computational reasons.
#' @param df.est Indicates if \code{df} shall be estimated. Has to be either
#' \code{FALSE} or \code{TRUE}, where \code{TRUE} means that it will be
#' estimated. For the \code{"gofPIOSTn"} test the estimate is limited to 60
#' degrees of freedom for computational reasons.
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
#' @param flip The vector of control parameters to flip the copula 
#' by 90, 180, 270 degrees clockwise. Only applicable for bivariate copula. 
#' Default is 0 and possible inputs are 0, 90, 180, 270 and NULL. One can either
#' specify one flip degree which will be applied on all copulae or choose
#' an individual flip for each copula in which case the input has to be a vector.
#' @param M The amount of bootstrap rounds to be performed by each test.
#' Default is 1000.
#' @param MJ Just for the test gofKernel. Size of bootstrapping sample.
#' @param dispstr A character string specifying the type of the symmetric
#' positive definite matrix characterizing the elliptical copula. Implemented
#' structures are "ex" for exchangeable and "un" for unstructured, see package
#' \code{copula}.
#' @param m Length of blocks. Only necessary if the test \code{gofPIOSTn} is
#' part of \code{tests}.
#' @param delta.J Scaling parameter for the matrix of smoothing parameters.
#' Only necessary if the test \code{gofKernel} is part of \code{tests}.
#' @param nodes.Integration Number of knots of the bivariate Gauss-Legendre
#' quadrature. Only necessary if the test \code{gofKernel} is part of
#' \code{tests}.
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
#' @return A list containing several objects of \code{class} gofCOP with the
#' following components for each copulae \item{method}{a character which
#' informs about the performed analysis} \item{copula}{the copula tested for}
#' \item{margins}{the method used to estimate the margin distribution.}
#' \item{param.margins}{the parameters of the estimated margin distributions.
#' Only applicable if the margins were not specified as \code{"ranks"} or
#' \code{NULL}.} \item{theta}{dependence parameters of the copulae}
#' \item{df}{the degrees of freedem of the copula. Only applicable for
#' t-copula.} \item{res.tests}{a matrix with the p-values and test statistics
#' of the hybrid and the individual tests}
#' @examples
#' 
#' data(IndexReturns2D)
#' 
#' gof(IndexReturns2D, priority = "tests", copula = "normal", 
#' tests = c("gofRosenblattSnB", "gofRosenblattSnC"), M = 5)
#' 
#' @export gof
gof <- function(x, priority = "copula", copula = NULL, tests = NULL, 
                customTests = NULL, param = 0.5, param.est = TRUE, df = 4, 
                df.est = TRUE, margins = "ranks", flip = 0, M = 1000, MJ = 100, 
                dispstr = "ex", m = 1, delta.J = 0.5, nodes.Integration = 12, 
                lower = NULL, upper = NULL, seed.active = NULL, processes = 1) {
  if (is.matrix(x) == FALSE) {
    stop("x must be a matrix")
  }
  if (any(lapply(copula, function(x) {
    is.element(x, c("normal", "gaussian", "t", "clayton", "gumbel", "frank", 
                    "joe", "amh", "galambos", "huslerReiss", "tawn", "tev", 
                    "fgm", "plackett"))
  }) == FALSE) == TRUE) {
    stop("At least one of the copulae is not implemented")
  }
  if (any(sapply(copula, function(y) {
    is.element(y, c("amh", "galambos", "huslerReiss", "tawn", "tev", "fgm", 
                    "plackett")) & dim(x)[2] > 2
  })) == TRUE) {
stop(
"The copulae amh, galambos, huslerReiss, tawn, tev, fgm, plackett are not 
implemented for dimensions larger 2."
)
  }
  
  if (!is.null(copula) & !is.null(tests)) {
    test_which = lapply(copula, gofTest4Copula, d = dim(x)[2])
    copula_which = sapply(test_which, function(x) all(!is.element(tests, x)))
    if (any(copula_which)) {
cat(paste0(paste(copula[copula_which], collapse = " "), ":"))
cat("\n")
cat(
"This/these copula is/are not implemented for this dimensionality for any of 
the selected tests. It is omitted from the analysis."
)
cat("\n")
cat("\n")
      copula = copula[!copula_which]
    }
    test_which = lapply(copula, gofTest4Copula, d = dim(x)[2])
    test_which2 = sapply(tests, 
                          function(x) {
                            length(which(unlist(test_which) == x)) == 
                              length(test_which)})
    if (any(!test_which2)) {
cat(paste0(paste(tests[!test_which2], collapse = " "), ":"))
cat("\n")
cat(
"This/these test is/are not implemented for this dimensionality for all copula. 
It is omitted from the analysis."
)
cat("\n")
cat("\n")
      tests = tests[test_which2]
    }
    if (length(tests) == 0 | length(copula) == 0) {
stop(
"The selected copula are not implemented for any of the selected tests for this 
dimensionality. Please check CopulaTestTable() or consider setting priority 
to `tests' or `copula'."
)
    }
  }
  
  if (is.null(copula)) {
    tests.available <- gofTest4Copula(copula, dim(x)[2])
  } else {
    tests.available <- Reduce(intersect, lapply(copula, 
                                                function(y) 
                                                  gofTest4Copula(y, dim(x)[2])))
  }
  if (!is.null(lower) & !is.numeric(lower) | !is.null(upper) & 
      !is.numeric(upper)) {
stop(
"The arguments 'upper' and 'lower' have to be either NULL or numeric."
)
  }
  if (!is.null(lower) | !is.null(upper)) {
    if (is.null(copula)) {
stop(
"Each copula has a different parameter space. When controlling the estimation 
via 'upper' and 'lower' bound, the copula to test for have be specified via the 
'copula' argument."
)
    } else if (length(copula) != length(lower) & !is.null(lower) | 
               length(copula) != length(upper)  & !is.null(upper)) {
stop(
"Each copula has a different parameter space. Provide a vector with lower and 
upper bounds for each copula."
)
    }
  }
  if (!is.null(tests) & any(!is.element(tests, tests.available)) || 
      !is.null(customTests) & any(!is.element(customTests, ls(".GlobalEnv")))) {
stop(
"At least one of the tests in 'tests' is not implemented, cannot handle a 
dataset of this dimension or at least one of the tests in 'customTests' does not 
match any function in the global workspace. Please check if it is correctly 
spelled in the function call."
)
  }
  if (is.element(priority, c("tests", "copula")) == FALSE) {
stop(
"Please insert a valid character string for the argument priority. It shall be 
either 'tests' or 'copula'."
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
  if ((is.null(copula) || is.element("gumbel", copula)) & 
      param.est == FALSE & param <= 1) {
warning(
"When copula is 'gumbel', 'param' has to be larger 1. Because 'param.est' was 
set to 'FALSE', 'param' will be set to 1.5 as default value for 'gumbel' copula 
in the individual tests."
)
  }
  if (!inherits(df.est, "logical")) {
    stop("The argument 'df.est' has to be either 'TRUE' or 'FALSE'.")
  }
  if (!is.null(copula) & !is.null(flip) & length(flip) > 1 & 
      length(flip) != length(copula)) {
    stop("The length of 'copula' and 'flip' are not equal.")
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
  if (any(!vapply(customTests, 
                  function(x) all(names(formals(x)) %in% c("x", "copula")), 
                  TRUE))) {
stop(
"At least one function in 'customTest' does not follow the requirements for the 
arguments. The first argument for the dataset has to be called 'x', the second 
one for the copula has to be called 'copula'."
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

  # Derivation of the selected tests with parametric bootstrap. 
  # Switch to Kendall's Tau if Maximum Likelihood estimation fails
  # Differentiation according to the choice of the function arguments 
  # priority, copula und tests
  if (!is.null(tests)) {
    if (!is.null(copula)) {
      if (is.null(lower)) {lower = -Inf}
      if (is.null(upper)) {upper = Inf}
      res <- mapply(function(cop, flip1, lower1, upper1) {
        cat(paste0(cop, if(flip1 == 0) {NULL} else {flip1}, " copula\n"))
        
        if (is.infinite(lower1)) {lower1 = NULL}
        if (is.infinite(upper1)) {upper1 = NULL}
        
        a <- tryCatch(.param.est(copula = cop, x = x, 
                                    param = param, param.est = param.est, 
                                    df = df, df.est = df.est, dispstr = dispstr, 
                                    lower = lower1, upper = upper1, 
                                 flip = flip1), 
                      error = function(e) warning(e))
        if (length(a) != 1) {
          copRes = .extract.cop.object(a[[1]])
          
          tmp <- suppressWarnings(.gofHybrid(copula = copRes[[1]], x = x, 
                                             tests = tests, 
                                             customTests = customTests, 
                                             margins = margins, 
                                             param.margins = param.margins, 
                                             flip = flip1, 
                                             dispstr = copRes[[6]], M = M, 
                                             param = copRes[[2]], 
                                             param.est = copRes[[3]], 
                                             df = copRes[[4]],
                                             df.est = a[[4]], m = m, 
                                             MJ = MJ, delta.J = delta.J, 
                                             nodes.Integration = nodes.Integration, 
                                             lower = lower1, 
                                             upper = upper1, 
                                             seed.active = seed.active, 
                                             processes = processes))
          cat("\n")
          return(tmp)
        } else {
cat(paste(
"The copula ", cop, " is excluded from the analysis since the parameters do not 
fit its parameter space. See warnings and manual for more details.", sep = ""), 
fill = TRUE)
          cat("\n")
          tmp = list(NULL)
          names(tmp) <- paste0(cop, flip1)
        }
      }, copula, flip, lower, upper, USE.NAMES = FALSE)
      # Removing of copula entries which were not conducted
      res = res[lengths(res) > 1]
      # Assigning of gofCOP structure
      return(structure(
        class = "gofCOP",
        res
      ))
    } else {
      copula_list <- lapply(tests, gofCopula4Test)
      copula <- Reduce(intersect, copula_list)
      copula_which = lapply(copula, gofTest4Copula, d = dim(x)[2])
      copula_which2 = sapply(copula_which, 
                             function(x) all(is.element(tests, x)))
      copula = copula[copula_which2]
      if (length(flip) > 1) {copula <- rep(copula,times = 1, 
                                           each = length(flip))}
      res <- mapply(function(cop, flip1) {
        cat(paste0(cop, if(flip1 == 0) {NULL} else {flip1}, " copula\n"))
        a <- tryCatch(.param.est(copula = cop, x = x, 
                                 param = param, param.est = param.est, 
                                 df = df, df.est = df.est, dispstr = dispstr, 
                                 lower = lower, upper = upper, flip = flip1), 
                      error = function(e) warning(e))
        if (length(a) != 1) {
          copRes = .extract.cop.object(a[[1]])
          
          tmp <- suppressWarnings(.gofHybrid(copula = copRes[[1]], x = x, 
                                             tests = tests, 
                                             customTests = customTests, 
                                             margins = margins, 
                                             param.margins = param.margins, 
                                             flip = flip1, 
                                             dispstr = copRes[[6]], M = M, 
                                             param = copRes[[2]], 
                                             param.est = copRes[[3]], 
                                             df = copRes[[4]],
                                             df.est = a[[4]], m = m, 
                                             MJ = MJ, delta.J = delta.J, 
                                             nodes.Integration = nodes.Integration, 
                                             lower = lower, 
                                             upper = upper, 
                                             seed.active = seed.active, 
                                             processes = processes))
          cat("\n")
          return(tmp)
        } else {
cat(paste(
"The copula ", cop, " is excluded from the analysis since the parameters do not 
fit its parameter space. See warnings and manual for more details.", sep = ""), 
fill = TRUE)
          cat("\n")
          tmp = list(NULL)
          names(tmp) <- paste0(cop, flip1)
        }
      }, copula, flip, USE.NAMES = FALSE)
      # Removing of copula entries which were not conducted
      res = res[lengths(res) > 1]
      # Assigning of gofCOP structure
      return(structure(
        class = "gofCOP",
        res
      ))
    }
  } else {
    if (!is.null(copula)) {
      tests_list <- lapply(copula, gofTest4Copula, d = dim(x)[2])
      tests <- Reduce(intersect, tests_list)
      tests <- tests[-which(tests == "gofCustomTest")]
      if (is.null(lower)) {lower = -Inf}
      if (is.null(upper)) {upper = Inf}
      res <- mapply(function(cop, flip1, lower1, upper1) {
        cat(paste0(cop, if(flip1 == 0) {NULL} else {flip1}, " copula\n"))
        
        if (is.infinite(lower1)) {lower1 = NULL}
        if (is.infinite(upper1)) {upper1 = NULL}
        
        a <- tryCatch(.param.est(copula = cop, x = x, 
                                 param = param, param.est = param.est, 
                                 df = df, df.est = df.est, dispstr = dispstr, 
                                 lower = lower1, upper = upper1, flip = flip1), 
                      error = function(e) warning(e))
        if (length(a) != 1) {
          copRes = .extract.cop.object(a[[1]])
          
          tmp <- suppressWarnings(.gofHybrid(copula = copRes[[1]], x = x, 
                                             tests = tests, 
                                             customTests = customTests, 
                                             margins = margins, 
                                             param.margins = param.margins, 
                                             flip = flip1, 
                                             dispstr = copRes[[6]], M = M, 
                                             param = copRes[[2]], 
                                             param.est = copRes[[3]], 
                                             df = copRes[[4]],
                                             df.est = a[[4]], m = m, 
                                             MJ = MJ, delta.J = delta.J, 
                                             nodes.Integration = nodes.Integration, 
                                             lower = lower1, 
                                             upper = upper1, 
                                             seed.active = seed.active, 
                                             processes = processes))
          cat("\n")
          return(tmp)
        } else {
cat(paste(
"The copula ", cop, " is excluded from the analysis since the parameters do not 
fit its parameter space. See warnings and manual for more details.", sep = ""), 
fill = TRUE)
          cat("\n")
          tmp = list(NULL)
          names(tmp) <- paste0(cop, flip1)
        }
      }, copula, flip, lower, upper, USE.NAMES = FALSE)
      # Removing of copula entries which were not conducted
      res = res[lengths(res) > 1]
      # Assigning of gofCOP structure
      return(structure(
        class = "gofCOP",
        res
      ))
    } else if (priority == "tests") {
      # Takes all tests and uses only the copula they share
      tests <- lapply(ls(pos = "package:gofCopula"), function(x) {
        a = try(eval(formals(x)$copula), silent = TRUE)
        if (!is.null(a) & !inherits(class(a), "try-error")) {
          x
        }
      })
      tests = unlist(tests)
      tests <- tests[-which(tests == "gofCustomTest")]
      cops = lapply(tests, gofCopula4Test)
      copula <- Reduce(intersect, cops)
      tests2 = lapply(copula, gofTest4Copula, d = dim(x)[2])
      tests2 <- Reduce(intersect, tests2)
      tests = intersect(tests, tests2)
      if (length(flip) > 1) {copula <- rep(copula,times = 1, 
                                           each = length(flip))}
      res <- mapply(function(cop, flip1) {
        cat(paste0(cop, if(flip1 == 0) {NULL} else {flip1}, " copula\n"))
        a <- tryCatch(.param.est(copula = cop, x = x, 
                                 param = param, param.est = param.est, 
                                 df = df, df.est = df.est, dispstr = dispstr, 
                                 lower = lower, upper = upper, flip = flip1), 
                      error = function(e) warning(e))
        if (length(a) != 1) {
          copRes = .extract.cop.object(a[[1]])
          
          tmp <- suppressWarnings(.gofHybrid(copula = copRes[[1]], x = x, 
                                             tests = tests, 
                                             customTests = customTests, 
                                             margins = margins, 
                                             param.margins = param.margins, 
                                             flip = flip1, 
                                             dispstr = copRes[[6]], M = M, 
                                             param = copRes[[2]], 
                                             param.est = copRes[[3]], 
                                             df = copRes[[4]],
                                             df.est = a[[4]], m = m, 
                                             MJ = MJ, delta.J = delta.J, 
                                             nodes.Integration = nodes.Integration, 
                                             lower = lower, 
                                             upper = upper, 
                                             seed.active = seed.active, 
                                             processes = processes))
          cat("\n")
          return(tmp)
        } else {
cat(paste(
"The copula ", cop, " is excluded from the analysis since the parameters do not 
fit its parameter space. See warnings and manual for more details.", sep = ""), 
fill = TRUE)
          cat("\n")
          tmp = list(NULL)
          names(tmp) <- paste0(cop, flip1)
        }
      }, copula, flip, USE.NAMES = FALSE)
      # Removing of copula entries which were not conducted
      res = res[lengths(res) > 1]
      # Assigning of gofCOP structure
      return(structure(
        class = "gofCOP",
        res
      ))
    } else if (priority == "copula") {
      # Takes all copula and uses only the tests they share
      copula <- eval(formals(gofCvM)$copula) 
      tests_list <- lapply(copula, gofTest4Copula, d = dim(x)[2])
      copulas.which <- sapply(tests_list, 
                              function(x) is.element("gofCustomTest", x))
      tests <- Reduce(intersect, tests_list[copulas.which])
      tests <- tests[-which(tests == "gofCustomTest")]
      copula = copula[copulas.which]
      if (length(flip) > 1) {copula <- rep(copula,times = 1, 
                                           each = length(flip))}
      res <- mapply(function(cop, flip1) {
        cat(paste0(cop, if(flip1 == 0) {NULL} else {flip1}, " copula\n"))
        a <- tryCatch(.param.est(copula = cop, x = x, 
                                 param = param, param.est = param.est, 
                                 df = df, df.est = df.est, dispstr = dispstr, 
                                 lower = lower, upper = upper, flip = flip1), 
                      error = function(e) warning(e))
        if (length(a) != 1) {
          copRes = .extract.cop.object(a[[1]])
          
          tmp <- suppressWarnings(.gofHybrid(copula = copRes[[1]], x = x, 
                                             tests = tests, 
                                             customTests = customTests, 
                                             margins = margins, 
                                             param.margins = param.margins, 
                                             flip = flip1, 
                                             dispstr = copRes[[6]], M = M, 
                                             param = copRes[[2]], 
                                             param.est = copRes[[3]], 
                                             df = copRes[[4]],
                                             df.est = a[[4]], m = m, 
                                             MJ = MJ, delta.J = delta.J, 
                                             nodes.Integration = nodes.Integration, 
                                             lower = lower, 
                                             upper = upper, 
                                             seed.active = seed.active, 
                                             processes = processes))
          cat("\n")
          return(tmp)
        } else {
cat(paste(
"The copula ", cop, " is excluded from the analysis since the parameters do not 
fit its parameter space. See warnings and manual for more details.", sep = ""), 
fill = TRUE)
          cat("\n")
          tmp = list(NULL)
          names(tmp) <- paste0(cop, flip1)
        }
      }, copula, flip, USE.NAMES = FALSE)
      # Removing of copula entries which were not conducted
      res = res[lengths(res) > 1]
      # Assigning of gofCOP structure
      return(structure(
        class = "gofCOP",
        res
      ))
    }
  }
}
