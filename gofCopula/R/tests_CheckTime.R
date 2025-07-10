#' Combining function for tests
#' 
#' The computation of a gof test can take very long, especially when the number
#' of bootstrap rounds is high. The function \code{\link{gofCheckTime}}
#' computes the time which the estimation most likely takes.
#' 
#' The function estimates the time which the entire gof test will take.
#' 
#' @param copula A character vector which indicates the copula to test for. 
#' Possible are \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"}, \code{"joe"}, \code{"amh"}, \code{"galambos"}, 
#' \code{"huslerReiss"}, \code{"tawn"}, \code{"tev"}, \code{"fgm"} and 
#' \code{"plackett"}.
#' @param x A matrix containing the data with rows being observations and
#' columns being variables.
#' @param tests A character vector which indicates the test to use.
#' @param customTests A character vector which indicates the customized test to
#' use, if any.
#' @param print.res Logical which defines if the resulting time shall be
#' printed or given as value. Default is TRUE.
#' @param param The copulae parameters to use for each test, if it shall not be
#' estimated.
#' @param param.est Shall be either \code{TRUE} or \code{FALSE}. \code{TRUE}
#' means that \code{param} will be estimated.
#' @param df The degrees of freedom, if not meant to be estimated. Only
#' necessary if tested for \code{"t"}-copula.  For the \code{"gofPIOSTn"} test
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
#' @param flip The control parameter to flip the copula by 90, 180, 270 degrees
#' clockwise. Only applicable for bivariate copula. Default is 0 and possible 
#' inputs are 0, 90, 180, 270 and NULL.
#' @param M The number of bootstrapping rounds which shall be later taken in
#' the estimation.
#' @param MJ Just for the test gofKernel. Size of bootstrapping sample.
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
#' @examples
#' 
#' \dontrun{
#' data(IndexReturns2D)
#' 
#' gofCheckTime("normal", IndexReturns2D, "gofKendallKS", M = 10000)
#' }
#' 
#' @export gofCheckTime
gofCheckTime <- function(copula, x, tests = NULL, customTests = NULL, 
                         param = 0.5, param.est = TRUE, df = 4, df.est = TRUE, 
                         margins = "ranks", flip = 0, M = 1000, MJ = 100, 
                         dispstr = "ex", print.res = TRUE, m = 1, delta.J = 0.5, 
                         nodes.Integration = 12, lower = NULL, upper = NULL, 
                         seed.active = NULL, processes = 1) {
  if (is.matrix(x) == FALSE) {
    stop("x must be a matrix")
  }
  if (is.null(tests) & is.null(customTests)) {
    stop("Please provide either 'tests' or 'customTests'.")
  }
  if (!is.null(tests) & any(is.element(tests, c("gofRn")))) {
stop(
"The test gofRn was removed due to inconsistencies with the remaining tests."
)
  }
  if (any(!is.element(tests, gofTest4Copula(copula, dim(x)[2]))) || 
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
  if (any(!is.element(margins, c("ranks", "beta", "cauchy", "chisq", "f", 
                                 "gamma", "lnorm", "norm", "t", "weibull", 
                                 "exp")))) {
stop(paste(
"At least one of the distributions in `margins' is not implemented. Please 
amend and run the function again. It has to be either of `ranks', `beta', 
`cauchy', `chisq', `f', `gamma', `lnorm', `norm', `t', `weibull', `exp'."
))
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
  if (!inherits(df.est, "logical")) {
    stop("The argument 'df.est' has to be either 'TRUE' or 'FALSE'.")
  }
  if (!is.null(lower) & !is.numeric(lower) | !is.null(upper) & 
      !is.numeric(upper)) {
    stop("The arguments 'upper' and 'lower' have to be either NULL 
         or numeric.")
  }
  if (!is.null(seed.active) & length(seed.active) != 1 & 
      length(seed.active) != (M + 1)) {
    stop("The seed has to be an integer or a vector of M+1 seeds.")
  }
  if (!is.null(seed.active) & all(!vapply(seed.active, 
                                          function(x) x %% 1 == 0, TRUE))) {
stop(
"All seeds have to be whole numbers. Please check seed.active for non-whole 
numbers."
)
  }
  if (!is.null(seed.active) & length(seed.active) == 1) {
    set.seed(seed.active)
    RNGsetting <- RNGkind()
    RNGkind(sample.kind = "default")
    on.exit(RNGkind(sample.kind = RNGsetting[3]))
    seed.active <- sample(x = 2147483647, size = M + 1)
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
"At least one of the distributions in `margins' is not implemented. Please 
amend and run the function again. It has to be either of `ranks', `beta', 
`cauchy', `chisq', `f', `gamma', `lnorm', `norm', `t', `weibull', `exp'."
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

  cop = copula
  # estimation of computational time by deriving test runs
  cat("An estimate of the computational time is under derivation.", fill = TRUE)
cat(
"Depending on the tests chosen, dimensionality and complexity of the data, this 
might take a while.", 
fill = TRUE)
  lasted.time <- c()
  N <- c(2, 5, 10, 15)
  NJ <- c(2, 5, 10, 15)
  if (!is.null(tests)) {
    for (j in seq_along(tests)) {
      times.comp <- c()
      if (tests[j] == "gofKernel") {
        for (i in N) {
          for (ii in NJ) {
            # Derivation of the selected tests with parametric bootstrap. 
            # Switch to Kendall's Tau if Maximum Likelihood estimation fails
            times.comp <- rbind(times.comp, system.time(
              invisible(capture.output(suppressWarnings(
                {
                  a <- .param.est(copula = cop, x = x, 
                                  param = param, param.est = param.est, 
                                  df = df, df.est = df.est, dispstr = dispstr, 
                                  lower = lower, upper = upper, 
                                  flip = flip) 
                  copRes = .extract.cop.object(a[[1]])
                  .gofHybrid(copula = copRes[[1]], x = x, 
                             dispstr = copRes[[6]], 
                             tests = tests[j], M = i, MJ = ii, 
                             margins = margins, param = copRes[[2]], 
                             param.est = copRes[[3]], df = copRes[[4]], 
                             df.est = copRes[[5]], delta.J = delta.J, 
                             nodes.Integration = nodes.Integration, m = m, 
                             lower = lower, upper = upper, 
                             seed.active = seed.active[seq_len(i + 1)], 
                             processes = processes)
                }
                ))))[3])
          }
        }
        times.comp <- cbind(sort(rep(N, length(N))), 
                            rep(NJ, length(NJ)), times.comp)
        times.lm <- lm(times.comp[, 3] ~ times.comp[, 1] + times.comp[, 2])
        lasted.time[j] <- round(times.lm$coefficients[1] + 
                                  times.lm$coefficients[2] * M + 
                                  times.lm$coefficients[3] * MJ)


        if (lasted.time[j] < 0) {
          cat(paste0("Derivation time could not be computed for ", 
                     tests[j]), fill = TRUE)
          lasted.time[j] <- NA
        }
      } else {
        for (i in N) {
          # Derivation of the selected tests with parametric bootstrap. 
          # Switch to Kendall's Tau if Maximum Likelihood estimation fails
          times.comp <- rbind(times.comp, system.time(
            invisible(capture.output(suppressWarnings(
              {
                a <- .param.est(copula = cop, x = x, 
                                param = param, param.est = param.est, 
                                df = df, df.est = df.est, dispstr = dispstr, 
                                lower = lower, upper = upper, 
                                flip = flip) 
                copRes = .extract.cop.object(a[[1]])
                .gofHybrid(copula = copRes[[1]], x = x, 
                           dispstr = copRes[[6]], 
                           tests = tests[j], M = i, MJ = MJ, 
                           margins = margins, param = copRes[[2]], 
                           param.est = copRes[[3]], df = copRes[[4]], 
                           df.est = copRes[[5]], delta.J = delta.J, 
                           nodes.Integration = nodes.Integration, m = m, 
                           lower = lower, upper = upper, 
                           seed.active = seed.active[seq_len(i + 1)], 
                           processes = processes)
              }
              ))))[3])
        }
        times.comp <- cbind(N, times.comp)
        times.lm <- lm(times.comp[, 2] ~ times.comp[, 1])
        lasted.time[j] <- round(times.lm$coefficients[1] + 
                                  times.lm$coefficients[2] * M)

        if (lasted.time[j] < 0) {
          cat(paste0("Derivation time could not be computed for ", 
                     tests[j]), fill = TRUE)
          lasted.time[j] <- NA
        }
      }
    }
  }

  # Derivation of the custom tests with parametric bootstrap. 
  # Switch to Kendall's Tau if Maximum Likelihood estimation fails
  lasted.time2 <- c()
  if (!is.null(customTests)) {
    for (j in seq_along(customTests)) {
      times.comp <- c()
      for (i in N) {
        times.comp <- rbind(times.comp, system.time(
          invisible(capture.output(suppressWarnings(
            {
              a <- .param.est(copula = cop, x = x, 
                              param = param, param.est = param.est, 
                              df = df, df.est = df.est, dispstr = dispstr, 
                              lower = lower, upper = upper, 
                              flip = flip) 
              copRes = .extract.cop.object(a[[1]])
              .gofHybrid(copula = copRes[[1]], x = x, 
                         dispstr = copRes[[6]], 
                         tests = NULL, customTests = customTests[j], 
                         M = i, MJ = MJ, 
                         margins = margins, param = copRes[[2]], 
                         param.est = copRes[[3]], df = copRes[[4]], 
                         df.est = copRes[[5]], delta.J = delta.J, 
                         nodes.Integration = nodes.Integration, m = m, 
                         lower = lower, upper = upper, 
                         seed.active = seed.active[seq_len(i + 1)], 
                         processes = processes)
            }
            ))))[3])
      }
      times.comp <- cbind(N, times.comp)
      times.lm <- lm(times.comp[, 2] ~ times.comp[, 1])
      lasted.time2[j] <- round(times.lm$coefficients[1] + 
                                 times.lm$coefficients[2] * M)

      if (lasted.time2[j] < 0) {
        cat(paste0("Derivation time could not be computed for ", 
                   customTests[j]), fill = TRUE)
        lasted.time2[j] <- NA
      }
    }
  }
  if (print.res == TRUE) {
    .get.time(sum(lasted.time, lasted.time2))
  } else {
    store.time <- sum(lasted.time, lasted.time2)
    return(store.time)
  }
}
