#' Output Hybrid gof test
#' 
#' \code{\link{gofOutputHybrid}} outputs the desired Hybrid tests from previous
#' test results from this package for the specified testing size.
#' 
#' In most of scenarios for goodness-of-fit tests, including the one for copula
#' models (e.g. Genest et al. (2009)) there exists no single dominant optimal
#' test. Zhang et al. (2015) proposed a hybrid test which performed in their
#' simulation study more desirably compared to the applied single tests.
#' 
#' The p-value is a combination of the single tests in the following way:
#' \deqn{p_n^{hybrid} = \min(q \cdot \min{(p_n^{(1)}, \dots, p_n^{(q)})},
#' 1)}{pn^(hybrid) = min(q x min(pn^(1), ..., pn^(q)), 1)} where \eqn{q}{q} is
#' the number of tests and \eqn{p_n^{(i)}}{pn^(i)} the p-value of the test
#' \eqn{i}. It is ensured that the hybrid test is consistent as long as at
#' least one of the tests is consistent.
#' 
#' The computation of the individual p-values is performed as described in the
#' details of this tests. Note that the derivation differs.
#' 
#' @param result An object of \code{class} gofCOP.
#' @param tests Individual tests which should be used in the hybrid test.
#' Submit a vector containing the position of the individual tests as they
#' appear in the object submitted, e.g. \code{c(1,4)} for the 1st and 4th
#' tests. If \code{tests} is set NULL (default), all possible testing sizes are
#' returned.
#' @param nsets The desired number of tests to be included in each hybrid test.
#' It should be an integer larger than 1 and smaller or equal than the number
#' of tests given in \code{result}. If \code{nsets} is set NULL (default), all
#' possible testing sizes are calculated.
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
#' \doi{10.1016/j.jeconom.2016.02.017} \cr \cr
#' @examples
#' 
#' data(IndexReturns2D)
#' 
#' res1 = gof(IndexReturns2D, priority = "tests", copula = "normal", 
#'            tests = c("gofKendallCvM", "gofRosenblattSnC", "gofKendallKS"), 
#'            M = 5)
#' gofOutputHybrid(res1, tests = 1, nsets = 2)
#' # mind the difference to the regular output
#' res1
#' 
#' @export gofOutputHybrid
gofOutputHybrid <- function(result, tests = NULL, nsets = NULL) {
  if (!inherits(result, "gofCOP")) {
stop(
"Please input an object of class 'gofCOP' generated from the function 'gof()', 
which is implemented in this package."
)
  }
  if (!any(grepl("hybrid", rownames(result[[1]]$res.tests)))) {
    stop("Please input an object containing hybrid test results.")
  }
  numb_tests <- sum(!grepl("hybrid", rownames(result[[1]]$res.tests)))
  if (isTRUE(nsets > numb_tests)) {
stop(
"The number of tests to obtain the hybrid test for cannot be larger than the 
number of tests available."
)
  }
  if (!all(is.element(tests, 1:numb_tests))) {
    stop("At least one of the 'tests' is not stored in the object.")
  }

  res_list <- list()
  for (j in seq_along(result)) {
    which_comb <- list()
    for (i in seq_len(2^numb_tests)) {
      which_comb[[i]] <- which(as.integer(intToBits(i)) == 1)
    }
    comb_exist <- which_comb[which(unlist(lapply(which_comb, length)) > 1)]

    which_tests <- rep(TRUE, length(comb_exist))
    if (!is.null(tests)) {
      which_tests <- sapply(comb_exist, function(x) any(is.element(x, tests)))
    }
    which_nsets <- rep(TRUE, length(comb_exist))
    if (!is.null(nsets)) {
      which_nsets <- sapply(comb_exist, function(x) (length(x) == nsets))
    }
    which_tests_nsets <- which_nsets & which_tests

    hybrid.results <- result[[j]]$res.tests[-c(1:numb_tests), ]
    new.res.tests <- result[[j]]$res.tests[1:numb_tests, ]
    names_new.res.tests <- rownames(new.res.tests)
    if (length(which_tests_nsets) == 1) {
      if (which_tests_nsets == TRUE) {
        new.res.tests <- rbind(new.res.tests, hybrid.results)
        rownames(new.res.tests) <- c(names_new.res.tests, 
                        rownames(result[[j]]$res.tests)[-c(1:numb_tests)])
      }
    } else {
      new.res.tests <- rbind(new.res.tests, hybrid.results[which_tests_nsets, ])
      rownames(new.res.tests) <- c(names_new.res.tests, 
                                   rownames(hybrid.results)[which_tests_nsets])
    }
    res_list[[j]] <- list(
      method = result[[j]]$method,
      copula = result[[j]]$copula,
      margins = result[[j]]$margins,
      param.margins = result[[j]]$param.margins,
      theta = result[[j]]$theta,
      df = result[[j]]$df,
      res.tests = new.res.tests
    )
  }
  names(res_list) <- names(result)

  return(structure(
    class = "gofCOP",
    res_list
  ))
}
