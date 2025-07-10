#' GetHybrid gof test
#' 
#' \code{\link{gofGetHybrid}} computes based on previous test results from this
#' package and on p-values from your own goodness-of-fit tests the hybrid test
#' p-values for the specified testing size.
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
#' @param p_values A vector containing different p-values from your own
#' goodness-of-fit tests. If the elements are unnamed, the test results in the
#' output will be labeled with Test_A, Test_B etc.
#' @param nsets The desired number of tests to be included in each hybrid test.
#' It should be an integer larger than 1 and smaller or equal than the number
#' of tests given in \code{result} and \code{p_values} together. If
#' \code{nsets} is set NULL (default), all possible testing sizes are
#' calculated.
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
#' res_2 = gof(x = IndexReturns2D, copula = "normal", 
#'             tests = c("gofKernel", "gofKendallCvM", "gofWhite"), M = 10)
#' gofGetHybrid(result = res_2, 
#'              p_values = c("MyTest" = 0.3, "AnotherTest" = 0.7), nsets = 3)
#' 
#' @export gofGetHybrid
gofGetHybrid <- function(result, p_values = NULL, nsets = NULL) {
  if (!inherits(result, "gofCOP")) {
stop(
"Please input an object of class 'gofCOP'. Such an object will be returned by 
functions of this package. If you input an object obtained from 'gof()', then 
input the result for all copula."
)
  }
  if (length(nsets) > 1) {
    stop("'nsets' has to be a single integer entry.")
  }

  res_list <- list()
  for (j in seq_along(result)) {
    ## checking setup
    if (length(result[[j]]) == 7) {
      tmp <- rownames(result[[j]]$res.tests)
      index <- which(startsWith(tmp, "hybrid"))
      if (length(index) > 0) {
        res_length <- length(tmp[-index])
      } else {
        res_length <- length(tmp)
      }
    } else {
      res_length <- 1
    }

    num_tests <- length(p_values) + res_length
    if (num_tests <= 1) {
stop(
"The input should contain information of at least two different tests."
)
    }
    if (!is.null(nsets)) {
      if (nsets < 1 | nsets > num_tests) {
stop(
"Please set nsets larger or equal than 1 and smaller or equal than the 
number of single tests. Otherwise hybrid testing is not meaningful."
)
      }
    }

    ## getting combinations
    which_comb <- list()
    for (i in seq_len(2^num_tests)) {
      which_comb[[i]] <- which(as.integer(intToBits(i)) == 1)
    }
    comb_exist <- which_comb[which(unlist(lapply(which_comb, length)) > 1)]

    ## building results
    # names and p-values of single tests
    s_res_names <- rownames(result[[j]]$res.tests)
    s_res_p <- result[[j]]$res.tests[, 1]
    index <- which(startsWith(s_res_names, "hybrid"))
    if (length(index) > 0) {
      s_res_names <- s_res_names[-index]
      s_res_p <- s_res_p[-index]
    }
    s_p_names <- if (!is.null(p_values)) {
      if (is.null(names(p_values))) {
        paste0("Test_", LETTERS[1:length(p_values)])
      } else {
        names(p_values)
      }
    } else {
      NULL
    }
    s_names <- c(s_res_names, s_p_names)
    p <- c(s_res_p, p_values)

    # combinations
    if (!is.null(nsets)) {
      if (nsets > 1) {
        index <- which(lapply(comb_exist, FUN = function(x) {
          length(x) == nsets
        }) == TRUE)
        comb_wanted <- comb_exist[index]
        for (i in seq_along(comb_wanted)) {
          p <- c(p, min(nsets * min(p[comb_wanted[[i]]]), 1))
        }
        comb_names <- paste("hybrid(", 
                            lapply(comb_wanted, paste, collapse = ", "), ")", 
                            sep = "")
        res <- matrix(p, ncol = 1, dimnames = list(c(s_names, comb_names), 
                                                   "p.value"))
      } else {
        res <- matrix(p, ncol = 1, dimnames = list(c(s_names), "p.value"))
      }
    } else {
      for (i in seq_along(comb_exist)) {
        p <- c(p, min(length(comb_exist[[i]]) * min(p[comb_exist[[i]]]), 1))
      }
      comb_names <- paste("hybrid(", 
                          lapply(comb_exist, paste, collapse = ", "), ")", 
                          sep = "")
      res <- matrix(p, ncol = 1, dimnames = list(c(s_names, comb_names), 
                                                 "p.value"))
    }
    res_list[[j]] <- list(
      method = "Hybrid test p-values for given single tests.",
      copula = result[[j]]$copula,
      margins = result[[j]]$margins,
      param.margins = result[[j]]$param.margins,
      theta = result[[j]]$theta,
      df = result[[j]]$df,
      res.tests = res
    )
  }
  names(res_list) <- names(result)

  # output
  return(structure(
    class = "gofCOP",
    res_list
  ))
}
