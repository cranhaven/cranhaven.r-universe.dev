
# Start halving() functions
###############################################################################
#    Michael Black 03 12 2013
#     Function: Halving PMF
#       calls:  get.tests combines possible number of tests
#               sub.grp.size  gets the subgroup sizes
#                 (could be added to main function)
#       inputs: se and sp sensitivity and specificity,
#               p a vector of individual probabilities
#               stages the number of stages for halving
#               order.p = TRUE if FALSE p.vec is not sorted for halving
###############################################################################

#get.tests is called by halving
get.tests <- function(t1, t2) {
  t1 <- data.matrix(t1)
  t2 <- data.matrix(t2)
  one <- data.matrix(c(rep(x = 1, times = length(t1))))
  Tests <- c(1, t(one %x% t1 + t2 %x% one + one %x% one))
  Tests
}



#sub.grp.size called by halving
sub.grp.size <- function(I.0, stages) {

  if (stages == 2) {
    return(I.0)
  }
  else if (stages == 3) {
    return(c(floor(I.0 / 2), I.0 - floor(I.0 / 2)))
    #return(c(ceiling(I.0 / 2), I.0 - ceiling(I.0 / 2)))
  }
  else {
    return(c(sub.grp.size(I.0 = floor(I.0 / 2), stages = stages - 1),
             sub.grp.size(I.0 = (I.0 - floor(I.0 / 2)),
                          stages = stages - 1)))
    # return(c(sub.grp.size(I.0 = ceiling(I.0 / 2), stages = stages - 1),
    #         sub.grp.size(I.0 = (I.0 - ceiling(I.0 / 2)),
    #                      stages = stages - 1)))
  }

}



# Start halving
###############################################################################

#' @title Probability mass function for halving
#'
#' @description Calculate the probability mass function for the number of tests
#' from using the halving algorithm.
#' @param p a vector of individual risk probabilities.
#' @param Se sensitivity of the diagnostic test.
#' @param Sp specificity of the diagnostic test.
#' @param stages the number of stages for the halving algorithm.
#' @param order.p logical; if TRUE, the vector of individual risk probabilities
#' will be sorted.
#'
#' @details Halving algorithms involve successively splitting a positive
#' testing group into two equal-sized halves (or as close to equal as possible)
#' until all individuals have been identified as positive or negative.
#' \eqn{S}-stage halving begins by testing the whole group of \eqn{I}
#' individuals. Positive groups are split in half until the final stage of the
#' algorithm, which consists of individual testing. For example, consider an
#' initial group of size \eqn{I=16} individuals. Three-stage halving (3H)
#' begins by testing the whole group of 16 individuals. If this group tests
#' positive, the second stage involves splitting into two groups of size 8.
#' If either of these groups test positive, a third stage involves testing each
#' individual rather than halving again. Four-stage halving (4H) would continue
#' with halving into groups of size 4 before individual testing. Five-stage
#' halving (5H) would continue with halving into groups of size 2 before
#' individual testing. 3H requires more than 2 individuals, 4H requires more
#' than 4 individuals, and 5H requires more than 8 individuals.
#'
#' This function calculates the probability mass function, expected testing
#' expenditure, and variance of the testing expenditure for halving algorithms
#' with 3 to 5 stages.
#'
#' @return A list containing:
#' \item{pmf}{the probability mass function for the halving algorithm.}
#' \item{et}{the expected testing expenditure for the halving algorithm.}
#' \item{vt}{the variance of the testing expenditure for the halving
#'           algorithm.}
#' \item{p}{a vector containing the probabilities of positivity for each individual.}
#'
#' @author This function was originally written by Michael Black for Black
#' et al. (2012). The function was obtained from
#' \url{http://chrisbilder.com/grouptesting/}. Minor modifications have been
#' made for inclusion of the function in the \code{binGroup2} package.
#'
#' @references
#' \insertRef{Black2012}{binGroup2}
#'
#' @seealso
#' \code{\link{expectOrderBeta}} for generating a vector of individual risk
#' probabilities for informative group testing.
#'
#' @family operating characteristic functions
#'
#' @examples
#' # Equivalent to Dorfman testing (two-stage hierarchical)
#' halving(p = rep(0.01, 10), Se = 1, Sp = 1, stages = 2,
#'         order.p = TRUE)
#'
#' # Halving over three stages; each individual has a
#' #   different probability of being positive
#' set.seed(12895)
#' p.vec <- expectOrderBeta(p = 0.05, alpha = 2, size = 20)
#' halving(p = p.vec, Se = 0.95, Sp = 0.95, stages = 3,
#'         order.p = TRUE)

# Brianna Hitt - 3 November 2023
#   Added checks to ensure p, Se, Sp are all between 0 and 1

halving <- function(p, Se = 1, Sp = 1, stages = 2, order.p = TRUE) {

  if (Se < 0 | Se > 1) {
    stop("Please provide a sensitivity value between 0 and 1.\n")
  }

  if (Sp < 0 | Sp > 1) {
    stop("Please provide a specificity value between 0 and 1.\n")
  }

  if (any(p < 0) | any(p > 1)) {
    stop("Please provide individual risk probabilities between 0 and 1.\n")
  }

  # Sensitivity and specificity re-defined to match how the
  #   function was originally coded
  se <- Se
  sp <- Sp

  if (order.p == TRUE) p <- sort(p)
  N <- length(p)

  if (stages < 2) {
    stages <- 2
    warning("stages out of range, using Dorfman")
  }

  #these make sure that stages is in bounds for the program
  if (stages > 5) {
    stages = 5
    warning("Too many stages, go back to 5")
  }
  max.stages <- floor(log(N, 2)) + 1
  if (stages > max.stages) {
    stages <- max.stages
    warning("Too many stages using max.stages")
  }

  # This splits it into final sub-groups
  fin.cnt <- sub.grp.size(I.0 = N, stages = stages)
  f <- length(fin.cnt)

  tb <- matrix(c(sp, 1 - sp, 1 - se, se), ncol = 2, byrow = 2)

  if (stages > 2) {

    #the for loop below gets the matrix for the testing error part
    for (i in 2:(stages - 1)) {
      c1 <- c(sp, rep(1 - se, (2^(2^(i - 1)) - 1)))
      ident <- diag(1 - c1)
      tb2 <- (tb %x% tb)
      tb3 <- t(t(tb2) %*% ident)
      tb <- cbind(c1, tb3)

    }
    # below gets the tests matrix for given final sub-groups
    tests.start <- NULL
    for (i in 1:f) {
      tests.start <- rbind(tests.start, c(1, 1 + fin.cnt[i]))
    }

    for (i in 1:(stages - 2)) {
      tests.fin <- NULL
      for (k in 1:(f / (2^i))) {
        tests.fin <- rbind(tests.fin,
                           get.tests(t1 = tests.start[(2*k - 1), ],
                                     t2 = tests.start[(2*k), ]))
      }
      tests.start <- tests.fin
    }
    Tests <- tests.start

  }
  if (stages == 2) {
    Tests <- t(c(1, 1 + N))
  }
  # gets the probability vector
  prob.p <- 1
  p.start <- 0
  for (j in 1:f) {
    p.end <- p.start + fin.cnt[j]
    p.part <- p[(p.start + 1):p.end]
    p.part.prod <- prod(1 - p.part)
    add.prob <- t(c(p.part.prod, 1 - p.part.prod))
    prob.p <- add.prob %x% prob.p
    p.start <- p.end
  }
  prob.ts <- prob.p %*% tb
  test.prop <- rbind(Tests, prob.ts)
  #finalizes the pmf
  pmf <- rbind((by(test.prop[1, ], test.prop[1, ], sum) /
                  by(test.prop[1, ], test.prop[1, ], length)),
               by(test.prop[2, ], test.prop[1, ], sum))
  #get the E(T)
  et <- sum(pmf[1, ] * pmf[2, ])
  #get variance for T
  et2 <- sum(((pmf[1, ])^2) * pmf[2, ])
  vt <- et2 - et^2

  pmf <- data.frame(num.tests = round(pmf[1, ]),
                    prob.tests = round(pmf[2, ], 4), row.names = NULL)

  res <- list(pmf = pmf, et = et, vt = vt, p = p)
  class(res) <- "halving"
  res
}
# End halving() functions




# Print function for halving()
###############################################################################
# Brianna Hitt - 07-06-2021

#' @title Print method for objects of class "halving"
#'
#' @description Print method for objects of class "halving" created
#' by the \code{\link{halving}} function.
#'
#' @param x An object of class "halving" (\code{\link{halving}}).
#' @param ... Additional arguments to be passed to \code{print}.
#' Currently only \code{digits} to be passed to \code{signif} for
#' appropriate rounding.
#'
#' @return A print out of the PMF, expected testing expenditure and variance
#' of testing expenditure resulting from \code{\link{halving}}.
#'
#' @author Brianna D. Hitt

print.halving <- function(x, ...) {

  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  if (length(x) > 3) {
    print.listof(x)

  } else {
    cat("\nPMF:")
    print(as.data.frame(x$pmf))

    cat("\nExpected testing expenditure:", signif(x$et, digits))

    cat("\nVariance of testing expenditure:", signif(x$vt, digits))
  }

}




# PMF function
##################################################################
# pmf.halving() function                                         #
##################################################################

#' @title Extract probability mass function (PMF) from group testing results
#'
#' @description Extract the probability mass function from group testing results
#' for the halving algorithm (objects of class "halving" returned
#' by \code{\link{halving}}).
#'
#' @param object An object of class "halving", created by \code{\link{halving}},
#' from which the PMF is to be extracted.
#' @param ... currently not used.
#'
#' @return Data frame containing the probability mass function
#' extracted from the object \kbd{object}.
#'
#' @author Brianna D. Hitt
#'
#' @examples
#' res <- halving(p = rep(0.01, 10), Sp = 1, Se = 1,
#'                stages = 2, order.p = TRUE)
#' pmf(res)

pmf.halving <- function (object, ...) {
  if (length(object) > 3) {
    object$pmf
  } else{
    object$pmf
  }
}





# ExpTests function
##################################################################
# ExpTests.halving() function                                   #
##################################################################

#' @title Extract the expected number of tests from testing configuration results
#'
#' @description Extract the expected number of tests from objects of class "halving" returned by
#' \code{\link{halving}} (\kbd{halving}).
#'
#' @param object An object of class "halving", from which the expected number
#' of tests is to be extracted.
#' @param ... Additional arguments to be passed to \code{ExpTests} (e.g.,
#' \code{digits} to be passed to \code{round} for appropriate rounding).
#'
#' @return A data frame containing the columns:
#' \item{ExpTests}{the expected number of tests required to decode all individuals
#' in the algorithm.}
#' \item{ExpTestsPerIndividual}{the expected number of tests per individual.}
#' \item{PercentReductionTests}{The percent reduction in the number of tests; 100 * (1 - ExpTestsPerIndividual).}
#' \item{PercentIncreaseTestCap}{The percent increase in testing capacity when the algorithm
#'   is applied to a continuous stream of specimens; 100 * (1/ExpTestsPerIndividual - 1).}
#'
#' @author Christopher R. Bilder

#' @references
#' \insertRef{bilder2020tests}{binGroup2}
#'
#' @examples
#' save.it1 <- halving(p = rep(0.01, 10), Sp = 1, Se = 1, stages = 2,
#'         order.p = TRUE)
#' ExpTests(save.it1)

ExpTests.halving <- function(object, ...) {
  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  ExpTestsPerIndividual <- format(round(object$et/length(object$p), digits),
    nsmall = digits)
  PercentReductionTests <- format(round(100*(1-as.numeric(ExpTestsPerIndividual)), 2),
    nsmall = 2)
  PercentIncreaseTestCap <- format(round(100*(1/as.numeric(ExpTestsPerIndividual) - 1), 2),
    nsmall = 2)

  res <- data.frame(ExpTests = format(round(object$et, digits),
    nsmall = digits),
    ExpTestsPerIndividual = ExpTestsPerIndividual, PercentReductionTests =
    PercentReductionTests, PercentIncreaseTestCap = PercentIncreaseTestCap)
  res
}


