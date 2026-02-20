#####################################################################
# NAME:  Brianna Hitt
# DATE:  6-29-2021
# Purpose: Implements the Thresholded Optimal Dorfman algorithm -
#          Calculates summary measures (expected tests, variance,
#          individual and pooling accuracy measures)
#
# Notes: The opt.info.dorf() function is from Chris McMahan's programs,
#        provided with "Informative Dorfman screening" by McMahan, Tebbs,
#        and Bilder (2012)
#
#####################################################################

#' @title Summary measures for the Thresholded Optimal Dorfman (TOD) algorithm
#'
#' @description Summary measures for the Thresholded Optimal Dorfman (TOD)
#' algorithm.
#'
#' @param p.vec a vector of individual risk probabilities.
#' @param Se sensitivity of the diagnostic test.
#' @param Sp specificity of the diagnostic test.
#' @param max the maximum allowable group size. Further details are given under
#' 'Details'.
#' @param init.group.sz the initial group size used for TOD, if \kbd{threshold}
#' is not specified. Further details are given under 'Details'.
#' @param threshold the threshold value for TOD. If a threshold is not
#' specified, one is found algorithmically. Further details are given under
#' 'Details'.
#'
#' @details This function finds the characteristics of an informative two-stage
#' hierarchical (Dorfman) decoding process. Characteristics found include the
#' expected expenditure of the decoding process, the variance of the expenditure
#' of the decoding process, and the pooling sensitivity, pooling specificity,
#' pooling positive predictive value, and pooling negative predictive value for
#' each individual and for the overall algorithm. Calculations of these
#' characteristics are done using equations presented in McMahan et al. (2012).
#'
#' Thresholded Optimal Dorfman (TOD) is an informative Dorfman algorithm in
#' which all \eqn{N} individuals are partitioned into two classes, low-risk and
#' high-risk individuals. The threshold can be specified using the optional
#' \kbd{threshold} argument. Alternatively, the TOD algorithm can identify
#' the optimal threshold value. The low-risk individuals are tested using an
#' optimal common pool size, and the high-risk individuals are tested
#' individually. If desired, the user can add the constraint of a maximum
#' allowable group size (\kbd{max}), so that each group will contain no more
#' than the maximum allowable number of individuals.
#'
#' The displayed overall pooling sensitivity, pooling specificity, pooling
#' positive predictive value, and pooling negative predictive value are
#' weighted averages of the corresponding individual accuracy measures for all
#' individuals within the initial group (or block) for a hierarchical
#' algorithm, or within the entire array for an array-based algorithm.
#' Expressions for these averages are provided in the Supplementary
#' Material for Hitt et al. (2019). These expressions are based on accuracy
#' definitions given by Altman and Bland (1994a, 1994b).
#'
#' @return A list containing:
#' \item{prob}{the vector of individual risk probabilities, as specified
#' by the user.}
#' \item{Se}{the sensitivity of the diagnostic test, as specified by the user.}
#' \item{Sp}{the specificity of the diagnostic test, as specified by the user.}
#' \item{group.sz}{the initial group size used for TOD, if applicable.}
#' \item{thresh.val}{the threshold value used for TOD, if applicable.}
#' \item{OTC}{a list specifying elements of the optimal testing configuration,
#' which may include:
#' \describe{
#' \item{Block.sz}{the block size/initial group size for informative Dorfman
#' testing, which is not tested.}
#' \item{pool.szs}{group sizes for the first stage of testing for informative
#' Dorfman testing.}}}
#' \item{ET}{the expected testing expenditure to decode all individuals in the
#' algorithm.}
#' \item{Var}{the variance of the testing expenditure to decode all individuals
#' in the algorithm.}
#' \item{Accuracy}{a list containing:
#' \describe{
#' \item{Individual}{a matrix of accuracy measures for each individual.
#' The rows correspond to each unique set of accuracy
#' measures in the algorithm. Individuals with the same set of accuracy
#' measures are displayed together in a single row of the matrix. The columns
#' correspond to the pool index, the individual risk probability, and the
#' pooling sensitivity, pooling specificity, pooling positive predictive value,
#' and pooling negative predictive value for the individuals in each row of the
#' matrix.}
#' \item{Overall}{a matrix of overall accuracy measures for the algorithm.
#' The columns correspond to the pooling sensitivity, pooling specificity,
#' pooling positive predictive value, and pooling negative predictive value
#' for the overall algorithm. Further details are given under 'Details'.}}}
#'
#' @author Brianna D. Hitt
#'
#' @references
#' \insertRef{Altman1994a}{binGroup2}
#'
#' \insertRef{Altman1994b}{binGroup2}
#'
#' \insertRef{Hitt2019}{binGroup2}
#'
#' \insertRef{McMahan2012a}{binGroup2}
#'
#' @seealso
#' \code{\link{expectOrderBeta}} for generating a vector of individual risk
#' probabilities.
#'
#' @family operating characteristic functions
#'
#' @examples
#' # Example 1: Find the characteristics of an informative
#' #   Dorfman algorithm, using the TOD procedure.
#' set.seed(1002)
#' p.vec <- expectOrderBeta(p = 0.01, alpha = 2, size = 20)
#' TOD(p = p.vec, Se = 0.95, Sp = 0.95, max = 5,
#'     threshold = 0.015)
#'
#' # Example 2: Find the threshold value for the TOD
#' #   procedure algorithmically. Then, find
#' #   characteristics of the algorithm.
#' TOD(p = p.vec, Se = 0.95, Sp = 0.95, max = 5,
#'     init.group.sz = 10)

# Brianna Hitt - 3 November 2023
#   Added checks to ensure p.vec, Se, Sp are all between 0 and 1

TOD <- function(p.vec, Se, Sp, max = 15, init.group.sz = NULL, threshold = NULL) {

  if (Se < 0 | Se > 1) {
    stop("Please provide a sensitivity value between 0 and 1.\n")
  }

  if (Sp < 0 | Sp > 1) {
    stop("Please provide a specificity value between 0 and 1.\n")
  }

  if (any(p.vec < 0) | any(p.vec > 1)) {
    stop("Please provide individual risk probabilities between 0 and 1.\n")
  }

  if (is.null(init.group.sz) & is.null(threshold)) {
    stop("Please specify either an initial group size or threshold value for the Thresholded Optimal Dorfman (TOD) algorithm.")
  }
  if (!is.null(init.group.sz) & !is.null(threshold)) {
    message("Note: Both an initial group size and a threshold value have been specified. The initial group size will be ignored.")
  }
  if (!is.null(threshold)) {
    if (threshold >= 1 | threshold <= 0) {
      stop("Please specify a threshold value between 0 and 1 (exclusive).")
    }
  }

  # This is used to access the accuracy measures
  #   Future implementations could allow particular individuals rather than all
  #if (is.null(a)) {
  a <- 1:length(p.vec)
  #}


  # generate a vector of sensitivity/specificity values
  Se <- generate.acc(algorithm = "ID2", diseases = 1,
                     value = Se, label = "sens")
  Sp <- generate.acc(algorithm = "ID2", diseases = 1,
                     value = Sp, label = "spec")

  res <- opt.info.dorf(prob = p.vec, se = Se, sp = Sp, method ="TOD",
                       max.pool = max, thresh.pool = init.group.sz,
                       threshold = threshold)

  # extract accuracy measures for each individual
  all.ind.testerror <- res$summary[,-1]
  ind.testerror <- get.unique.index(all.ind.testerror[a, ],
                                    which(colnames(all.ind.testerror) == "PSp"),
                                    rowlabel = a)[,-1]
  colnames(ind.testerror) <- c("PSe", "PSP", "PPPV", "PNPV", "individuals")
  PSe.vec <- res$summary[,3]
  PSp.vec <- res$summary[,4]

  # calculate overall accuracy measures
  PSe <- sum(p.vec * PSe.vec) / sum(p.vec)
  PSp <- sum((1 - p.vec) * (PSp.vec)) / sum(1 - p.vec)
  PPPV <- sum(p.vec * PSe.vec) / sum(p.vec * PSe.vec +
                                       (1 - p.vec) * (1 - PSp.vec))
  PNPV <- sum((1 - p.vec) * PSp.vec) / sum((1 - p.vec) * PSp.vec +
                                             p.vec * (1 - PSe.vec))

  # put accuracy measures in a matrix for easier display of results
  acc.ET <- matrix(data = c(PSe, PSp, PPPV, PNPV), nrow = 1, ncol = 4,
                   dimnames = list(NULL, c("PSe", "PSp", "PPPV", "PNPV")))

  # create input accuracy value matrices for output display
  Se.display <- matrix(data = Se, nrow = 1, ncol = 2,
                       dimnames = list(NULL, "Stage" = 1:2))
  Sp.display <- matrix(data = Sp, nrow = 1, ncol = 2,
                       dimnames = list(NULL, "Stage" = 1:2))

  if (is.null(init.group.sz)) {
    group.sz <- NA
  }
  if (is.null(threshold)) {
    threshold <- NA
  }



  save.results <- list("prob" = p.vec, "Se" = Se.display, "Sp" = Sp.display,
       "thresh.val" = res$tv,
       "OTC" = list("Block.sz" = length(p.vec), "pool.szs" = res$pools),
       "ET" = res$e, "Var" = res$v,
       "Accuracy" = list("Individual" = res$summary, "Overall" = acc.ET))
  class(save.results) <- "TOD"
  save.results
}



# Print method for TOD function
##################################################################
# print.TOD() function                                           #
##################################################################

#' @title Print method for \kbd{TOD}
#'
#' @description Print method for objects obtained by
#' \code{\link{TOD}}.
#'
#' @param x An object of class "TOD" created by
#' \code{\link{TOD}}.
#' @param ... not currently used.
#'
#' @return A print out of configuration and operating characteristics
#' found with \code{\link{TOD}}.
#'
#' @author Chris Bilder
"print.TOD" <- function(x, ...) {
   cat("Overall block size:", x$OTC$Block.sz, "\n")
   cat("Ordered individual probabilities of positivity: \n",
     round(x$prob,4), "\n")
   cat("Group sizes for individuals ordered by probabilities of positivity: \n",
     x$OTC$pool.szs, "\n \n")
   cat("Expected number of tests:", round(x$ET,2), "\n")
   cat("Variance for the number of tests:", round(x$Var,2), "\n")
   invisible(x)
}



# ExpTests function
##################################################################
# ExpTests.TOD() function                                   #
##################################################################

#' @title Extract the expected number of tests from testing configuration results
#'
#' @description Extract the expected number of tests from objects of class "TOD" returned by
#' \code{\link{TOD}} (\kbd{TOD}).
#'
#' @param object An object of class "TOD", from which the expected number
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
#' set.seed(1002)
#' p.vec <- expectOrderBeta(p = 0.01, alpha = 2, size = 20)
#' save.it1 <- TOD(p = p.vec, Se = 0.95, Sp = 0.95, max = 5, threshold = 0.015)
#' ExpTests(save.it1)

ExpTests.TOD <- function(object, ...) {
  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  ExpTestsPerIndividual <- format(round(object$ET/length(object$prob), digits),
    nsmall = digits)
  PercentReductionTests <- format(round(100*(1-as.numeric(ExpTestsPerIndividual)), 2),
    nsmall = 2)
  PercentIncreaseTestCap <- format(round(100*(1/as.numeric(ExpTestsPerIndividual) - 1), 2),
    nsmall = 2)

  res <- data.frame(ExpTests = format(round(object$ET, digits),
    nsmall = digits),
    ExpTestsPerIndividual = ExpTestsPerIndividual, PercentReductionTests =
    PercentReductionTests, PercentIncreaseTestCap = PercentIncreaseTestCap)
  res
}













#
