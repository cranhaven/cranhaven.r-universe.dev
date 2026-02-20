# Start OTC1() function
###############################################################################
# Updated: Brianna Hitt - 12-06-19 (warning message text)
# Updated: Brianna Hitt - 01-22-20
#   Removed the printing of the algorithm name
#   Removed the printing of all "messages" other than warnings/errors

#' @title Find the optimal testing configuration for group testing algorithms
#' that use a single-disease assay
#'
#' @description Find the optimal testing configuration (OTC) using
#' non-informative and informative hierarchical and array-based group testing
#' algorithms. Single-disease assays are used at each stage of the algorithms.
#'
#' @param algorithm character string defining the group testing algorithm to
#' be used. Non-informative testing options include two-stage hierarchical
#' ("\kbd{D2}"), three-stage hierarchical ("\kbd{D3}"), square array testing
#' without master pooling ("\kbd{A2}"), and square array testing with master
#' pooling ("\kbd{A2M}"). Informative testing options include two-stage
#' hierarchical ("\kbd{ID2}"), three-stage hierarchical ("\kbd{ID3}"), and
#' square array testing without master pooling ("\kbd{IA2}").
#' @param p overall probability of disease that will be used to generate a
#' vector/matrix of individual probabilities. For non-informative algorithms,
#' a homogeneous set of probabilities will be used. For informative
#' algorithms, the \code{\link{expectOrderBeta}} function will be used to
#' generate a heterogeneous set of probabilities. Further details are given
#' under 'Details'. Either \kbd{p} or \kbd{probabilities} should be specified,
#' but not both.
#' @param probabilities a vector of individual probabilities, which is
#' homogeneous for non-informative testing algorithms and heterogeneous for
#' informative testing algorithms. Either  \kbd{p} or \kbd{probabilities}
#' should be specified, but not both.
#' @param Se a vector of sensitivity values, where one value is given for each
#' stage of testing (in order). If a single value is provided, sensitivity
#' values are assumed to be equal to this value for all stages of testing.
#' Further details are given under 'Details'.
#' @param Sp a vector of specificity values, where one value is given for each
#' stage of testing (in order). If a single value is provided, specificity
#' values are assumed to be equal to this value for all stages of testing.
#' Further details are given under 'Details'.
#' @param group.sz a single group size or range of group sizes for which to
#' calculate operating characteristics and/or find the OTC. The details of
#' group size specification are given under 'Details'.
#' @param obj.fn a list of objective functions which are minimized to find the
#' OTC. The expected number of tests per individual, "\kbd{ET}", will always
#' be calculated. Additional options include "\kbd{MAR}"
#' (the expected number of tests divided by the expected number of correct
#' classifications, described in Malinovsky et al. (2016)), and "\kbd{GR}"
#' (a linear combination of the expected number of tests, the number of
#' misclassified negatives, and the number of misclassified positives,
#' described in Graff & Roeloffs (1972)). See Hitt et al. (2019) for
#' additional details. The first objective function specified in this list
#' will be used to determine the results for the top configurations.
#' Further details are given under 'Details'.
#' @param weights a matrix of up to six sets of weights for the GR function.
#' Each set of weights is specified by a row of the matrix.
#' @param alpha a shape parameter for the beta distribution that specifies the
#' degree of heterogeneity for the generated probability vector (for
#' informative testing only).
#' @param trace a logical value indicating whether the progress of
#' calculations should be printed for each initial group size provided by
#' the user. The default is \kbd{TRUE}.
#' @param print.time a logical value indicating whether the length of time
#' for calculations should be printed. The default is \kbd{TRUE}.
#' @param ... arguments to be passed to the \code{\link{expectOrderBeta}}
#' function, which generates a vector of probabilities for informative testing
#' algorithms. Further details are given under 'Details'.
#'
#' @details This function finds the OTC for group testing algorithms
#' with an assay that tests for one disease and computes the associated
#' operating characteristics, as described in Hitt et al. (2019).
#'
#' Available algorithms include two- and three-stage hierarchical testing and
#' array testing with and without master pooling. Both non-informative and
#' informative group testing settings are allowed for each algorithm, except
#' informative array testing with master pooling is unavailable because this
#' method has not appeared in the group testing literature. Operating
#' characteristics calculated are expected number of tests, pooling
#' sensitivity, pooling specificity, pooling positive predictive value, and
#' pooling negative predictive value for each individual.
#'
#' For informative algorithms where the \kbd{p} argument is specified, the
#' expected value of order statistics from a beta distribution are found.
#' These values are used to represent disease risk probabilities for each
#' individual to be tested. The beta distribution has two parameters: a mean
#' parameter \kbd{p} (overall disease prevalence) and a shape parameter
#' \kbd{alpha} (heterogeneity level). Depending on the specified \kbd{p},
#' \kbd{alpha}, and overall group size, simulation may be necessary to
#' generate the vector of individual probabilities. This is done using
#' \code{\link{expectOrderBeta}} and requires the user to set a seed to
#' reproduce results.
#'
#' Informative two-stage hierarchical (Dorfman) testing is implemented via
#' the pool-specific optimal Dorfman (PSOD) method described in McMahan et al.
#' (2012a), where the greedy algorithm proposed for PSOD is replaced by
#' considering all possible testing configurations. Informative array testing
#' is implemented via the gradient method (the most efficient array design),
#' where higher-risk individuals are grouped in the left-most columns of the
#' array. For additional details on the gradient arrangement method for
#' informative array testing, see McMahan et al. (2012b).
#'
#' The sensitivity/specificity values are allowed to vary across stages of
#' testing. For hierarchical testing, a different sensitivity/specificity
#' value may be used for each stage of testing. For array testing, a different
#' sensitivity/specificity value may be used for master pool testing (if
#' included), row/column testing, and individual testing. The values must be
#' specified in order of the testing performed. For example, values are
#' specified as (stage 1, stage 2, stage 3) for three-stage hierarchical
#' testing or (master pool testing, row/column testing, individual testing)
#' for array testing with master pooling. A single sensitivity/specificity
#' value may be specified instead. In this situation, sensitivity/specificity
#' values for all stages are assumed to be equal.
#'
#' The value(s) specified by \kbd{group.sz} represent the initial (stage 1)
#' group size for hierarchical testing and the row/column size for array
#' testing. For informative two-stage hierarchical testing, the \kbd{group.sz}
#' specified represents the block size used in the pool-specific optimal
#' Dorfman (PSOD) method, where the initial group (block) is not tested. For
#' more details on informative two-stage hierarchical testing implemented via
#' the PSOD method, see Hitt et al. (2019) and McMahan et al. (2012a).
#'
#' If a single value is provided for \kbd{group.sz} with array testing or
#' non-informative two-stage hierarchical testing, operating characteristics
#' will be calculated and no optimization will be performed. If a single value
#' is provided for \kbd{group.sz} with three-stage hierarchical or informative
#' two-stage hierarchical, the OTC will be found over all possible
#' configurations. If a range of group sizes is specified, the OTC will be
#' found over all group sizes.
#'
#' In addition to the OTC, operating characteristics for some of the other
#' configurations corresponding to each initial group size provided by the
#' user will be displayed. These additional configurations are only determined
#' for whichever objective function ("ET", "MAR", or "GR") is specified first
#' in the function call. If "GR" is the objective function listed first, the
#' first set of corresponding weights will be used. For algorithms where there
#' is only one configuration for each initial group size (non-informative
#' two-stage hierarchical and all array testing algorithms), results for each
#' initial group size are provided. For algorithms where there is more than
#' one possible configuration for each initial group size (informative
#' two-stage hierarchical and all three-stage hierarchical algorithms), two
#' sets of configurations are provided: 1) the best configuration for each
#' initial group size, and 2) the top 10 configurations for each initial group
#' size provided by the user. If a single value is provided for \kbd{group.sz}
#' with array testing or non-informative two-stage hierarchical testing,
#' operating characteristics will not be provided for configurations other
#' than that specified by the user. Results are sorted by the value of the
#' objective function per individual, \kbd{value}.
#'
#' The displayed overall pooling sensitivity, pooling specificity, pooling
#' positive predictive value, and pooling negative predictive value are
#' weighted averages of the corresponding individual accuracy measures for all
#' individuals within the initial group (or block) for a hierarchical
#' algorithm, or within the entire array for an array-based algorithm.
#' Expressions for these averages are provided in the Supplementary
#' Material for Hitt et al. (2019). These expressions are based on accuracy
#' definitions given by Altman and Bland (1994a, 1994b). Individual
#' accuracy measures can be calculated using the
#' \code{\link{operatingCharacteristics1}} (\code{\link{opChar1}}) function.
#'
#' The \kbd{OTC1} function accepts additional arguments, namely \kbd{num.sim},
#' to be passed to the \code{\link{expectOrderBeta}} function, which generates
#' a vector of probabilities for informative group testing algorithms. The
#' \kbd{num.sim} argument specifies the number of simulations from the beta
#' distribution when simulation is used. By default, 10,000 simulations are
#' used.
#'
#' @return A list containing:
#' \item{algorithm}{the group testing algorithm used for calculations.}
#' \item{prob}{the probability of disease or the vector of individual
#' probabilities, as specified by the user.}
#' \item{alpha}{level of heterogeneity for the generated probability vector
#' (for informative testing only).}
#' \item{Se}{the vector of sensitivity values for each stage of testing.}
#' \item{Sp}{the vector of specificity values for each stage of testing.}
#' \item{opt.ET, opt.MAR, opt.GR}{a list of results for each
#' objective function specified by the user, containing:
#' \describe{
#' \item{OTC}{a list specifying elements of the optimal testing configuration,
#' which may include:
#' \describe{
#' \item{Stage1}{group size for the first stage of hierarchical testing, if
#' applicable.}
#' \item{Stage2}{group sizes for the second stage of hierarchical testing, if
#' applicable.}
#' \item{Block.sz}{the block size/initial group size for informative Dorfman
#' testing, which is not tested.}
#' \item{pool.szs}{group sizes for the first stage of testing for informative
#' Dorfman testing.}
#' \item{Array.dim}{the row/column size for array testing.}
#' \item{Array.sz}{the overall array size for array testing (the square of the
#' row/column size).}}}
#' \item{p.vec}{the sorted vector of individual probabilities, if applicable.}
#' \item{p.mat}{the sorted matrix of individual probabilities in gradient
#' arrangement, if applicable. Further details are given under 'Details'.}
#' \item{ET}{the expected testing expenditure to decode all individuals in the
#' algorithm; this includes all individuals in all groups for hierarchical
#' algorithms or in the entire array for array testing.}
#' \item{value}{the value of the objective function per individual.}
#' \item{Accuracy}{a matrix of overall accuracy measures for the
#' algorithm. The columns correspond to the pooling sensitivity,
#' pooling specificity, pooling positive predictive value, and
#' pooling negative predictive value for the overall algorithm.
#' Further details are given under 'Details'.}}}
#' \item{Configs}{a data frame containing results for the best configuration
#' for each initial group size provided by the user. The columns correspond to
#' the initial group size, configuration (if applicable), overall array size
#' (if applicable), expected number of tests, value of the objective function
#' per individual, pooling sensitivity, pooling specificity, pooling positive
#' predictive value, and pooling negative predictive value. No results are
#' displayed if a single \kbd{group.sz} is provided. Further details are given
#' under 'Details'.}
#' \item{Top.Configs}{a data frame containing results for the top overall
#' configurations across all initial group sizes provided by the user. The
#' columns correspond to the initial group size, configuration,
#' expected number of tests, value of the objective function per individual,
#' pooling sensitivity, pooling specificity, pooling positive predictive
#' value, and pooling negative predictive value. No results are displayed for
#' non-informative two-stage hierarchical testing or for array testing
#' algorithms. Further details are given under 'Details'.}
#' \item{group.sz}{Initial group (or block) sizes examined to find the OTC.}
#'
#' @section Note: This function returns the pooling positive and negative
#' predictive values for all individuals even though these measures are
#' diagnostic specific; e.g., the pooling positive predictive value should
#' only be considered for those individuals who have tested positive.
#'
#' Additionally, only stage dependent sensitivity and specificity values are
#' allowed within the program (no group within stage dependent values are
#' allowed). See Bilder et al. (2019) for additional information.
#'
#' @author Brianna D. Hitt
#'
#' @references
#' \insertRef{Altman1994a}{binGroup2}
#'
#' \insertRef{Altman1994b}{binGroup2}
#'
#' \insertRef{Bilder2019}{binGroup2}
#'
#' \insertRef{Graff1972}{binGroup2}
#'
#' \insertRef{Hitt2019}{binGroup2}
#'
#' \insertRef{Malinovsky2016}{binGroup2}
#'
#' \insertRef{McMahan2012a}{binGroup2}
#'
#' \insertRef{McMahan2012b}{binGroup2}
#'
#' @family OTC functions
#'
#' @examples
#' # Find the OTC for non-informative
#' #   two-stage hierarchical (Dorfman) testing.
#' OTC1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'      group.sz = 2:100, obj.fn = "ET",
#'      trace = TRUE, print.time = TRUE)
#'
#' # Find the OTC for informative two-stage hierarchical
#' #   (Dorfman) testing.
#' # A vector of individual probabilities is generated using
#' #   the expected value of order statistics from a beta
#' #   distribution with p = 0.01 and a heterogeneity level
#' #   of alpha = 0.5.
#' \donttest{set.seed(52613)
#' OTC1(algorithm = "ID2", p = 0.01, Se = 0.95, Sp = 0.95,
#'      group.sz = 50, obj.fn = c("ET", "MAR", "GR"),
#'      weights = matrix(data = c(1, 1, 10, 10, 0.5, 0.5),
#'      nrow = 3, ncol = 2, byrow = TRUE), alpha = 0.5,
#'      trace = FALSE, print.time = TRUE, num.sim = 10000)}
#'
#' # Find the OTC over all possible testing configurations
#' #   for non-informative three-stage hierarchical testing
#' #   with a specified group size.
#' OTC1(algorithm = "D3", p = 0.001, Se = 0.95, Sp = 0.95,
#'      group.sz = 18, obj.fn = "ET",
#'      trace = FALSE, print.time = FALSE)
#'
#' # Find the OTC for non-informative three-stage
#' #   hierarchical testing.
#' \donttest{OTC1(algorithm = "D3", p = 0.06, Se = 0.90, Sp = 0.90,
#'      group.sz = 3:30, obj.fn = c("ET", "MAR", "GR"),
#'      weights = matrix(data = c(1, 1, 10, 10, 100, 100),
#'      nrow = 3, ncol = 2, byrow = TRUE))}
#'
#' # Find the OTC over all possible configurations
#' #   for informative three-stage hierarchical testing
#' #   with a specified group size and a heterogeneous
#' #   vector of probabilities.
#' set.seed(1234)
#' OTC1(algorithm = "ID3",
#'      probabilities = c(0.012, 0.014, 0.011,
#'                        0.012, 0.010, 0.015),
#'      Se = 0.99, Sp = 0.99, group.sz = 6,
#'      obj.fn = "ET",
#'      alpha = 0.5, num.sim = 5000, trace = FALSE)
#'
#' # Calculate the operating characteristics for
#' #   non-informative array testing without master pooling
#' #   with a specified array size.
#' OTC1(algorithm = "A2", p = 0.005, Se = 0.95, Sp = 0.95,
#'      group.sz = 8, obj.fn = "ET", trace = FALSE)
#'
#' # Find the OTC for informative array testing without
#' #   master pooling.
#' # A vector of individual probabilities is generated using
#' #   the expected value of order statistics from a beta
#' #   distribution with p = 0.03 and a heterogeneity level
#' #   of alpha = 2. The probabilities are then arranged in
#' #   a matrix using the gradient method.
#' \donttest{set.seed(1002)
#' OTC1(algorithm = "IA2", p = 0.03, Se = 0.95, Sp = 0.95,
#'      group.sz = 2:20, obj.fn = c("ET", "MAR", "GR"),
#'      weights = matrix(data = c(1, 1, 10, 10, 100, 100),
#'                       nrow = 3, ncol = 2, byrow = TRUE),
#'      alpha = 2)}
#'
#' # Find the OTC for non-informative array testing
#' #   with master pooling. The calculations may not
#' #   be completed instantaneously.
#' \donttest{OTC1(algorithm = "A2M", p = 0.04, Se = 0.90, Sp = 0.90,
#'      group.sz = 2:20, obj.fn = "ET")}

# Brianna Hitt - 04.02.2020
# Changed cat() to warning()

# Brianna Hitt - 06.08.2020
# Allowed for group sizes of 2

# Brianna Hitt - 03.11.2021
# Removed "MAR" as a default objective function
# Changed warning() to message() for large initial group sizes that will
#   take significant time to run in OTC1(); message() allows the warning text
#   to display at the function start rather than at its completion. The warning
#   text was also edited.

# Brianna Hitt - 10.18.2023
# Added function security to ensure probabilities, Se, and Sp
#   are all between 0 and 1

OTC1 <- function(algorithm, p = NULL, probabilities = NULL,
                 Se = 0.99, Sp = 0.99, group.sz, obj.fn = "ET",
                 weights = NULL, alpha = 2,
                 trace = TRUE, print.time = TRUE, ...) {

  ## make sure that all necessary information is included in the correct format
  if (!(algorithm %in% c("D2", "D3", "A2", "A2M", "ID2", "ID3", "IA2"))) {
    stop("Please specify one of the following algorithms: D2, ID2, D3, ID3, A2, IA2, A2M.")
  }

  if (is.null(p) & is.null(probabilities)) {
    stop("Please specify an overall probability of disease using the 'p' argument, or specify a vector of individual probabilities using the 'probabilities' argument.")
  } else if (!is.null(p) & !is.null(probabilities)) {
    stop("You have specified both an overall probability of disease AND a vector of individual probabilities. Please specify only one option.")
  } else {
    if (!is.null(p)) {
      if (length(p) > 1) {
        stop("You have specified a probability vector instead of an overall probability of disease. Please specify an overall probability of disease, and the probability vector will be generated based on the algorithm specified for each group size included in the range.\n")
      }

      if (p > 1 | p < 0) {
        stop("Please specify an overall probability of disease between 0 and 1.\n")
      }
    }

    if (!is.null(probabilities)) {
      if(any(probabilities > 1) | any(probabilities < 0)) {
        stop("Please specify individual probabilities between 0 and 1.\n")
      }
      if (length(group.sz) == 1) {
        if ((algorithm %in% c("D2", "D3", "ID2", "ID3")) &
            length(probabilities) != group.sz) {
          stop("The vector of individual probabilities is not the correct length. Please make sure that the length of the probability vector is the same as the specified group size.\n")
        } else if ((algorithm %in% c("A2", "A2M", "IA2")) &
                   length(probabilities) != group.sz^2) {
          stop("The vector of individual probabilities is not the correct length. Please make sure that the length of the probability vector is the same as the overall array size (the square of the specified row/column size).\n")
        }
        if ((algorithm %in% c("D2", "D3", "A2", "A2M")) &
            all.equal(probabilities, rep(probabilities[1],
                                         length(probabilities))) != TRUE) {
          stop("You have specified a heterogeneous probability vector for a non-informative algorithm. Please specify a homogeneous probability vector using the 'probabilities' argument or specify an overall probability of disease using the 'p' argument.\n")
        }
      } else if (length(group.sz) > 1) {
        stop("You have specified a probability vector along with a range of group sizes. Please specify a single group size.\n")
      }
    }
  }

  if (length(Se) == 1) {
    if (Se < 0 | Se > 1) {
      stop("Please provide sensitivity values between 0 and 1.\n")
    }
  }
  else if (length(Se) > 1) {
    if (any(Se < 0) | any(Se > 1)) {
      stop("Please provide sensitivity values between 0 and 1.\n")
    }

    if ((algorithm %in% c("D2", "ID2", "A2", "IA2") & length(Se) != 2) |
        (algorithm %in% c("D3", "ID3", "A2M") & length(Se) != 3) |
        (algorithm %in% c("D4", "ID4") & length(Se) != 4)) {
      stop("The vector of sensitivity values is not the correct length. Please specify a vector of sensitivity values (one for each stage of testing, in order), or a single value for all stages of testing.\n")
    }
  }

  if (length(Sp) == 1) {
    if (Sp < 0 | Sp > 1) {
      stop("Please provide specificity values between 0 and 1.\n")
    }
  }
  else if (length(Sp) > 1) {
    if (any(Sp < 0) | any(Sp > 1)) {
      stop("Please provide specificity values between 0 and 1.\n")
    }

    if ((algorithm %in% c("D2", "ID2", "A2", "IA2") & length(Sp) != 2) |
        (algorithm %in% c("D3", "ID3", "A2M") & length(Sp) != 3) |
        (algorithm %in% c("D4", "ID4") & length(Sp) != 4)) {
      stop("The vector of specificity values is not the correct length. Please specify a vector of specificity values (one for each stage of testing, in order), or a single value for all stages of testing.\n")
    }
  }

  Se <- generate.acc(algorithm = algorithm, diseases = 1,
                     value = Se, label = "sens")
  Sp <- generate.acc(algorithm = algorithm, diseases = 1,
                     value = Sp, label = "spec")

  # check the minimum and maximum group sizes
  if (min(group.sz) < 3) {
    if (algorithm %in% c("D3", "ID2", "ID3")) {
      stop("Please specify a minimum group size of at least 3.\n")
    }
    if (min(group.sz) < 2) {
      if (algorithm %in% c("D2")) {
        stop("Please specify a minimum group size of at least 2.\n")
      } else if (algorithm %in% c("A2", "IA2", "A2M")) {
        stop("Please specify a minimum row/column size of at least 2.\n")
      }
    }
  }
  if (max(group.sz) >= 50) {
    if (algorithm %in% c("D3", "ID2", "ID3")) {
      message("Note: Because the maximum group size is 50 or larger, this function may take a significant amount of time to run. Press 'ESC' if you wish to cancel the submitted statements.\n")
    } else if (algorithm %in% c("A2", "A2M", "IA2")) {
      message("Note: Because the maximum row/column size is 50 or larger, this function may take a significant amount of time to run. Press 'ESC' if you wish to cancel the submitted statements.\n")
    }
  }

  if (is.null(obj.fn)) {
    stop("Please specify one or more objective functions for which to find the optimal testing configuration.\n")
  }

  if (!("ET" %in% obj.fn)) {
    obj.fn <- c(obj.fn, "ET")
  }

  if ("GR" %in% obj.fn) {
    if (is.null(weights)) {
      stop("No weights have been specified. The GR function will not be calculated.\n")
    } else if (dim(weights)[2] != 2) {
      stop("Please check the dimension of the weights matrix. Each row should specify a set of weights, D1 and D2.\n")
    }
  }

  # call function for non-informative two-stage hierarchical (Dorfman) testing
  if (algorithm == "D2") {
    if (!is.null(p)) {
      results <- NI.Dorf.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                              obj.fn = obj.fn, weights = weights,
                              trace = trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- NI.Dorf.OTC1(p = probabilities, Se = Se, Sp = Sp,
                              group.sz = group.sz,
                              obj.fn = obj.fn, weights = weights,
                              trace = trace, print.time = print.time, ...)
    }
  }

  # call function for non-informative three-stage hierarchical testing
  if (algorithm == "D3") {
    if (!is.null(p)) {
      results <- NI.D3.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                            obj.fn = obj.fn, weights = weights,
                            trace = trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- NI.D3.OTC1(p = probabilities, Se = Se, Sp = Sp,
                            group.sz = group.sz,
                            obj.fn = obj.fn, weights = weights,
                            trace = trace, print.time = print.time, ...)
    }
  }

  # call function for non-informative square array testing without
  # master pooling
  if (algorithm == "A2") {
    if (!is.null(p)) {
      results <- NI.Array.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                               obj.fn = obj.fn, weights = weights,
                               trace = trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- NI.Array.OTC1(p = probabilities, Se = Se, Sp = Sp,
                               group.sz = group.sz,
                               obj.fn = obj.fn, weights = weights,
                               trace = trace, print.time = print.time, ...)
    }
  }

  # call function for non-informative square array testing with master pooling
  if (algorithm == "A2M") {
    if (!is.null(p)) {
      results <- NI.A2M.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                             obj.fn = obj.fn, weights = weights,
                             trace = trace, print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- NI.A2M.OTC1(p = probabilities, Se = Se, Sp = Sp,
                             group.sz = group.sz,
                             obj.fn = obj.fn, weights = weights,
                             trace = trace, print.time = print.time, ...)
    }
  }

  # call function for informative two-stage hierarchical (Dorfman) testing
  if (algorithm == "ID2") {
    if (!is.null(p)) {
      results <- Inf.Dorf.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                               obj.fn = obj.fn, weights = weights,
                               alpha = alpha, trace = trace,
                               print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.Dorf.OTC1(p = probabilities, Se = Se, Sp = Sp,
                               group.sz = group.sz, obj.fn = obj.fn,
                               weights = weights, alpha = alpha,
                               trace = trace, print.time = print.time, ...)
    }
  }

  # call function for informative three-stage hierarchical testing
  if (algorithm == "ID3") {
    if (!is.null(p)) {
      results <- Inf.D3.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                             obj.fn = obj.fn, weights = weights,
                             alpha = alpha, trace = trace,
                             print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.D3.OTC1(p = probabilities, Se = Se, Sp = Sp,
                             group.sz = group.sz, obj.fn = obj.fn,
                             weights = weights, alpha = alpha,
                             trace = trace, print.time = print.time, ...)
    }
  }

  # call function for informative square array testing without master pooling
  if (algorithm == "IA2") {
    if (!is.null(p)) {
      results <- Inf.Array.OTC1(p = p, Se = Se, Sp = Sp, group.sz = group.sz,
                                obj.fn = obj.fn, weights = weights,
                                alpha = alpha, trace = trace,
                                print.time = print.time, ...)
    } else if (!is.null(probabilities)) {
      results <- Inf.Array.OTC1(p = probabilities, Se = Se, Sp = Sp,
                                group.sz = group.sz, obj.fn = obj.fn,
                                weights = weights, alpha = alpha,
                                trace = trace, print.time = print.time, ...)
    }
  }

  results$group.sz <- group.sz  # save initial group sizes, helpful for method functions
  class(results) <- "OTC"
  results
}



