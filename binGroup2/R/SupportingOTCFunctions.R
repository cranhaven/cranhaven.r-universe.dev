# Supporting functions for OTC functions and the associated calls
###############################################################################

#' @title Determine a vector of probabilities for informative group
#' testing algorithms
#'
#' @description Find the expected value of order statistics from a beta
#' distribution. This function is used to provide a set of individual
#' risk probabilities for informative group testing.
#'
#' @param p overall probability of disease that will be used to determine a
#' vector of individual risk probabilities. This is the expected value of a
#' random variable with a beta distribution,
#' \eqn{\frac{\alpha}{\alpha + \beta}}{\alpha/(\alpha + \beta)}.
#' @param alpha a shape parameter for the beta distribution that
#' specifies the degree of heterogeneity for the determined
#' probability vector.
#' @param size the size of the vector of individual risk probabilities to be
#' generated. This is also the number of total individuals for which to
#' determine risk probabilities.
#' @param grp.sz the number of total individuals for which to determine risk
#' probabilities. This argument is deprecated; the \kbd{size} argument should
#' be used instead.
#' @param num.sim the number of simulations. This argument is used only when
#' simulation is necessary.
#' @param rel.tol relative tolerance used for integration.
#' @param ... arguments to be passed to the \code{beta.dist} function
#' written by Michael Black for Black et al. (2015).
#'
#' @details This function uses the \code{beta.dist} function from
#' Black et al. (2015) to determine a vector of individual risk probabilities,
#' ordered from least to greatest. Depending on the specified probability,
#' \eqn{\alpha} level, and overall group size, simulation may be necessary in
#' order to determine the probabilities. For this reason, the user should set
#' a seed in order to reproduce results. The number of simulations (default =
#' 10,000) and relative tolerance for integration can be specified by the user.
#' The \kbd{expectOrderBeta} function augments the \code{beta.dist} function by
#' checking whether simulation is needed before attempting to determine the
#' probabilities, and by allowing the number of simulations to be specified by
#' the user. See Black et al. (2015) for additional details on the original
#' \kbd{beta.dist} function.
#'
#' @return A vector of individual risk probabilities.
#'
#' @author Brianna D. Hitt
#'
#' @references
#' \insertRef{Black2015}{binGroup2}
#'
#' @seealso \code{\link{informativeArrayProb}} for
#' arranging a vector of individual risk probabilities in a matrix for
#' informative array testing without master pooling.
#'
#' @examples
#' set.seed(8791)
#' expectOrderBeta(p = 0.03, alpha = 0.5, size = 100, rel.tol = 0.0001)
#'
#' expectOrderBeta(p = 0.05, alpha = 2, size = 40)

# Brianna Hitt - 03.12.2021
# Added the num.sim and rel.tol arguments to the function call (rather than
#   leaving them as additional arguments in the ... argument).
# Edited examples

expectOrderBeta <- function(p, alpha, size, grp.sz, num.sim = 10000,
                            rel.tol = ifelse(alpha >= 1, .Machine$double.eps^0.25,
                                             .Machine$double.eps^0.1), ...) {

  if (!missing(grp.sz)) {
    warning("argument grp.sz is deprecated; please use size instead.",
            call. = FALSE)
    size <- grp.sz
  }

  if (is.na(p)) {
    NA
  } else {
    p.try <- suppressWarnings(try(beta.dist2(p = p, alpha = alpha,
                                             grp.sz = size, rel.tol = rel.tol,
                                             ...),
                                  silent = TRUE))
    if (inherits(p.try, "try-error")) {
      beta.dist2(p = p, alpha = alpha, grp.sz = size, simul = TRUE,
                 num.sim = num.sim, rel.tol = rel.tol, ...)
    } else {
      beta.dist2(p = p, alpha = alpha, grp.sz = size, rel.tol = rel.tol, ...)
    }
  }
}

###############################################################################




# Start MAR.func() function
###############################################################################
#    Brianna Hitt - 4-17-17
#    Purpose: calculates MAR objective function, from Malinovsky,
#               Albert & Roy (2015)
#      inputs: ET - expected number of tests
#              p.vec - vector of individual probabilities
#              PSe.vec - vector of individual pooling sensitivities
#              PSp.vec - vector of individual pooling specificities
#      Note: The MAR objective function divides ET, the expected number of
#              tests, by EC, the expected number of correct classifications,
#              and should be minimized.
#      Note: Malinovsky, Albert, & Roy (2015) maximized the reciprocal,
#              E(C)/E(T).

MAR.func <- function(ET, p.vec, PSe.vec, PSp.vec) {
  EC <- sum(PSe.vec * p.vec + PSp.vec * (1 - p.vec))
  ET / EC
}
###############################################################################




# Start GR.func() function
###############################################################################
#    Brianna Hitt - 4-17-17
#    Purpose: calculates GR objective function, from Graff & Roeloffs (1972)
#               M = E(T) + D_1*(# of misclassified negatives)
#                        + D_2*(# of misclassified positives)
#      inputs: ET - expected number of tests
#              p.vec - vector of individual probabilities
#              PSe.vec - vector of individual pooling sensitivities
#              PSp.vec - vector of individual pooling specificities
#              D1, D2 - weights/costs for misclassification
#      note: this function specifies equal weights of 1 by default

GR.func <- function(ET, p.vec, PSe.vec, PSp.vec, D1 = 1, D2 = 1) {
  ET + D1 * sum((1 - PSp.vec) * (1 - p.vec)) +
    D2 * sum((1 - PSe.vec) * p.vec)
}
###############################################################################




# Start time.it() function
###############################################################################
#    Brianna Hitt - 5-13-17
#    Purpose: calculates the time elapsed
#      inputs: x = object containing the start time

time.it <- function(x) {
  end.time <- proc.time()
  save.time <- end.time - x
  cat("\n Number of minutes running: ", round(save.time[3] / 60, 2), "\n \n")
  save.time[3] / 60
}
###############################################################################





# Start generate.acc() function
###############################################################################
#    Brianna Hitt - 11-15-19
#    Purpose: generates a vector of sensitivity/specificity values
#             with the appropriate dimensions
#      inputs: algorithm = the group testing algorithm
#              diseases = number of diseases for the diagnostic assay
#              value = the user-specified Se/Sp value for OTC1() and OTC2()
#              label = "sens" for sensitivity, "spec" for specificity

generate.acc <- function(algorithm, diseases, value, label) {
  if (label == "sens") {
    measure <- "sensitivity"
  } else if (label == "spec") {
    measure <- "specificity"
  }

  if (diseases == 1) {
    if (length(value) == 1) {
      if (algorithm %in% c("D2", "ID2", "A2", "IA2")) {
        output <- rep(value, 2)
      } else if (algorithm %in% c("D3", "ID3", "A2M")) {
        output <- rep(value, 3)
      } else if (algorithm %in% c("D4", "ID4")) {
        output <- rep(value, 4)
      }
    } else if (algorithm %in% c("D2", "ID2", "A2", "IA2")) {
      if (length(value) == 2) {
        output <- value
      } else {
        stop("Please specify a ", measure,
             " vector with two values, one for each stage of the testing algorithm.\n")
      }
    } else if (algorithm %in% c("D3", "ID3", "A2M")) {
      if (length(value) == 3) {
        output <- value
      } else {
        stop("Please specify a ", measure,
             " vector with three values, one for each stage of the testing algorithm.\n")
      }
    } else if (algorithm %in% c("D4", "ID4")) {
      if (length(value) == 4) {
        output <- value
      } else {
        stop("Please specify a ", measure,
             " vector with four values, one for each stage of the testing algorithm.\n")
      }
    }
  } else if (diseases == 2) {
    if (is.vector(value)) {
      if (length(value) == 2) {
        if (algorithm %in% c("D2", "ID2", "A2", "IA2")) {
          output <- matrix(data = value, nrow = 2, ncol = 2,
                           dimnames = list(Infection = 1:2, Stage = 1:2))
        } else if (algorithm %in% c("D3", "ID3", "A2M")) {
          output <- matrix(data = value, nrow = 2, ncol = 3,
                           dimnames = list(Infection = 1:2, Stage = 1:3))
        } else if (algorithm %in% c("D4", "ID4")) {
          output <- matrix(data = value, nrow = 2, ncol = 4,
                           dimnames = list(Infection = 1:2, Stage = 1:4))
        } else if (algorithm %in% c("D5", "ID5")) {
          output <- matrix(data = value, nrow = 2, ncol = 5,
                           dimnames = list(Infection = 1:2, Stage = 1:5))
        }
      } else {
        stop("Please specify a matrix of ", measure,
             " values with the correct dimensions. Each row should correspond to a disease, and each column should correspond to a stage in the algorithm.\n")
      }
    } else if (is.matrix(value)) {
      if (algorithm %in% c("D2", "ID2", "A2", "IA2")) {
        if (dim(value)[1] == 2 & dim(value)[2] == 2) {
          output <- value
        } else {
          stop("Please specify a 2x2 matrix of ", measure, "
               values with the correct dimensions. Each row should correspond to a disease, and each column should correspond to a stage in the algorithm.\n")
        }
      } else if (algorithm %in% c("D3", "ID3", "A2M")) {
        if (dim(value)[1] == 2 & dim(value)[2] == 3) {
          output <- value
        } else {
          stop("Please specify a 2x3 matrix of ", measure,
               " values with the correct dimensions. Each row should correspond to a disease, and each column should correspond to a stage in the algorithm.\n")
        }
      } else if (algorithm %in% c("D4", "ID4")) {
        if (dim(value)[1] == 2 & dim(value)[2] == 4) {
          output <- value
        } else {
          stop("Please specify a 2x4 matrix of ", measure,
               " values with the correct dimensions. Each row should correspond to a disease, and each column should correspond to a stage in the algorithm.\n")
        }
      } else if (algorithm %in% c("D5", "ID5")) {
        if (dim(value)[1] == 2 & dim(value)[2] == 5) {
          output <- value
        } else {
          stop("Please specify a 2x5 matrix of ", measure,
               " values with the correct dimensions. Each row should correspond to a disease, and each column should correspond to a stage in the algorithm.\n")
        }
      }
    }
  }
}




###############################################################################
# Start convert.config() function
###############################################################################
# Brianna Hitt
# Support function for OTC functions for one and two diseases
# converts configurations to a usable format for OTC functions

# updated convert.config() allows two disease functions
# updated 04-08-18 to allow for two disease assays
# function to convert the configurations to a single column

convert.config <- function(algorithm, results, diseases = 1,
                           old.label = "pool.sz", new.label = "pool.szs",
                           sep = ",") {
  new.results <- cbind(results, NA)
  colnames(new.results)[dim(new.results)[2]] <- new.label

  index.pools <- which(colnames(new.results) == old.label)[1]

  if (algorithm %in% c("D2")) {
    if (diseases == 1) {
      final <- data.frame(I = as.numeric(new.results[,which(colnames(new.results) == "I")]),
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe")]), 4), nsmall = 4)),
                          PSp = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp")]), 4), nsmall = 4)),
                          PPPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV")]), 4), nsmall = 4)),
                          PNPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV")]), 4), nsmall = 4)),
                          row.names = NULL)
    } else if (diseases == 2) {
      final <- data.frame(I = as.numeric(new.results[,which(colnames(new.results) == "I")]),
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe1")]), 4), nsmall = 4)),
                          PSp1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp1")]), 4), nsmall = 4)),
                          PPPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV1")]), 4), nsmall = 4)),
                          PNPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV1")]), 4), nsmall = 4)),
                          PSe2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe2")]), 4), nsmall = 4)),
                          PSp2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp2")]), 4), nsmall = 4)),
                          PPPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV2")]), 4), nsmall = 4)),
                          PNPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV2")]), 4), nsmall = 4)),
                          row.names = NULL)
    }
  } else if (algorithm %in% c("A2", "IA2", "A2M")) {
    if (diseases == 1) {
      final <- data.frame(I = as.numeric(new.results[,which(colnames(new.results) == "I")]),
                          N = as.numeric(new.results[,which(colnames(new.results) == "N")]),
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe")]), 4), nsmall = 4)),
                          PSp = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp")]), 4), nsmall = 4)),
                          PPPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV")]), 4), nsmall = 4)),
                          PNPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV")]), 4), nsmall = 4)),
                          row.names = NULL)
    } else if (diseases == 2) {
      final <- data.frame(I = as.numeric(new.results[,which(colnames(new.results) == "I")]),
                          N = as.numeric(new.results[,which(colnames(new.results) == "N")]),
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe1")]), 4), nsmall = 4)),
                          PSp1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp1")]), 4), nsmall = 4)),
                          PPPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV1")]), 4), nsmall = 4)),
                          PNPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV1")]), 4), nsmall = 4)),
                          PSe2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe2")]), 4), nsmall = 4)),
                          PSp2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp2")]), 4), nsmall = 4)),
                          PPPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV2")]), 4), nsmall = 4)),
                          PNPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV2")]), 4), nsmall = 4)),
                          row.names = NULL)
    }
  } else if (algorithm %in% c("ID2")) {
    for (i in 1:dim(new.results)[1]) {

      config <- paste(new.results[i,index.pools],
                      new.results[i,(index.pools + 1)], sep = sep)
      for (j in (index.pools + 2):(dim(new.results)[2] - 1)) {
        if (new.results[i,j] == 0) {
          config <- config
        } else {
          config <- paste(config, new.results[i,j], sep = sep)
        }
      }
      new.results[i,dim(new.results)[2]] <- config
    }
    if (diseases == 1) {
      final <- data.frame(N = as.numeric(new.results[,which(colnames(new.results) == "N")]),
                          config = new.results[,which(colnames(new.results) == new.label)],
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe")]), 4), nsmall = 4)),
                          PSp = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp")]), 4), nsmall = 4)),
                          PPPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV")]), 4), nsmall = 4)),
                          PNPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV")]), 4), nsmall = 4)),
                          row.names = NULL)
    } else if (diseases == 2) {
      final <- data.frame(N = as.numeric(new.results[,which(colnames(new.results) == "N")]),
                          config = new.results[,which(colnames(new.results) == new.label)],
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe1")]), 4), nsmall = 4)),
                          PSp1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp1")]), 4), nsmall = 4)),
                          PPPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV1")]), 4), nsmall = 4)),
                          PNPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV1")]), 4), nsmall = 4)),
                          PSe2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe2")]), 4), nsmall = 4)),
                          PSp2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp2")]), 4), nsmall = 4)),
                          PPPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV2")]), 4), nsmall = 4)),
                          PNPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV2")]), 4), nsmall = 4)),
                          row.names = NULL)
    }
  } else if (algorithm %in% c("D3", "ID3")) {
    for (i in 1:dim(new.results)[1]) {

      config <- paste(new.results[i,index.pools],
                      new.results[i,(index.pools + 1)], sep = sep)
      for (j in (index.pools + 2):(dim(new.results)[2] - 1)) {
        if (new.results[i,j] == 0) {
          config <- config
        } else {
          config <- paste(config, new.results[i,j], sep = sep)
        }
      }
      new.results[i,dim(new.results)[2]] <- config
    }
    if (diseases == 1) {
      final <- data.frame(I = as.numeric(new.results[,which(colnames(new.results) == "I")]),
                          config = new.results[,which(colnames(new.results) == new.label)],
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe")]), 4), nsmall = 4)),
                          PSp = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp")]), 4), nsmall = 4)),
                          PPPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV")]), 4), nsmall = 4)),
                          PNPV = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV")]), 4), nsmall = 4)),
                          row.names = NULL)
    } else if (diseases == 2) {
      final <- data.frame(I = as.numeric(new.results[,which(colnames(new.results) == "I")]),
                          config = new.results[,which(colnames(new.results) == new.label)],
                          ET = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "ET")]), 4), nsmall = 4)),
                          value = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "value")]), 4), nsmall = 4)),
                          PSe1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe1")]), 4), nsmall = 4)),
                          PSp1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp1")]), 4), nsmall = 4)),
                          PPPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV1")]), 4), nsmall = 4)),
                          PNPV1 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV1")]), 4), nsmall = 4)),
                          PSe2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSe2")]), 4), nsmall = 4)),
                          PSp2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PSp2")]), 4), nsmall = 4)),
                          PPPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PPPV2")]), 4), nsmall = 4)),
                          PNPV2 = as.numeric(format(round(as.numeric(new.results[,which(colnames(new.results) == "PNPV2")]), 4), nsmall = 4)),
                          row.names = NULL)
    }
  }
  final
}

#

