# Summary function for operatingCharacteristics1() and
#   operatingCharacteristics2()
###############################################################################

#' @title Summary method for operating characteristics results
#'
#' @description Produce a summary list for objects of class
#' \kbd{"opChar"} returned by \code{\link{operatingCharacteristics1}}
#' (\kbd{opChar1}) or \code{\link{operatingCharacteristics2}}
#' (\kbd{opChar2}).
#'
#' @param object an object of class \kbd{"opChar"}, providing the calculated
#' operating characteristics for a group testing algorithm.
#' @param ... currently not used.
#'
#' @details This function produces a summary list for objects of
#' class \kbd{"opChar"} returned by \code{\link{operatingCharacteristics1}}
#' (\kbd{opChar1}) or \code{\link{operatingCharacteristics2}}
#' (\kbd{opChar2}). It formats the testing configuration, expected number
#' of tests, expected number of tests per individual, and accuracy measures.
#'
#' The \kbd{Configuration} component of the result
#' gives the testing configuration, which may include the group sizes for
#' each stage of a hierarchical testing algorithm or the row/column size and
#' array size for an array testing algorithm. The \kbd{Tests} component
#' of the result gives the expected number of tests and the expected
#' number of tests per individual for the algorithm.
#'
#' The \kbd{Accuracy} component gives the individual accuracy measures for
#' each individual in \kbd{object} and the overall accuracy measures for the
#' algorithm. Accuracy measures included are the pooling sensitivity, pooling
#' specificity, pooling positive predictive value, and pooling negative
#' predictive value. The overall accuracy measures displayed are weighted
#' averages of the corresponding individual accuracy measures for all
#' individuals in the algorithm. Expressions for these averages are provided
#' in the Supplementary Material for Hitt et al. (2019). For more information,
#' see the Details' section for the \code{\link{operatingCharacteristics1}}
#' (\kbd{opChar1}) or \code{\link{operatingCharacteristics2}} (\kbd{opChar2})
#' function.
#'
#' @return \kbd{summary.opChar} returns an object of class
#' \kbd{"summary.opChar"}, a list containing:
#' \item{Algorithm}{character string specifying the name of the group testing
#' algorithm.}
#' \item{Configuration}{matrix detailing the configuration from \kbd{object}.
#' For hierarchical testing, this includes the group sizes for each stage of
#' testing. For array testing, this includes the array dimension (row/column
#' size) and the array size (the total number of individuals in the array).}
#' \item{Tests}{matrix detailing the expected number of tests and expected
#' number of tests per individual from \kbd{object}}.
#' \item{Accuracy}{a list containing:
#' \describe{
#' \item{Individual}{matrix detailing the accuracy measures for each
#' individual from \kbd{object} (for objects returned by \code{\link{opChar1}}).}
#' \item{Disease 1 Individual}{matrix detailing the accuracy measures
#' pertaining to disease 1 for each individual from \kbd{object}
#' (for objects returned by \code{\link{opChar2}}).}
#' \item{Disease 2 Individual}{matrix detailing the accuracy measures
#' pertaining to disease 2 for each individual from \kbd{object}
#' (for objects returned by \code{\link{opChar2}}).}
#' \item{Overall}{matrix detailing the overall accuracy measures for
#' the algorithm from \kbd{object}.}}}
#'
#' @author Brianna D. Hitt
#'
#' @seealso
#' \code{\link{operatingCharacteristics1}} (\kbd{opChar1}) and
#' \code{\link{operatingCharacteristics2}} (\kbd{opChar2}) for creating
#' an object of class \kbd{"opChar"}.
#'
#' @examples
#' # Calculate the operating characteristics for
#' #   non-informative four-stage hierarchical testing.
#' config.mat <- matrix(data = c(rep(1, 24), rep(1, 16),
#'                               rep(2, 8), rep(1, 8),
#'                               rep(2, 8), rep(3, 4),
#'                               rep(4, 2), rep(5, 2), 1:24),
#'                      nrow = 4, ncol = 24, byrow = TRUE)
#' calc1 <- opChar1(algorithm = "D4", p = 0.01,
#'                  Se = 0.99, Sp = 0.99,
#'                  hier.config = config.mat,
#'                  a = c(1, 9, 17, 21, 23))
#' summary(calc1)
#'
#' # Calculate the operating characteristics for
#' #   informative array testing without master pooling.
#' calc2 <- opChar1(algorithm = "IA2", p = 0.025, alpha = 0.5,
#'                  Se = 0.95, Sp = 0.99, rowcol.sz = 10)
#' summary(calc2)
#'
#' # Calculate the operating characteristics for
#' #   informative two-stage hierarchical testing
#' #   with a multiplex assay for two diseases.
#' config.mat <- matrix(data = c(rep(1, 5), rep(2, 4),
#'                               1, 1:10),
#'                      nrow = 2, ncol = 10, byrow = TRUE)
#' Se <- matrix(data = c(rep(0.95, 2), rep(0.99, 2)),
#'              nrow = 2, ncol = 2, byrow = FALSE)
#' Sp <- matrix(data = c(rep(0.96, 2), rep(0.98, 2)),
#'              nrow = 2, ncol = 2, byrow = FALSE)
#' calc3 <- opChar2(algorithm = "ID2",
#'                  alpha = c(18.25, 0.75, 0.75, 0.25),
#'                  Se = Se, Sp = Sp,
#'                  hier.config = config.mat)
#' summary(calc3)
#'
#' # Calculate the operating characteristics for
#' #   non-informative array testing with master pooling
#' #   with a multiplex assay for two diseases.
#' calc4 <- opChar2(algorithm = "A2M",
#'                  p.vec = c(0.92, 0.05, 0.02, 0.01),
#'                  Se = rep(0.95, 2), Sp = rep(0.99, 2),
#'                  rowcol.sz = 8)
#' summary(calc4)

summary.opChar <- function(object, ...) {

  # algorithm
  algorithm <- object$algorithm
  cat("\nAlgorithm:", algorithm, "\n\n")

  # configuration
  config <- object$Config
  # create a data frame detailing the configuration
  stage1 <- config[[1]]
  stage2 <- NULL
  stage3 <- NULL
  stage4 <- NULL
  if (length(config) > 1) {
    # three-stage and informative two-stage hierarchical testing
    if (length(config[[2]]) > 1) {
      stage2 <- config[[2]][1]
      for (i in 2:length(config[[2]])) {
        stage2 <- paste(stage2, config[[2]][i], sep = ",")
      }
    } else{
      # array testing and non-informative two-stage hierarchical testing
      stage2 <- config[[2]]
    }
  }
  # four-stage hierarchical testing
  if (length(config) > 2) {
    if (length(config[[3]]) > 1) {
      stage3 <- config[[3]][1]
      for (i in 2:length(config[[3]])) {
        stage3 <- paste(stage3, config[[3]][i], sep = ",")
      }
    } else{
      stage3 <- config[[3]]
    }
  }
  # five-stage hierarchical testing
  if (length(config) > 3) {
    if (length(config[[4]]) > 1) {
      stage4 <- config[[4]][1]
      for (i in 2:length(config[[4]])) {
        stage4 <- paste(stage4, config[[4]][i], sep = ",")
      }
    } else{
      stage4 <- config[[4]]
    }
  }
  config <- rbind(stage1, stage2, stage3, stage4)
  if (grepl("Informative two-stage", algorithm)) {
    dimnames(config) <- list(c("Block size", "Group sizes"), "")
  } else if (grepl("hierarchical", algorithm)) {
    dimnames(config) <- list(c("Stage 1", "Stage 2", "Stage 3",
                               "Stage 4")[1:length(config)], "")
  } else if (grepl("array", algorithm)) {
    dimnames(config) <- list(c("Row/column size", "Array size"), "")
  }

  cat("Testing configuration:\n")
  for (i in 1:length(config)) {
    cat(paste0(rownames(config)[i], ": ", config[i], "\n"))
  }

  ET <- format(round(object$ET, 2), nsmall = 2)
  value <- format(round(object$value, 4), nsmall = 4)
  # create a matrix detailing the expected number of tests
  tests <- matrix(data = c(ET, value), nrow = 2, ncol = 1)
  rownames(tests) <- c("Expected number of tests",
                       "Expected number of tests per individual")
  cat("\nExpected number of tests:", ET)
  cat("\nExpected number of tests per individual:", value)

  # create a matrix for accuracy measures
  if (inherits(object$Accuracy, "matrix")) {
    ind.acc <- NULL
    overall.acc <- as.data.frame(format(round(object$Accuracy, 4),
                                        nsmall = 4))
  } else if (length(object$Accuracy) == 2) {
    ind.acc <- object$Accuracy$Individual
    ind.acc[, 1:4] <- format(round(ind.acc[, 1:4], 4), nsmall = 4)
    colnames(ind.acc) <- c("PSe", "PSp", "PPPV", "PNPV", "Individuals")

    cat("\n\nAccuracy for individuals:\n")
    print(as.data.frame(ind.acc))

    overall.acc <- as.data.frame(format(round(object$Accuracy$Overall, 4),
                                        nsmall = 4))
  } else if (length(object$Accuracy) > 2) {
    ind.acc.dis1 <- object$Accuracy$'Disease 1 Individual'
    ind.acc.dis1[, 1:4] <- format(round(ind.acc.dis1[, 1:4], 4), nsmall = 4)
    colnames(ind.acc.dis1) <- c("PSe", "PSp", "PPPV", "PNPV", "Individuals")

    ind.acc.dis2 <- object$Accuracy$'Disease 2 Individual'
    ind.acc.dis2[, 1:4] <- format(round(ind.acc.dis2[, 1:4], 4), nsmall = 4)
    colnames(ind.acc.dis2) <- c("PSe", "PSp", "PPPV", "PNPV", "Individuals")

    cat("\n\nDisease 1 accuracy for individuals:\n")
    print(as.data.frame(ind.acc.dis1))
    cat("\nDisease 2 accuracy for individuals:\n")
    print(as.data.frame(ind.acc.dis2))

    overall.acc <- as.data.frame(format(round(object$Accuracy$Overall, 4),
                                        nsmall = 4))
  }
  colnames(overall.acc) <- c("PSe", "PSp", "PPPV", "PNPV")

  cat("\nOverall accuracy of the algorithm:\n")
  print(as.data.frame(overall.acc))

  cat("\nPSe denotes the pooling sensitivity.\n")
  cat("PSp denotes the pooling specificity.\n")
  cat("PPPV denotes the pooling positive predictive value.\n")
  cat("PNPV denotes the pooling negative predictive value.\n")

  if (inherits(object$Accuracy, "matrix") |
      length(object$Accuracy) == 2) {
    res <- list("Algorithm" = algorithm,
                "Configuration" = config,
                "Tests" = tests,
                "Accuracy" = list("Individual" = ind.acc,
                                  "Overall" = overall.acc))
  } else {
    res <- list("Algorithm" = algorithm,
                "Configuration" = config,
                "Tests" = tests,
                "Accuracy" = list("Disease 1 Individual" = ind.acc.dis1,
                                  "Disease 2 Individual" = ind.acc.dis2,
                                  "Overall" = overall.acc))
  }

  class(res) <- "summary.opChar"
  invisible(res)
}





# Print function
###############################################################################
# print.opChar() function

#' @title Print method for operating characteristics results
#'
#' @description Print method for objects of class \kbd{"opChar"} returned by
#' \code{\link{operatingCharacteristics1}} (\kbd{opChar1}) or
#' \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' @param x an object of class \kbd{"opChar"}, providing the calculated
#' operating characteristics for a group testing algorithm.
#' @param ... Additional arguments to be passed to \code{print} (e.g.,
#' \code{digits} to be passed to \code{round} for appropriate rounding).
#'
#' @return A print out of the algorithm, testing configuration, expected number
#' of tests, expected number of tests per individual, and accuracy measures
#' for individuals and for the overall algorithm.
#'
#' @author Brianna D. Hitt

print.opChar <- function(x, ...) {

  object <- x

  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  # algorithm
  algorithm <- object$algorithm
  cat("\nAlgorithm:", algorithm, "\n")

  # configuration
  cat("\nTesting configuration: \n")

  config <- object$Config
  # create a data frame detailing the configuration
  stage1 <- config[[1]]
  stage2 <- NULL
  stage3 <- NULL
  stage4 <- NULL
  if (length(config) == 1) {
    config.df <- data.frame("Stage1" = stage1)
  }
  if (length(config) > 1) {
    # three-stage and informative two-stage hierarchical testing
    if (length(config[[2]]) > 1) {
      stage2 <- config[[2]][1]
      for (i in 2:length(config[[2]])) {
        stage2 <- paste(stage2, config[[2]][i], sep = ",")
      }
      if ("Block.sz" %in% names(config)) {
        config.df <- data.frame("Block.sz" = stage1, "pool.szs" = stage2)
      } else {
        config.df <- data.frame("Stage1" = stage1, "Stage2" = stage2)
      }
    } else{
      # array testing and non-informative two-stage hierarchical testing
      stage2 <- config[[2]]
      if ("Array.dim" %in% names(config)) {
        config.df <- data.frame("Array.dim" = stage1, "Array.sz" = stage2)
      } else {
        config.df <- data.frame("Stage1" = stage1)
      }
    }
  }
  # four-stage hierarchical testing
  if (length(config) > 2) {
    if (length(config[[3]]) > 1) {
      stage3 <- config[[3]][1]
      for (i in 2:length(config[[3]])) {
        stage3 <- paste(stage3, config[[3]][i], sep = ",")
      }
    } else{
      stage3 <- config[[3]]
    }

    config.df <- data.frame("Stage1" = stage1, "Stage2" = stage2,
                            "Stage3" = stage3)
  }
  # five-stage hierarchical testing
  if (length(config) > 3) {
    if (length(config[[4]]) > 1) {
      stage4 <- config[[4]][1]
      for (i in 2:length(config[[4]])) {
        stage4 <- paste(stage4, config[[4]][i], sep = ",")
      }
    } else{
      stage4 <- config[[4]]
    }

    config.df <- data.frame("Stage1" = stage1, "Stage2" = stage2,
                            "Stage3" = stage3, "Stage4" = stage4)
  }
  print(as.data.frame(config.df))

  # operating characteristics
  ET <- format(round(object$ET, digits), nsmall = digits)
  value <- format(round(object$value, digits), nsmall = digits)
  cat("\nExpected number of tests:", ET)
  cat("\nExpected number of tests per individual:", value)

  # create a matrix for accuracy measures
  if (inherits(object$Accuracy, "matrix")) {
    ind.acc <- NULL
    overall.acc <- as.data.frame(format(round(object$Accuracy, digits),
                                        nsmall = digits))
  } else if (length(object$Accuracy) == 2) {
    ind.acc <- object$Accuracy$Individual
    ind.acc[, 1:4] <- format(round(ind.acc[, 1:4], digits), nsmall = digits)
    colnames(ind.acc) <- c("PSe", "PSp", "PPPV", "PNPV", "Individuals")

    cat("\n\nAccuracy for individuals:\n")
    print(as.data.frame(ind.acc))

    overall.acc <- as.data.frame(format(round(object$Accuracy$Overall, digits),
                                        nsmall = digits))
  } else if (length(object$Accuracy) > 2) {
    ind.acc.dis1 <- object$Accuracy$'Disease 1 Individual'
    ind.acc.dis1[, 1:4] <- format(round(ind.acc.dis1[, 1:4], digits),
                                  nsmall = digits)
    colnames(ind.acc.dis1) <- c("PSe", "PSp", "PPPV", "PNPV", "Individuals")

    ind.acc.dis2 <- object$Accuracy$'Disease 2 Individual'
    ind.acc.dis2[, 1:4] <- format(round(ind.acc.dis2[, 1:4], digits),
                                  nsmall = digits)
    colnames(ind.acc.dis2) <- c("PSe", "PSp", "PPPV", "PNPV", "Individuals")

    cat("\n\nDisease 1 accuracy for individuals:\n")
    print(as.data.frame(ind.acc.dis1))
    cat("\nDisease 2 accuracy for individuals:\n")
    print(as.data.frame(ind.acc.dis2))

    overall.acc <- as.data.frame(format(round(object$Accuracy$Overall, digits),
                                        nsmall = digits))
  }
  colnames(overall.acc) <- c("PSe", "PSp", "PPPV", "PNPV")

  cat("\nOverall accuracy of the algorithm:\n")
  print(as.data.frame(overall.acc))
}



# ExpTests function
##################################################################
# ExpTests.opChar() function                                     #
##################################################################

#' @title Extract the expected number of tests from testing configuration results
#'
#' @description Extract the expected number of tests and expected number of
#' tests per individual from objects of class "opchar" returned by
#' \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' @param object An object of class "opChar", from which the expected number
#' of tests and expected number of tests per individual are to be extracted.
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
#' @author Brianna D. Hitt and Christopher R. Bilder

#' @references
#' \insertRef{bilder2020tests}{binGroup2}
#'
#' @examples
#' config.mat <- matrix(data = c(rep(1, 10), 1:10),
#'                      nrow = 2, ncol = 10, byrow = TRUE)
#' res1 <- opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'                 hier.config = config.mat)
#' ExpTests(res1)
#'
#' res2 <- opChar2(algorithm = "A2M", p.vec = c(0.92, 0.05, 0.02, 0.01),
#'                 Se = rep(0.95, 2), Sp = rep(0.99, 2), rowcol.sz = 8)
#' ExpTests(res2)

ExpTests.opChar <- function(object, ...) {
  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  ExpTests <- format(round(object$ET, digits), nsmall = digits)
  ExpTestsPerIndividual <- format(round(object$value, digits), nsmall = digits)
  PercentReductionTests <- format(round(100*(1-object$value), 2), nsmall = 2)
  PercentIncreaseTestCap <- format(round(100*(1/object$value - 1), 2), nsmall = 2)

  res <- data.frame(ExpTests, ExpTestsPerIndividual, PercentReductionTests, PercentIncreaseTestCap)
  res
}



# Accuracy() function - This is not a method function but because it is used
#   with both opChar1() and opChar2() results, we decided to put it here.
##################################################################
# Accuracy() function                                            #
##################################################################

# Brianna Hitt - 04.03.2022
# Changed format(round()) to signif() for overall accuracy measures
#   because format(round()) was leading to conversion to strings.

#' @title Extract the accuracy measures from group testing results
#'
#' @description Extract the accuracy measures from objects of class
#' "opchar" returned by \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' @param object An object of class "opChar", from which the accuracy measures
#' are to be extracted.
#' @param individual A logical argument that determines whether the accuracy
#' measures for each individual (\kbd{individual=TRUE}) are to be included.
#' @param ... Additional arguments to be passed to \code{Accuracy} (e.g.,
#' \code{digits} to be passed to \code{round} or \code{signif} for appropriate
#' rounding).
#'
#' @details The \kbd{Accuracy} function gives the individual accuracy measures
#' for each individual in \kbd{object} and the overall accuracy measures for
#' the algorithm. If \kbd{individual=TRUE}, individual accuracy measures
#' are provided for each individual specified in the \code{a} argument of the
#' call to \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' Accuracy measures included are the pooling sensitivity, pooling
#' specificity, pooling positive predictive value, and pooling negative
#' predictive value. The overall accuracy measures displayed are weighted
#' averages of the corresponding individual accuracy measures for all
#' individuals in the algorithm. Expressions for these averages are provided
#' in the Supplementary Material for Hitt et al. (2019). For more information,
#' see the Details' section for the \code{\link{operatingCharacteristics1}}
#' (\kbd{opChar1}) or \code{\link{operatingCharacteristics2}} (\kbd{opChar2})
#' function.
#'
#' The rows in the matrices of individual accuracy measures correspond to each
#' unique set of accuracy measures in the algorithm. Individuals with the same
#' set of accuracy measures are displayed together in a single row of the
#' matrix. The columns correspond to the pooling sensitivity, pooling
#' specificity, pooling positive predictive value, pooling negative predictive
#' value, and the indices for the individuals in each row of the matrix.
#' Individual accuracy measures are provided only if \kbd{individual=TRUE}.
#'
#' @return A list containing:
#' \item{Individual}{matrix detailing the accuracy measures for each individual
#' from \kbd{object} (for objects returned by \code{\link{opChar1}}).}
#' \item{Disease 1 Individual}{matrix detailing the accuracy measures
#' pertaining to disease 1 for each individual from \kbd{object}
#' (for objects returned by \code{\link{opChar2}}).}
#' \item{Disease 2 Individual}{matrix detailing the accuracy measures
#' pertaining to disease 2 for each individual from \kbd{object}
#' (for objects returned by \code{\link{opChar2}}).}
#' \item{Overall}{matrix detailing the overall accuracy measures for
#' the algorithm from \kbd{object}.}
#'
#' @author Brianna D. Hitt
#'
#' @examples
#' config.mat <- matrix(data = c(rep(1, 10), 1:10),
#'                      nrow = 2, ncol = 10, byrow = TRUE)
#' res1 <- opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'         hier.config = config.mat)
#' Accuracy(res1, individual = FALSE)
#' Accuracy(res1, individual = TRUE)
#'
#' res2 <- opChar2(algorithm = "A2M",
#'                 p.vec = c(0.92, 0.05, 0.02, 0.01),
#'                 Se = rep(0.95, 2), Sp = rep(0.99, 2),
#'                 rowcol.sz = 8)
#' Accuracy(res2)

Accuracy <- function(object, individual = TRUE, ...) {

  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  if (isFALSE(individual)) {
    list("Overall" = signif(object$Accuracy$Overall, digits))
  } else {
    if (length(object$Accuracy) > 2) {
      dis1 <- object$Accuracy$'Disease 1 Individual'
      dis2 <- object$Accuracy$'Disease 2 Individual'
      list("Disease 1 Individual" =
             data.frame(PSe = format(round(dis1[,1], digits), nsmall = digits),
                        PSp = format(round(dis1[,2], digits), nsmall = digits),
                        PPPV = format(round(dis1[,3], digits), nsmall = digits),
                        PNPV = format(round(dis1[,4], digits), nsmall = digits),
                        individuals = dis1[,5]),
           "Disease 2 Individual" =
             data.frame(PSe = format(round(dis2[,1], digits), nsmall = digits),
                        PSp = format(round(dis2[,2], digits), nsmall = digits),
                        PPPV = format(round(dis2[,3], digits), nsmall = digits),
                        PNPV = format(round(dis2[,4], digits), nsmall = digits),
                        individuals = dis2[,5]),
           "Overall" = signif(object$Accuracy$Overall, digits))
    } else {
      res <- object$Accuracy$Individual
      list("Individual" =
             data.frame(PSe = format(round(res[,1], digits), nsmall = digits),
                        PSp = format(round(res[,2], digits), nsmall = digits),
                        PPPV = format(round(res[,3], digits), nsmall = digits),
                        PNPV = format(round(res[,4], digits), nsmall = digits),
                        individuals = res[,5]),
           "Overall" = signif(object$Accuracy$Overall, digits))
    }
  }
}




# Config function
##################################################################
# Config.opChar() function                                       #
##################################################################

#' @title Extract the testing configuration from group testing results
#'
#' @description Extract the testing configuration from objects of class
#' "opchar" returned by \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' @param object An object of class "opChar", from which the testing
#' configuration is to be extracted.
#' @param ... currently not used.
#'
#' @return A data frame specifying elements of the testing configuration.
#'
#' @author Brianna D. Hitt
#'
#' @examples
#' config.mat <- matrix(data = c(rep(1, 10), 1:10),
#'                      nrow = 2, ncol = 10, byrow = TRUE)
#' res1 <- opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'         hier.config = config.mat)
#' Config(res1)
#'
#' config.mat <- matrix(data = c(rep(1, 20), rep(1, 10), rep(2, 10),
#'                              rep(c(1, 2, 3, 4), each = 5),
#'                              rep(1, 3), rep(2, 2), rep(3, 3),
#'                              rep(4, 2), rep(5, 3), rep(6, 2),
#'                              rep(7, 3), rep(8, 2), 1:20),
#'                     nrow = 5, ncol = 20, byrow = TRUE)
#' Se <- matrix(data = rep(0.95, 10), nrow = 2, ncol = 5,
#'              dimnames = list(Infection = 1:2, Stage = 1:5))
#' Sp <- matrix(data = rep(0.99, 10), nrow = 2, ncol = 5,
#'              dimnames = list(Infection = 1:2, Stage = 1:5))
#' res2 <- opChar2(algorithm = "ID5",
#'                 alpha = c(18.25, 0.75, 0.75, 0.25),
#'                 Se = Se, Sp = Sp, hier.config = config.mat)
#' Config(res2)

Config.opChar <- function(object, ...) {
  config <- object$Config
  # create a data frame detailing the configuration
  stage1 <- config[[1]]
  stage2 <- NULL
  stage3 <- NULL
  stage4 <- NULL
  if (length(config) == 1) {
    config.df <- data.frame("Stage1" = stage1)
  }
  if (length(config) > 1) {
    # three-stage and informative two-stage hierarchical testing
    if (length(config[[2]]) > 1) {
      stage2 <- config[[2]][1]
      for (i in 2:length(config[[2]])) {
        stage2 <- paste(stage2, config[[2]][i], sep = ",")
      }
      if ("Block.sz" %in% names(config)) {
        config.df <- data.frame("Block.sz" = stage1, "pool.szs" = stage2)
      } else {
        config.df <- data.frame("Stage1" = stage1, "Stage2" = stage2)
      }
    } else{
      # array testing and non-informative two-stage hierarchical testing
      stage2 <- config[[2]]
      if ("Array.dim" %in% names(config)) {
        config.df <- data.frame("Array.dim" = stage1, "Array.sz" = stage2)
      } else {
        config.df <- data.frame("Stage1" = stage1)
      }
    }
  }
  # four-stage hierarchical testing
  if (length(config) > 2) {
    if (length(config[[3]]) > 1) {
      stage3 <- config[[3]][1]
      for (i in 2:length(config[[3]])) {
        stage3 <- paste(stage3, config[[3]][i], sep = ",")
      }
    } else{
      stage3 <- config[[3]]
    }

    config.df <- data.frame("Stage1" = stage1, "Stage2" = stage2,
                            "Stage3" = stage3)
  }
  # five-stage hierarchical testing
  if (length(config) > 3) {
    if (length(config[[4]]) > 1) {
      stage4 <- config[[4]][1]
      for (i in 2:length(config[[4]])) {
        stage4 <- paste(stage4, config[[4]][i], sep = ",")
      }
    } else{
      stage4 <- config[[4]]
    }

    config.df <- data.frame("Stage1" = stage1, "Stage2" = stage2,
                            "Stage3" = stage3, "Stage4" = stage4)
  }
  print(as.data.frame(config.df))
}







# IndProb function
##################################################################
# IndProb.opChar() function                                      #
##################################################################

#' @title Extract the individual probabilities used to calculate group testing
#' results
#'
#' @description Extract the individual probabilities from objects of class
#' "opchar" returned by \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' @param object An object of class "opChar", from which the individual
#' probabilities are to be extracted.
#' @param ... Additional arguments to be passed to \code{IndProb} (e.g.,
#' \code{digits} to be passed to \code{signif} for appropriate
#' rounding).
#'
#' @return Either \kbd{p.vec}, the sorted vector of individual probabilities
#' (for hierarchical group testing algorithms) or \kbd{p.mat}, the sorted
#' matrix of individual probabilities in gradient arrangement (for array
#' testing algorithms). Further details are given under the 'Details' section
#' for the \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}) functions.
#'
#' @author Brianna D. Hitt
#'
#' @examples
#' config.mat <- matrix(data = c(rep(1, 10), 1:10),
#'                      nrow = 2, ncol = 10, byrow = TRUE)
#' res1 <- opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'         hier.config = config.mat)
#' IndProb(res1)
#'
#' config.mat <- matrix(data = c(rep(1, 20), rep(1, 10), rep(2, 10),
#'                              rep(c(1, 2, 3, 4), each = 5),
#'                              rep(1, 3), rep(2, 2), rep(3, 3),
#'                              rep(4, 2), rep(5, 3), rep(6, 2),
#'                              rep(7, 3), rep(8, 2), 1:20),
#'                     nrow = 5, ncol = 20, byrow = TRUE)
#' Se <- matrix(data = rep(0.95, 10), nrow = 2, ncol = 5,
#'              dimnames = list(Infection = 1:2, Stage = 1:5))
#' Sp <- matrix(data = rep(0.99, 10), nrow = 2, ncol = 5,
#'              dimnames = list(Infection = 1:2, Stage = 1:5))
#' res2 <- opChar2(algorithm = "ID5",
#'                 alpha = c(18.25, 0.75, 0.75, 0.25),
#'                 Se = Se, Sp = Sp, hier.config = config.mat)
#' IndProb(res2)

IndProb <- function(object, ...) {
  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  if (!is.null(object$p.vec)) {
    signif(object$p.vec, digits)
  } else if (!is.null(object$p.mat)) {
    signif(object$p.mat, digits)
  }
}



# CompareConfig() function - This is not a method function but because it is used
#   with both opChar1() and opChar2() results, we decided to put it here.
##################################################################
# CompareConfig() function                                      #
##################################################################

#' @title Compare group testing results
#'
#' @description Compare group testing results from objects of class
#' "opchar" returned by \code{\link{operatingCharacteristics1}} (\kbd{opChar1})
#' or \code{\link{operatingCharacteristics2}} (\kbd{opChar2}).
#'
#' @param object1 An object of class "opChar" containing group testing results.
#' @param object2 A second object of class "opChar" containing group
#' testing results.
#'
#' @details The \code{CompareConfig} function compares group testing results from
#' two objects of class "opChar". The function creates a
#' data frame with these comparisons.
#'
#' @return A data frame with the expected percent reduction in tests (PercentReductionTests) and the expected
#' increase in testing capacity (PercentIncreaseTestCap) when using the second testing configuration rather than
#' the first testing configuration. Positive values for these quantities
#' indicate that the second testing configuration is more efficient than the first.

#'
#' @author Brianna D. Hitt and Christopher R. Bilder
#'
#' @examples
#' config.mat1 <- matrix(data = c(rep(1, 10), rep(1:2, each = 5), 1:10),
#'                       nrow = 3, ncol = 10, byrow = TRUE)
#' res1 <- opChar1(algorithm = "D3", p = 0.05, Se = 0.99, Sp = 0.99,
#'                 hier.config = config.mat1)
#' config.mat2 <- matrix(data = c(rep(1, 10), 1:10),
#'                       nrow = 2, ncol = 10, byrow = TRUE)
#' res2 <- opChar1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'         hier.config = config.mat2)
#' CompareConfig(res2, res1)
#'
#' config.mat3 <- matrix(data = c(rep(1, 10), rep(1, 5),
#'                                rep(2, 4), 3, 1:9, NA),
#'                       nrow = 3, ncol = 10, byrow = TRUE)
#' Se <- matrix(data = rep(0.95, 6), nrow = 2, ncol = 3,
#'              dimnames = list(Infection = 1:2, Stage = 1:3))
#' Sp <- matrix(data = rep(0.99, 6), nrow = 2, ncol = 3,
#'              dimnames = list(Infection = 1:2, Stage = 1:3))
#' res3 <- opChar2(algorithm = "D3", p.vec = c(0.95, 0.02, 0.02, 0.01),
#'                 Se = Se, Sp = Sp, hier.config = config.mat3)
#' config.mat4 <- matrix(data = c(rep(1, 12), rep(1, 6), rep(2, 6),
#'                                rep(1, 4), rep(2, 2), rep(3, 3),
#'                                rep(4, 3), 1:12),
#'                     nrow = 4, ncol = 12, byrow = TRUE)
#' Se <- matrix(data = rep(0.95, 8), nrow = 2, ncol = 4,
#'              dimnames = list(Infection = 1:2, Stage = 1:4))
#' Sp <- matrix(data = rep(0.99, 8), nrow = 2, ncol = 4,
#'              dimnames = list(Infection = 1:2, Stage = 1:4))
#' res4 <- opChar2(algorithm = "D4", p.vec = c(0.92, 0.05, 0.02, 0.01),
#'                 Se = Se, Sp = Sp, hier.config = config.mat4)
#' CompareConfig(res4, res3)

CompareConfig <- function(object1, object2) {

   ET <- rbind(ExpTests.opChar(object1), ExpTests.opChar(object2))
   cat("Testing configurations compared to individual testing: \n")
   print(ET)
   cat("\n")

   ReductionTests <- (object1$value - object2$value) / object1$value
   PercentReductionTests <- format(round(ReductionTests * 100,
      2), nsmall = 2)
   PercentIncreaseTestCap <- format(round((1/(object2$value/object1$value) - 1) * 100,
     2), nsmall = 2)

   cat("Percent reduction in tests when using the second testing
     configuration rather than the first:", PercentReductionTests, "\n \n")
   cat("Percent increase in testing capacity when using the second testing
     configuration rather than the first:", PercentIncreaseTestCap)

   save.res <- data.frame(PercentReductionTests, PercentIncreaseTestCap)
   cat("\n \n")

   invisible(save.res)
}




