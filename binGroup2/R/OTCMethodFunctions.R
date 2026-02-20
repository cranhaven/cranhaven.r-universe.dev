# Summary function for OTC1() and OTC2()
###############################################################################
#' @title Summary method for optimal testing configuration results
#'
#' @description Produce a summary list for objects of class \kbd{"OTC"}
#' returned by \code{\link{OTC1}} or \code{\link{OTC2}}.
#'
#' @param object an object of class \kbd{"OTC"}, providing the optimal testing
#' configuration and associated operating characteristics for a group testing
#' algorithm.
#' @param ... currently not used.
#'
#' @details This function produces a summary list for objects of class
#' \kbd{"OTC"} returned by \code{\link{OTC1}} or \code{\link{OTC2}}.
#' It formats the optimal testing configuration, expected number of tests,
#' expected number of tests per individual, and accuracy measures.
#' A summary of the results from \code{\link{OTC1}} includes results for all
#' objective functions specified by the user.
#'
#' The \kbd{OTC} component of the result gives the optimal testing
#' configuration, which may include the group sizes for each stage of a
#' hierarchical testing algorithm or the row/column size and array size for an
#' array testing algorithm. The \kbd{Tests} component of the result gives the
#' expected number of tests and the expected number of tests per individual
#' for the algorithm.
#'
#' The \kbd{Accuracy} component gives the overall accuracy measures for the
#' algorithm. Accuracy measures included are the pooling sensitivity, pooling
#' specificity, pooling positive predictive value, and pooling negative
#' predictive value. These values are weighted averages of the corresponding
#' individual accuracy measures for all individuals in the algorithm.
#' Expressions for these averages are provided in the Supplementary Material
#' for Hitt et al. (2019). For more information, see the 'Details' section for
#' the \code{\link{OTC1}} or \code{\link{OTC2}} function.
#'
#' @return \kbd{summary.OTC} returns an object of class \kbd{"summary.OTC"},
#' a list containing:
#' \item{Algorithm}{character string specifying the name of the group testing
#' algorithm.}
#' \item{OTC}{matrix detailing the optimal testing configuration from
#' \kbd{object}. For hierarchical testing, this includes the group sizes for
#' each stage of testing. For array testing, this includes the array dimension
#' (row/column size) and the array size (the total number of individuals
#' in the array).}
#' \item{Tests}{matrix detailing the expected number of tests and expected
#' number of tests per individual from \kbd{object}.}
#' \item{Accuracy}{matrix detailing the overall accuracy measures for the
#' algorithm, including the pooling sensitivity, pooling specificity,
#' pooling positive predictive value, and pooling negative predictive value
#' for the algorithm from \kbd{object}. Further details are found in the
#' 'Details' section.}
#'
#' @author Brianna D. Hitt
#'
#' @seealso
#' \code{\link{OTC1}} and \code{\link{OTC2}}
#' for creating an object of class \kbd{"OTC"}.
#'
#' @examples
#' # Find the optimal testing configuration for
#' #   non-informative two-stage hierarchical testing.
#' res1 <- OTC1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99,
#'              group.sz = 2:100, obj.fn = c("ET", "MAR", "GR1"),
#'              weights = matrix(data = c(1,1), nrow = 1, ncol = 2))
#' summary(res1)
#'
#' # Find the optimal testing configuration for
#' #   informative three-stage hierarchical testing
#' res2 <- OTC1(algorithm = "ID3", p = 0.025,
#'              Se = c(0.95, 0.95, 0.99), Sp = c(0.96, 0.96, 0.98),
#'              group.sz = 3:10, obj.fn = c("ET", "MAR"), alpha = 2)
#' summary(res2)
#'
#' # Find the optimal testing configuration for
#' #   informative array testing without master pooling.
#' \donttest{res3 <- OTC1(algorithm = "IA2", p = 0.05, alpha = 2,
#'              Se = 0.90, Sp = 0.90, group.sz = 2:15,
#'              obj.fn = "ET")
#' summary(res3)}
#'
#' # Find the optimal testing configuraiton for
#' #   informative two-stage hierarchical testing.
#' Se <- matrix(data = c(rep(0.95, 2), rep(0.99, 2)),
#'              nrow = 2, ncol = 2, byrow = FALSE)
#' Sp <- matrix(data = c(rep(0.96, 2), rep(0.98, 2)),
#'              nrow = 2, ncol = 2, byrow = FALSE)
#' res4 <- OTC2(algorithm = "ID2",
#'              alpha = c(18.25, 0.75, 0.75, 0.25),
#'              Se = Se, Sp = Sp, group.sz = 8)
#' summary(res4)
#'
#' # Find the optimal testing configuration for
#' #   non-informative three-stage hierarchical testing.
#' \donttest{Se <- matrix(data = c(rep(0.95, 6)), nrow = 2, ncol = 3)
#' Sp <- matrix(data = c(rep(0.99, 6)), nrow = 2, ncol = 3)
#' res5 <- OTC2(algorithm = "D3",
#'              p.vec = c(0.95, 0.0275, 0.0175, 0.005),
#'              Se = Se, Sp = Sp, group.sz = 5:12)
#' summary(res5)}
#'
#' # Find the optimal testing configuration for
#' #   non-informative array testing with master pooling.
#' \donttest{res6 <- OTC2(algorithm = "A2M", p.vec = c(0.90, 0.04, 0.04, 0.02),
#'              Se = rep(0.99, 2), Sp = rep(0.99, 2), group.sz = 2:12)
#' summary(res6)}

summary.OTC <- function(object, ...) {

  # algorithm
  algorithm <- object$algorithm
  cat("\nAlgorithm:", algorithm, "\n\n")

  # extract the results for all objective functions
  #   some of these may be NULL if not requested by the user
  opt.ET <- object$opt.ET
  opt.MAR <- object$opt.MAR
  opt.GR1 <- object$opt.GR1
  opt.GR2 <- object$opt.GR2
  opt.GR3 <- object$opt.GR3
  opt.GR4 <- object$opt.GR4
  opt.GR5 <- object$opt.GR5
  opt.GR6 <- object$opt.GR6

  all.objfns <- c("ET", "MAR", "GR1", "GR2", "GR3", "GR4", "GR5", "GR6")
  objfn.labels <- all.objfns[paste0("opt.", all.objfns) %in% names(object)]

  # create a matrix detailing the optimal configurations for each obj. fn
  stage1 <- c(opt.ET$OTC[[1]], opt.MAR$OTC[[1]], opt.GR1$OTC[[1]],
              opt.GR2$OTC[[1]], opt.GR3$OTC[[1]], opt.GR4$OTC[[1]],
              opt.GR5$OTC[[1]], opt.GR6$OTC[[1]])

  if (length(opt.ET$OTC) > 1) {
    if (length(opt.ET$OTC[[2]]) > 1) {
      # concatenate pool sizes for ET objective function
      stage2.ET <- opt.ET$OTC[[2]][1]
      for (i in 2:length(opt.ET$OTC[[2]])) {
        stage2.ET <- paste(stage2.ET, opt.ET$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for MAR objective function
      stage2.MAR <- opt.MAR$OTC[[2]][1]
      for (i in 2:length(opt.MAR$OTC[[2]])) {
        stage2.MAR <- paste(stage2.MAR, opt.MAR$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for GR1 objective function
      stage2.GR1 <- opt.GR1$OTC[[2]][1]
      for (i in 2:length(opt.GR1$OTC[[2]])) {
        stage2.GR1 <- paste(stage2.GR1, opt.GR1$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for GR2 objective function
      stage2.GR2 <- opt.GR2$OTC[[2]][1]
      for (i in 2:length(opt.GR2$OTC[[2]])) {
        stage2.GR2 <- paste(stage2.GR2, opt.GR2$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for GR3 objective function
      stage2.GR3 <- opt.GR3$OTC[[2]][1]
      for (i in 2:length(opt.GR3$OTC[[2]])) {
        stage2.GR3 <- paste(stage2.GR3, opt.GR3$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for GR4 objective function
      stage2.GR4 <- opt.GR4$OTC[[2]][1]
      for (i in 2:length(opt.GR4$OTC[[2]])) {
        stage2.GR4 <- paste(stage2.GR4, opt.GR4$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for GR5 objective function
      stage2.GR5 <- opt.GR5$OTC[[2]][1]
      for (i in 2:length(opt.GR5$OTC[[2]])) {
        stage2.GR5 <- paste(stage2.GR5, opt.GR5$OTC[[2]][i], sep = ",")
      }
      # concatenate pool sizes for GR6 objective function
      stage2.GR6 <- opt.GR6$OTC[[2]][1]
      for (i in 2:length(opt.GR6$OTC[[2]])) {
        stage2.GR6 <- paste(stage2.GR6, opt.GR6$OTC[[2]][i], sep = ",")
      }

      stage2 <- c(stage2.ET, stage2.MAR, stage2.GR1, stage2.GR2,
                  stage2.GR3, stage2.GR4, stage2.GR5, stage2.GR6)
    } else {
      stage2  <- c(opt.ET$OTC[[2]], opt.MAR$OTC[[2]], opt.GR1$OTC[[2]],
                   opt.GR2$OTC[[2]], opt.GR3$OTC[[2]], opt.GR4$OTC[[2]],
                   opt.GR5$OTC[[2]], opt.GR6$OTC[[2]])
    }
  }
  # four-stage hierarchical testing
  # if (length(opt.ET$OTC) > 2) {
  #   if (length(opt.ET$OTC[[3]]) > 1) {
  #     # concatenate pool sizes for ET objective function
  #     stage3.ET <- opt.ET$OTC[[3]][1]
  #     for (i in 2:length(opt.ET$OTC[[3]])) {
  #       stage3.ET <- paste(stage3.ET, opt.ET$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for MAR objective function
  #     stage3.MAR <- opt.MAR$OTC[[3]][1]
  #     for (i in 2:length(opt.MAR$OTC[[3]])) {
  #       stage3.MAR <- paste(stage3.MAR, opt.MAR$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for GR1 objective function
  #     stage3.GR1 <- opt.GR1$OTC[[3]][1]
  #     for (i in 2:length(opt.GR1$OTC[[3]])) {
  #       stage3.GR1 <- paste(stage3.GR1, opt.GR1$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for GR2 objective function
  #     stage3.GR2 <- opt.GR2$OTC[[3]][1]
  #     for (i in 2:length(opt.GR2$OTC[[3]])) {
  #       stage3.GR2 <- paste(stage3.GR2, opt.GR2$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for GR3 objective function
  #     stage3.GR3 <- opt.GR3$OTC[[3]][1]
  #     for (i in 2:length(opt.GR3$OTC[[3]])) {
  #       stage3.GR3 <- paste(stage3.GR3, opt.GR3$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for GR4 objective function
  #     stage3.GR4 <- opt.GR4$OTC[[3]][1]
  #     for (i in 2:length(opt.GR4$OTC[[3]])) {
  #       stage3.GR4 <- paste(stage3.GR4, opt.GR4$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for GR5 objective function
  #     stage3.GR5 <- opt.GR5$OTC[[3]][1]
  #     for (i in 2:length(opt.GR5$OTC[[3]])) {
  #       stage3.GR5 <- paste(stage3.GR5, opt.GR5$OTC[[3]][i], sep = ",")
  #     }
  #     # concatenate pool sizes for GR6 objective function
  #     stage3.GR6 <- opt.GR6$OTC[[3]][1]
  #     for (i in 2:length(opt.GR6$OTC[[3]])) {
  #       stage3.GR6 <- paste(stage3.GR6, opt.GR6$OTC[[3]][i], sep = ",")
  #     }
  #
  #     stage3 <- c(stage3.ET, stage3.MAR, stage3.GR1, stage3.GR2,
  #                 stage3.GR3, stage3.GR4, stage3.GR5, stage3.GR6)
  #   } else {
  #     stage3 <- c(opt.ET$OTC[[3]], opt.MAR$OTC[[3]], opt.GR1$OTC[[3]],
  #                  opt.GR2$OTC[[3]], opt.GR3$OTC[[3]], opt.GR4$OTC[[3]],
  #                  opt.GR5$OTC[[3]], opt.GR6$OTC[[3]])
  #   }
  # }

  # columns correspond to objective functions
  # config <- rbind(stage1, stage2)
  # rownames(config) <- stage.labels
  # colnames(config) <- objfn.labels

  # rows correspond to objective functions
  if (grepl("Non-informative two-stage", algorithm)) {
    config <- matrix(data = stage1, nrow = length(objfn.labels), ncol = 1)
  } else {
    config <- cbind(stage1, stage2)
  }
  if (grepl("Informative two-stage", algorithm)) {
    stage.labels <- c("Block size", "Group sizes")
  } else if (grepl("hierarchical", algorithm)) {
    # stage.labels <- c("Stage 1", "Stage 2")[1:nrow(config)]
    stage.labels <- c("Stage 1", "Stage 2")[1:ncol(config)]
  } else if (grepl("array", algorithm)) {
    stage.labels <- c("Row/column size", "Array size")
  }
  rownames(config) <- objfn.labels
  colnames(config) <- stage.labels

  cat("Optimal testing configuration:\n")
  print(as.data.frame(config))

  # create a matrix detailing the expected number of tests for each obj. fn
  ExpT <- c(opt.ET$ET, opt.MAR$ET, opt.GR1$ET, opt.GR2$ET,
            opt.GR3$ET, opt.GR4$ET, opt.GR5$ET, opt.GR6$ET)
  value <- c(opt.ET$value, opt.MAR$value, opt.GR1$value, opt.GR2$value,
             opt.GR3$value, opt.GR4$value, opt.GR5$value, opt.GR6$value)

  # columns correspond to objective functions
  # tests <- rbind(ExpT, value)
  # rownames(tests) <- c("Expected number of tests",
  #                      "Objective function value per individual")
  # colnames(tests) <- objfn.labels

  # rows correspond to objective functions
  tests <- cbind(format(round(ExpT, 2), nsmall = 2),
                 format(round(value, 4), nsmall = 4))
  rownames(tests) <- objfn.labels
  colnames(tests) <- c("E(T)", "Value")

  cat("\nExpected number of tests:\n")
  print(as.data.frame(tests))
  cat("\nE(T) denotes the expected number of tests.\n")
  cat("Value denotes the objective function value per individual.\n\n")

  # create a matrix detailing the accuracy measures for each obj. fn
  if (dim(opt.ET$Accuracy)[1] == 1) {
    overall.acc <- rbind(opt.ET$Accuracy, opt.MAR$Accuracy, opt.GR1$Accuracy,
                         opt.GR2$Accuracy, opt.GR3$Accuracy, opt.GR4$Accuracy,
                         opt.GR5$Accuracy, opt.GR6$Accuracy)
    overall.acc <- format(round(overall.acc, 4), nsmall = 4)
    rownames(overall.acc) <- objfn.labels
    colnames(overall.acc) <- c("PSe", "PSp", "PPPV", "PNPV")

    cat("Overall accuracy of the algorithm:\n")
    print(as.data.frame(overall.acc))
  } else if (dim(opt.ET$Accuracy)[1] == 2) {
    # overall.acc1 <- rbind(opt.ET$Accuracy[1,], opt.MAR$Accuracy[1,],
    #                       opt.GR1$Accuracy[1,], opt.GR2$Accuracy[1,],
    #                       opt.GR3$Accuracy[1,], opt.GR4$Accuracy[1,],
    #                       opt.GR5$Accuracy[1,], opt.GR6$Accuracy[1,])
    # overall.acc1 <- format(round(overall.acc1, 4), nsmall = 4)
    # rownames(overall.acc1) <- objfn.labels
    # colnames(overall.acc1) <- c("PSe", "PSp", "PPPV", "PNPV")
    #
    # overall.acc2 <- rbind(opt.ET$Accuracy[2,], opt.MAR$Accuracy[2,],
    #                       opt.GR1$Accuracy[2,], opt.GR2$Accuracy[2,],
    #                       opt.GR3$Accuracy[2,], opt.GR4$Accuracy[2,],
    #                       opt.GR5$Accuracy[2,], opt.GR6$Accuracy[2,])
    # overall.acc2 <- format(round(overall.acc2, 4), nsmall = 4)
    # rownames(overall.acc2) <- objfn.labels
    # colnames(overall.acc2) <- c("PSe", "PSp", "PPPV", "PNPV")
    overall.acc <- as.data.frame(format(round(object$opt.ET$Accuracy, 4),
                                        nsmall = 4))

    # cat("Overall accuracy of the algorithm for disease 1:\n")
    # print(as.data.frame(overall.acc1))
    # cat("Overall accuracy of the algorithm for disease 2:\n")
    # print(as.data.frame(overall.acc2))
    cat("Overall accuracy of the algorithm:\n")
    print(as.data.frame(overall.acc))
  }

  cat("\nPSe denotes the pooling sensitivity.\n")
  cat("PSp denotes the pooling specificity.\n")
  cat("PPPV denotes the pooling positive predictive value.\n")
  cat("PNPV denotes the pooling negative predictive value.\n")

  # if (dim(opt.ET$Accuracy)[1] == 1) {
  #   res <- list("Algorithm" = algorithm,
  #               "Configuration" = config,
  #               "Tests" = tests,
  #               "Accuracy" = overall.acc)
  # } else {
  #   res <- list("Algorithm" = algorithm,
  #               "Configuration" = config,
  #               "Tests" = tests,
  #               "Accuracy" = list("Disease 1" = overall.acc1,
  #                                 "Disease 2" = overall.acc2))
  # }
  res <- list("Algorithm" = algorithm,
              "Configuration" = config,
              "Tests" = tests,
              "Accuracy" = overall.acc)

  class(res) <- "summary.OTC"
  invisible(res)
}




# Plotting function for OTC1() and OTC2()
###############################################################################
#' @title Plot method for optimal testing configuration results
#'
#' @description Produce a plot for objects of class \kbd{"OTC"}
#' returned by \code{\link{OTC1}} or \code{\link{OTC2}}.
#'
#' @param x an object of class \kbd{"OTC"}, providing operating
#' characteristics for the optimal testing configuration and similar
#' configurations for a group testing algorithm.
#' @param ... currently not used.
#'
#' @details This function produces a plot for objects of class \kbd{"OTC"}
#' returned by \code{\link{OTC1}} or \code{\link{OTC2}}. It plots the expected
#' number of tests per individual for each similar testing configuration
#' in the object.
#'
#' In addition to the OTC, the \code{\link{OTC1}} and \code{\link{OTC2}}
#' functions provide operating characteristics for other configurations
#' corresponding to each initial group size provided by the user. For
#' algorithms where there is only one configuration for each initial group size
#' (non-informative two-stage hierarchical and all array testing algorithms),
#' results for each initial group size are plotted. For algorithms where there
#' is more than one possible configuration for each initial group size
#' (informative two-stage hierarchical and all three-stage hierarchical
#' algorithms), the results corresponding to the best configuration for each
#' initial group size are plotted.
#'
#' If a single value is provided for the \kbd{group.sz} argument in the
#' \code{\link{OTC1}} or \code{\link{OTC2}} functions, no plot will be
#' produced.
#'
#' The plot is produced using the \code{ggplot2} package. Customization
#' features from \code{ggplot2} are available once the package is loaded.
#' Examples are shown in the 'Examples' section.
#'
#' @return A plot of the expected number of tests per individual for similar
#' configurations provided in the object.
#'
#' @author Brianna D. Hitt
#'
#' @seealso
#' \code{\link{OTC1}} and \code{\link{OTC2}} for creating an object of class
#' \kbd{"OTC"}.
#'
#' @examples
#'
#' # Find the optimal testing configuration for
#' #   non-informative two-stage hierarchical testing.
#' res1 <- OTC1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99,
#'              group.sz = 3:100, obj.fn = c("ET", "MAR", "GR1"),
#'              weights = matrix(data = c(1, 1), nrow = 1, ncol = 2))
#' plot(res1)
#'
#' # Customize the plot using the ggplot2 package.
#' library(ggplot2)
#' plot(res1) + ylim(0,1) +
#'   ggtitle("Similar configurations for Dorfman testing") +
#'   theme(plot.title = element_text(hjust = 0.5))
#'
#' # Find the optimal testing configuration for
#' #   informative three-stage hierarchical testing
#' res2 <- OTC1(algorithm = "ID3", p = 0.025,
#'              Se = c(0.95, 0.95, 0.99), Sp = c(0.96, 0.96, 0.98),
#'              group.sz = 3:15, obj.fn = "ET", alpha = 2)
#' plot(res2)
#'
#' # Find the optimal testing configuration for
#' #   informative array testing without master pooling.
#' \donttest{res3 <- OTC1(algorithm = "IA2", p = 0.09, alpha = 2,
#'              Se = 0.90, Sp = 0.90, group.sz = 3:20, obj.fn = "ET")
#' plot(res3)}
#'
#' # Find the optimal testing configuration for
#' #   informative two-stage hierarchical testing.
#' \donttest{Se <- matrix(data = c(rep(0.95, 2), rep(0.99, 2)),
#'              nrow = 2, ncol = 2, byrow = FALSE)
#' Sp <- matrix(data = c(rep(0.96, 2), rep(0.98, 2)),
#'              nrow = 2, ncol = 2, byrow = FALSE)
#' res4 <- OTC2(algorithm = "ID2", alpha = c(18.25, 0.75, 0.75, 0.25),
#'                 Se = Se, Sp = Sp, group.sz = 12:20)
#' plot(res4)}
#'
#'
#' # Find the optimal testing configuration for
#' #   non-informative array testing with master pooling.
#' \donttest{res5 <- OTC2(algorithm = "A2M", p.vec = c(0.90, 0.04, 0.04, 0.02),
#'              Se = rep(0.99, 2), Sp = rep(0.99, 2), group.sz = 3:20)
#' plot(res5)}

plot.OTC <- function(x, ...) {

  if (length(x$Configs) == 1 && is.na(x$Configs)) {
    stop("No similar configurations provided. No plot will be produced.")
  }

  if (x$algorithm %in% c("Informative two-stage hierarchical testing")) {
    xlabel <- "Block size"

    ggplot(data = x$Configs, aes(x = x$Configs$N, y = x$Configs$value)) +
      geom_point() + geom_line() +
      xlab(xlabel) +
      ylab("Expected number of tests\n per individual\n") +
      scale_x_continuous(breaks = pretty_breaks()) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  } else {
    if (x$algorithm %in% c("Non-informative two-stage hierarchical testing",
                           "Non-informative three-stage hierarchical testing",
                           "Informative three-stage hierarchical testing")) {
      xlabel <- "Initial group size"

      ggplot(data = x$Configs, aes(x = x$Configs$I, y = x$Configs$value)) +
        geom_point() + geom_line() +
        xlab(xlabel) +
        ylab("Expected number of tests\n per individual\n") +
        scale_x_continuous(breaks = pretty_breaks()) +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12))
    } else {
      xlabel <- "Initial row/column size"

      ggplot(data = x$Configs, aes(x = x$Configs$I, y = x$Configs$value)) +
        geom_point() + geom_line() +
        xlab(xlabel) +
        ylab("Expected number of tests\n per individual\n") +
        scale_x_continuous(breaks = pretty_breaks()) +
        theme(axis.title = element_text(size = 14),
              axis.text = element_text(size = 12))
    }
  }
}




# Print function
###############################################################################
# print.OTC() function

#' @title Print method for optimal testing configuration results
#'
#' @description Print method for objects of class \kbd{"OTC"} returned by
#' \code{\link{OTC1}} or \code{\link{OTC2}}.
#'
#' @param x an object of class \kbd{"OTC"}, providing the optimal testing
#' configuration results for a group testing algorithm.
#' @param ... Additional arguments to be passed to \code{print} (e.g.,
#' \code{digits} to be passed to \code{round} for appropriate rounding).
#'
#' @return A print out of the algorithm, testing configuration, expected number
#' of tests, expected number of tests per individual, and accuracy measures
#' for individuals and for the overall algorithm.
#'
#' @author Brianna D. Hitt

print.OTC <- function(x, ...) {

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

  config <- object$opt.ET$OTC
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
  ET <- format(round(object$opt.ET$ET, digits), nsmall = digits)

  value <- format(round(object$opt.ET$value, digits), nsmall = digits)
  cat("\nExpected number of tests:", ET)
  cat("\nObjective function value:", value)

  # In future, could replace the objective function value with the
  #   expected number of tests per individual
  # if(!is.null(object$opt.ET$p.mat)) {
  #   tot.ind <- ncol(object$opt.ET$p.mat)
  #}
  #else {
  #  tot.ind <- length(object$opt.ET$p.vec)
  #}
  # value <- format(round(object$opt.ET$ET / ncol(object$opt.ET$p.mat),
  #                      digits), nsmall = digits)
  # value <- format(round(object$opt.ET$ET / tot.ind, digits), nsmall = digits)

  # create a matrix for accuracy measures
  overall.acc <- as.data.frame(format(round(object$opt.ET$Accuracy, digits),
                                      nsmall = digits))
  colnames(overall.acc) <- c("PSe", "PSp", "PPPV", "PNPV")

  cat("\nOverall accuracy of the algorithm:\n")
  print(as.data.frame(overall.acc))
  NULL
}




# ExpTests.OTC function
##################################################################
# ExpTests.OTC() function                                        #
##################################################################

#' @title Extract the expected number of tests from optimal testing
#' configuration results
#'
#' @description Extract the expected number of tests and expected number of
#' tests per individual from objects of class "OTC" returned by
#' \code{\link{OTC1}} or \code{\link{OTC2}}.
#'
#' @param object An object of class "OTC", from which the expected number
#' of tests and expected number of tests per individual are to be extracted.
#' @param ... Additional arguments to be passed to \code{ExpTests} (e.g.,
#' \code{digits} to be passed to \code{round} for appropriate rounding).
#'
#' @return A data frame containing the columns:
#' \item{ExpTests}{the expected number of tests required by the optimal testing
#' configuration.}
#' \item{ExpTestsPerInd}{the expected number of tests per individual for the optimal
#' testing configuration.}
#' \item{PercentReductionTests}{The percent reduction in the number of tests; 100 * (1 - ExpTestsPerIndividual).}
#' \item{PercentIncreaseTestCap}{The percent increase in testing capacity when the algorithm
#'   is applied to a continuous stream of specimens; 100 * (1/ExpTestsPerIndividual - 1).}

#' Each row of the data frame represents an objective function specified in
#' the call to \code{\link{OTC1}} or \code{\link{OTC2}}.
#'
#' @author Brianna D. Hitt and Christopher R. Bilder
#'
#' @references
#' \insertRef{bilder2020tests}{binGroup2}
#'
#' @examples
#' res1 <- OTC1(algorithm = "D2", p = 0.05, Se = 0.99, Sp = 0.99,
#'              group.sz = 2:100, obj.fn = c("ET", "MAR"),
#'              trace = TRUE)
#' ExpTests.OTC(res1)

ExpTests.OTC <- function(object, ...) {
  args <- list(...)
  if (is.null(args$digits)) {
    digits <- 4
  }
  else {
    digits <- args$digits
  }

  if (dim(object$opt.ET$Accuracy)[1] == 1) {
    opt.ET <- c(object$opt.ET$ET, object$opt.ET$value)
    if (!is.null(object$opt.MAR)) {
      opt.MAR <- c(object$opt.MAR$ET,
                   object$opt.MAR$ET / max(length(object$opt.MAR$p.vec),
                                           length(object$opt.MAR$p.mat)))
    } else {opt.MAR <- c(NA, NA)}
    if (!is.null(object$opt.GR1)) {
      opt.GR1 <- c(object$opt.GR1$ET,
                   object$opt.GR1$ET / max(length(object$opt.GR1$p.vec),
                                           length(object$opt.GR1$p.mat)))
    } else {opt.GR1 <- c(NA, NA)}
    if (!is.null(object$opt.GR2)) {
      opt.GR2 <- c(object$opt.GR2$ET,
                   object$opt.GR2$ET / max(length(object$opt.GR2$p.vec),
                                           length(object$opt.GR2$p.mat)))
    } else {opt.GR2 <- c(NA, NA)}
    if (!is.null(object$opt.GR3)) {
      opt.GR3 <- c(object$opt.GR3$ET,
                   object$opt.GR3$ET / max(length(object$opt.GR3$p.vec),
                                           length(object$opt.GR3$p.mat)))
    } else {opt.GR3 <- c(NA, NA)}
    if (!is.null(object$opt.GR4)) {
      opt.GR4 <- c(object$opt.GR4$ET,
                   object$opt.GR4$ET / max(length(object$opt.GR4$p.vec),
                                           length(object$opt.GR4$p.mat)))
    } else {opt.GR4 <- c(NA, NA)}
    if (!is.null(object$opt.GR5)) {
      opt.GR5 <- c(object$opt.GR5$ET,
                   object$opt.GR5$ET / max(length(object$opt.GR5$p.vec),
                                           length(object$opt.GR5$p.mat)))
    } else {opt.GR5 <- c(NA, NA)}
    if (!is.null(object$opt.GR6)) {
      opt.GR6 <- c(object$opt.GR6$ET,
                   object$opt.GR6$ET / max(length(object$opt.GR6$p.vec),
                                           length(object$opt.GR6$p.mat)))
    } else {opt.GR6 <- c(NA, NA)}

    all <- rbind(opt.ET, opt.MAR, opt.GR1, opt.GR2, opt.GR3, opt.GR4,
                 opt.GR5, opt.GR6)
    res <- na.omit(as.data.frame(all))
    colnames(res) <- c("ExpTests", "ExpTestsPerInd")
    } else if (dim(object$opt.ET$Accuracy)[1] == 2) {
    res <- data.frame(ExpTests = object$opt.ET$ET,
                      ExpTestsPerInd = object$opt.ET$value,
                      row.names = "opt.ET")
  }

  res$PercentReductionTests <- format(round(100*(1-res[,2]),2), nsmall = 2)
  res$PercentIncreaseTestCap <- format(round(100*(1/res[,2] - 1),2), nsmall = 2)
  res$ExpTests <- format(round(res$ExpTests, digits), nsmall = 4)
  res$ExpTestsPerInd <- format(round(res$ExpTestsPerInd, digits), nsmall = 4)

  res
}





# Config function
##################################################################
# Config.OTC() function                                          #
##################################################################

#' @title Extract the testing configuration from group testing results
#'
#' @description Extract the testing configuration from objects of class
#' "OTC" returned by \code{\link{OTC1}} (\kbd{OTC1})
#' or \code{\link{OTC2}} (\kbd{OTC2}).
#'
#' @param object An object of class "OTC", from which the testing
#' configuration is to be extracted.
#' @param n Number of testing configurations.
#' @param top.overall logical; if TRUE, best overall testing configurations; if FALSE,
#'   best testing configurations by initial group size
#' @param ... currently not used.
#'
#' @return A data frame providing the best testing configurations.
#'
#' @author Christopher R. Bilder
#'
#' @examples
#' res1 <- OTC1(algorithm = "D3", p = 0.05, Se = 0.99, Sp = 0.99,
#'              group.sz = 3:15, obj.fn = "ET")
#' Config(res1)

Config.OTC <- function(object, n = 5, top.overall = FALSE, ...) {
  if(top.overall) {
    if(!is.null(object$Top.Configs)) {
      head(object$Top.Configs, n = n)
      }
    else {
      if(length(object$group.sz) == 1) {
        cat("\n The OTC is the only configuration because only one initial group (or block) size was used. \n \n")
      }
      else {
        cat("\n Due to the algorithm, the top overall is the same as when top.overall = FALSE  \n \n")
        head(object$Configs, n = n)
        }
      }
    }
  else {
    if(length(object$group.sz) == 1) {
      cat("\n The OTC is the only configuration because only one initial group (or block) size was used. \n \n")
    }
    else {
      head(object$Configs, n = n)
    }
     #invisible(object$Configs)
    }

}





