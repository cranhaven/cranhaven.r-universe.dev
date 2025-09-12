#
# D2MCS provides a novel framework to able to automatically develop and deploy
# an accurate Multiple Classifier System (MCS) based on the feature-clustering
# distribution achieved from an input dataset. D2MCS was developed focused on
# four main aspects: (i) the ability to determine an effective method to
# evaluate the independence of features, (ii) the identification of the optimal
# number of feature clusters, (iii) the training and tuning of ML models and
# (iv) the execution of voting schemes to combine the outputs of each classifier
# comprising the MCS.
#
# Copyright (C) 2021 Sing Group (University of Vigo)
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/gpl-3.0.html>

#' @title Implementation of Majority Voting voting.
#'
#' @description Implementation of the parliamentary 'majority voting' procedure.
#' The majority class value is defined as final class. All class values have the
#' same importance.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{ClassMajorityVoting}},
#' \code{\link{ClassWeightedVoting}}, \code{\link{ProbAverageVoting}},
#' \code{\link{ProbAverageWeightedVoting}}, \code{\link{ProbBasedMethodology}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export ClassMajorityVoting

ClassMajorityVoting <- R6::R6Class(
  classname = "ClassMajorityVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param cutoff A \link{character} vector defining the minimum probability
    #' used to perform a positive classification. If is not defined, 0.5 will be
    #' used as default value.
    #' @param class.tie A \link{character} used to define the target class value
    #' used when a tie is found. If \link{NULL} positive class value will be
    #' assigned.
    #' @param majority.class A \link{character} defining the value of the
    #' majority class. If \link{NULL} will be used same value as training stage.
    #'
    initialize = function(cutoff = 0.5, class.tie = NULL, majority.class = NULL) {
      if (all(!is.null(class.tie), !is.character(class.tie), !is.numeric(class.tie))) {
        stop("[", class(self)[1], "][FATAL] Invalid class tie value. Aborting...")
      }
      super$initialize(cutoff = cutoff)
      private$class.tie <- class.tie
      private$majority.class <- majority.class
    },
    #'
    #' @description The function returns the value of the majority class.
    #'
    #' @return A \link{character} vector of length 1 with the name of the
    #' majority class.
    #'
    getMajorityClass = function() { private$majority.class },
    #'
    #' @description The function gets the class value assigned to solve ties.
    #'
    #' @return A \link{character} vector of length 1.
    #'
    getClassTie = function() { private$class.tie },
    #'
    #' @description The function implements the majority voting procedure.
    #'
    #' @param predictions A \code{\link{ClusterPredictions}} object containing
    #' all the predictions achieved for each cluster.
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #'
    execute = function(predictions, verbose = FALSE) {
      if (!inherits(predictions, "ClusterPredictions")) {
        stop("[", class(self)[1], "][FATAL] Predictions parameter must be defined ",
             "as 'ClusterPrediction' type. Aborting...")
      }

      if (predictions$size() <= 0) {
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not computed. ",
             "Aborting...")
      }

      if (is.null(private$majority.class) ||
          !(private$majority.class %in% predictions$getClassValues())) {
        message("[", class(self)[1], "][WARNING] Majority class unset or invalid.",
                " Assuming '", predictions$getPositiveClass(), "' by default")
        private$majority.class <- predictions$getPositiveClass()
      }

      if (any(is.null(private$class.tie),
             !(private$class.tie %in% predictions$getClassValues()))) {
        message("[", class(self)[1], "][INFO] Class tie unset or invalid")
        private$class.tie <- NULL
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting using '",
                self$getMajorityClass(), "' as majority class")
      }

      final.raw <- c()
      final.prob <- data.frame()

      raw.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("raw")
        data.frame(pred, row.names = row.names(pred))
      }))

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))
      for (row in 1:nrow(prob.pred)) {
        row.summary <- table(as.matrix(raw.pred[row, ]))
        max.values <- names(which(row.summary == max(row.summary)))

        if (length(max.values) > 1) {
          if (self$getMajorityClass() %in% max.values) {
            message("[", class(self)[1], "][INFO] Found Tie. Resolving using ",
                    "'majority class' solver")
            entry <- self$getMajorityClass()
          } else {
            entry <- self$getClassTie()
            if (any(is.null(self$getClassTie()),
                    !(self$getClassTie() %in% max.values))) {
              message("[", class(self)[1], "][INFO] Resolving tie using first ",
                      "occurrence.")
              entry <- max.values[1]
            }
          }
        } else { entry <- max.values }

        mean.row <- mean(as.numeric(prob.pred[row, which(raw.pred[row, ] == entry)]))

        if (entry == predictions$getPositiveClass() &&
            mean.row < self$getCutoff()) {
          entry <- setdiff(predictions$getClassValues(), predictions$getPositiveClass())
        }

        final.prob <- rbind(final.prob, data.frame(mean.row, abs(mean.row - 1)))
        final.raw <- c(final.raw, entry)
      }

      private$final.pred$set(final.prob, final.raw,
                             predictions$getClassValues(),
                             predictions$getPositiveClass())
    }
  ),
  private = list(
    class.tie = NULL,
    majority.class = NULL
  )
)
