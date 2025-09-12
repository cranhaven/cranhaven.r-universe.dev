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

#' @title Implementation of Probabilistic Average Weighted voting.
#'
#' @description Computes the final prediction by performing the weighted mean of
#' the probability achieved by each cluster prediction. By default, weight
#' values are consistent with the performance value achieved by the best M.L.
#' model on each cluster.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{ClassMajorityVoting}},
#' \code{\link{ClassWeightedVoting}}, \code{\link{ProbAverageVoting}},
#' \code{\link{ProbAverageWeightedVoting}}, \code{\link{ProbBasedMethodology}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export ProbAverageWeightedVoting

ProbAverageWeightedVoting <- R6::R6Class(
  classname = "ProbAverageWeightedVoting",
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
    #' @param weights A \link{numeric} vector with the weights of each cluster.
    #' If \link{NULL} performance achieved during training will be used as
    #' default.
    #'
    initialize = function(cutoff = 0.5, class.tie = NULL, weights = NULL) {
      if (all(!is.null(class.tie), !is.character(class.tie), !is.numeric(class.tie))) {
        stop("[", class(self)[1], "][FATAL] Invalid class tie value. Aborting...")
      }

      super$initialize(cutoff = cutoff)
      private$class.tie <- class.tie
      private$weights <- weights
    },
    #'
    #' @description The function gets the class value assigned to solve ties.
    #'
    #' @return A \link{character} vector of length 1.
    #'
    getClassTie = function() { private$class.tie },
    #'
    #' @description The function returns the value of the majority class.
    #'
    #' @return A \link{character} vector of length 1 with the name of the
    #' majority class.
    #'
    getWeights = function() { private$weights },
    #'
    #' @description The function allows changing the value of the weights.
    #'
    #' @param weights A \link{numeric} vector containing the new weights.
    #'
    setWeights = function(weights) {
      if (missing(weights) || is.null(weights)) {
        message("[", class(self)[1], "][WARNING] Weights values not changed due ",
                "to inconsistency error")
      } else {
        private$weights <- data.frame(matrix(NA, nrow = 1, ncol = 0),
                                      stringsAsFactors = FALSE)
        colNames <- c()
        for (i in 1:length(weights)) {
          private$weights <- cbind(self$getWeights(),
                                   data.frame(as.numeric(weights[i]),
                                              stringsAsFactors = FALSE))
          colNames <- c(colNames, paste0("CLUSTER ", i))
        }
        names(private$weights) <- colNames
      }
    },
    #'
    #' @description The function implements the cluster-weighted probabilistic
    #' voting procedure.
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


      if (any(is.null(private$weights),
                length(private$weights) != predictions$size()))
      {
        if (isTRUE(verbose)) {
          message("[", class(self)[1], "][WARNING] Weight values are missing or ",
                   "incorrect. Assuming default model performance values")
        }
        private$weights <- sapply(predictions$getAll(), function(x) {
          x$getModelPerformance()
        })
      }


      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting using '",
                self$getClassTie(), "' as tie solving")
      }

      final.prob <- data.frame()
      final.raw <- c()

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x) {
        pred <- x$getPrediction("prob", predictions$getPositiveClass())
        data.frame(pred, row.names = row.names(pred))
      }))

      pred.weight <- as.data.frame(apply(prob.pred, 1, function(row, weights) {
        weighted.mean(row, weights) }, weights = self$getWeights())
      )


      final.prob <- data.frame(pred.weight, (1 - pred.weight),
                                row.names = row.names(prob.pred))

      names(final.prob) <- c(predictions$getPositiveClass(),
                              setdiff(predictions$getClassValues(),
                                      predictions$getPositiveClass()))

      final.raw <- c()

      for (pos in seq_len(nrow(final.prob))) {
        row <- final.prob[pos, ]
        max.col <- which(row == max(row))
        if (length(max.col) == 1) {
          max.value <- names(row)[max.col]
          if (max.value == predictions$getPositiveClass() &&
              row[max.col] < self$getCutoff()) {
            entry <- setdiff(predictions$getClassValues(),
                             predictions$getPositiveClass())
          } else { entry <- names(row)[max.col] }
        } else {
          max.values <- names(row)[max.col]
          if (is.null(self$getClassTie()) ||
              !(self$getClassTie() %in% max.values)) {
            message("[", class(self)[1], "][INFO] Tie solver not found. ",
                    "Resolving tie using first occurrence.")
            entry <- max.values[1]
          } else {
            message("[", class(self)[1], "][INFO] Tie solver found. ",
                    "Resolving tie using '", self$getClassTie(), "'.")
            entry <- self$getClassTie()
          }
        }
        final.raw <- c(final.raw, entry)
      }

      private$final.pred$set(final.prob, final.raw,
                             predictions$getClassValues(),
                             predictions$getPositiveClass())
    }
  ),
  private = list(
    weights = NULL,
    class.tie = NULL
  )
)
