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

#' @title Implementation Weighted Voting scheme.
#'
#' @description A new implementation of \code{\link{ClassMajorityVoting}} where
#' each class value has different values (weights).
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{ClassMajorityVoting}},
#' \code{\link{ClassWeightedVoting}}, \code{\link{ProbAverageVoting}},
#' \code{\link{ProbAverageWeightedVoting}}, \code{\link{ProbBasedMethodology}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export ClassWeightedVoting

ClassWeightedVoting <- R6::R6Class(
  classname = "ClassWeightedVoting",
  portable = TRUE,
  inherit = SimpleVoting,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param cutoff A \link{character} vector defining the minimum probability
    #' used to perform a positive classification. If is not defined, 0.5 will be
    #' used as default value.
    #' @param weights A \link{numeric} vector with the weights of each cluster.
    #' If \link{NULL} performance achieved during training will be used as
    #' default.
    #'
    initialize = function(cutoff = 0.5, weights = NULL) {
      super$initialize(cutoff = cutoff)
      private$weights <- weights
    },
    #'
    #' @description The function returns the weights used to perform the voting
    #' scheme.
    #'
    #' @return A \link{numeric} vector.
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
    #' @description The function implements the cluster-weighted majority voting
    #' procedure.
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
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not ",
             "computed. Aborting...")
      }

      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing voting with '~",
                paste0(round(self$getWeights(), digits = 4), collapse = ", ~"),
                "' weights and cutoff of ", self$getCutoff())
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

      final.raw <- c()
      final.prob <- data.frame()

      raw.pred <- do.call(cbind, lapply(predictions$getAll(), function(x, col.index) {
        x$getPrediction("raw", predictions$getPositiveClass())
      }))

      prob.pred <- do.call(cbind, lapply(predictions$getAll(), function(x, col.index) {
        x$getPrediction("prob", predictions$getPositiveClass())
      }))

      for (row in seq_len(nrow(raw.pred))) {
        values <- unique(factor(as.matrix(raw.pred[row, ]),
                                levels = predictions$getClassValues()))

        row.sum <- c()
        for (val in values) {
          row.sum <- c(row.sum, sum(self$getWeights()[which(raw.pred[row, ] == val)]))
        }

        names(row.sum) <- values
        winner.class <- names(row.sum)[which(row.sum == max(row.sum))]

        if (length(winner.class) != 1) {
          stop("[", class(self)[1], "][FATAL] Tie found. Untied method under ",
               "development")
        } else {
          winner.prob <- weighted.mean(prob.pred[row, which(raw.pred[row, ] == winner.class)],
                                       self$getWeights()[which(raw.pred[row, ] == winner.class)])
          final.prob <- rbind(final.prob, data.frame(winner.prob, 1 - winner.prob))

          if (winner.class == predictions$getPositiveClass() &&
               winner.prob < self$getCutoff()) {
            winner.class <- setdiff(predictions$getClassValues(),
                                    predictions$getPositiveClass())
          }
          final.raw <- c(final.raw, winner.class)
        }
      }

      private$final.pred$set(final.prob, final.raw,
                             predictions$getClassValues(),
                             predictions$getPositiveClass())
    }
  ),
  private = list(
    weights = NULL
  )
)
