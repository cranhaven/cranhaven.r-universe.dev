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

#' @title Implementation of Combined Voting.
#'
#' @description Calculates the final prediction by performing the result of the
#' predictions of different metrics obtained through a \code{\link{SimpleVoting}}
#' class.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{ClassMajorityVoting}},
#' \code{\link{ClassWeightedVoting}}, \code{\link{ProbAverageVoting}},
#' \code{\link{ProbAverageWeightedVoting}}, \code{\link{ProbBasedMethodology}},
#' \code{\link{SimpleVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export CombinedVoting

CombinedVoting <- R6::R6Class(
  classname = "CombinedVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param voting.schemes A \link{list} of elements inherited from
    #' \code{\link{SimpleVoting}}.
    #' @param combined.metrics An object defining the metrics used to combine
    #' the voting schemes. The object must inherit from
    #' \code{\link{CombinedMetrics}} class.
    #' @param methodology An object specifying the methodology used to execute
    #' the combined voting. Object inherited from \code{\link{Methodology}}
    #' object
    #' @param metrics A \link{character} vector with the name of the
    #' metrics used to perform the combined voting operations. Metrics should be
    #' previously defined during training stage.
    #'
    initialize = function(voting.schemes, combined.metrics, methodology, metrics) {
      if (!inherits(voting.schemes, "SimpleVoting")) {
        stop("[", class(self)[1], "][FATAL] Voting.schemes parameter must be ",
             "defined as 'SimpleVoting' type. Aborting...")
      }
      if (!inherits(combined.metrics, "CombinedMetrics")) {
        stop("[", class(self)[1], "][FATAL] Combined.metrics parameter must be ",
             "defined as 'CombinedMetrics' type. Aborting...")
      }
      if (!inherits(methodology, "Methodology")) {
        stop("[", class(self)[1], "][FATAL] Methodology parameter must be ",
             "defined as 'Methodology' type. Aborting...")
      }

      if (!all(is.character(metrics), length(metrics) >= 2)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metrics. Aborting...")
      }

      super$initialize()
      private$voting.schemes <- voting.schemes
      private$combined.metrics <- combined.metrics
      private$methodology <- methodology
      private$metrics <- metrics
      private$final.pred <- FinalPred$new()
    },
    #'
    #' @description The function returns the metrics used to combine the metrics
    #' results.
    #'
    #' @return An object inherited from \code{\link{CombinedMetrics}} class.
    #'
    getCombinedMetrics = function() { private$combined.metrics },
    #'
    #' @description The function gets the methodology used to execute the
    #' combined votings.
    #'
    #' @return An object inherited from \code{\link{Methodology}} class.
    #'
    getMethodology = function() { private$methodology },
    #'
    #' @description The function returns the predictions obtained after
    #' executing the combined-voting methodology.
    #'
    #' @param type A \link{character} to define which type of predictions
    #' should be returned. If not defined all type of probabilities will be
    #' returned. Conversely if "prob" or "raw" is defined then computed
    #' 'probabilistic' or 'class' values are returned.
    #' @param target A \link{character} defining the value of the
    #' positive class.
    #' @param filter A \link{logical} value used to specify if only predictions
    #' matching the target value should be returned or not. If \link{TRUE} the
    #' function returns only the predictions matching the target value.
    #' Conversely if \link{FALSE} (by default) the function returns all the
    #' predictions.
    #'
    #' @return A \link{data.frame} with the computed predictions.
    #'
    getFinalPred = function(type = NULL, target = NULL, filter = NULL) {
      if (any(is.null(type), !(type %in% c("raw", "prob")))) {
        private$final.pred
      } else {
        if (!is.logical(filter)) {
          message("[", class(self)[1], "][WARNING] Filter parameter must be ",
                  "defined as 'logical' type. Aborting...")
          filter <- FALSE
        }
        class.values <- private$final.pred$getClassValues()

        switch(type,
               "prob" = {
                 if (is.null(target) || !(target %in% class.values)) {
                   message("[", class(self)[1], "][WARNING] Target not ",
                           "specified or invalid. Using '",
                           private$final.pred$getPositiveClass(),
                           "' as default value")
                   target <- private$final.pred$getPositiveClass()
                 }
                 if (filter) {
                   private$final.pred$getProb()[private$final.pred$getRaw() == target,
                                                as.character(target), drop = FALSE]
                 } else {
                   private$final.pred$getProb()[, as.character(target), drop = FALSE]
                 }
               },
               "raw" = {
                 if (filter) {
                   private$final.pred$getRaw()[private$final.pred$getRaw() == target,
                                               , drop = FALSE]
                 } else { private$final.pred$getRaw() }
               }
        )
      }
    },
    #'
    #' @description The function implements the combined voting scheme.
    #'
    #' @param predictions A \code{\link{ClusterPredictions}} object containing
    #' the predictions computed for each cluster.
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #'
    execute = function(predictions, verbose = FALSE) {

      if (is.null(predictions) || !is.vector(predictions) ||
           !all(sapply(predictions, function(pred) {
                       inherits(pred, "ClusterPredictions") }))) {
        stop("[", class(self)[1], "][FATAL] Predictions parameter must be a ",
             "list comprised of 'ClusterPredictions' objects. Aborting...")
      }

      if (any(sapply(predictions, function(pred) { pred$size() <= 0 }))) {
        stop("[", class(self)[1], "][FATAL] Cluster predictions were not ",
             "computed. Aborting...")
      }

      if (!any(self$getMetrics() %in% names(predictions))) {
        stop("[", class(self)[1], "][FATAL] Metrics are incorrect. ",
             "Must be: [", paste(names(predictions), collapse = ", "),
             "]. Aborting...")
      }

      predictions <- predictions[self$getMetrics()]

      positive.class <- predictions[[1]]$getPositiveClass()
      class.values <- predictions[[1]]$getClassValues()
      negative.class <- setdiff(class.values,
                                positive.class)

      all.raw.pred <- data.frame(matrix(nrow = length(predictions[[1]]$getAll()[[1]]$getPrediction(type = "raw")),
                                        ncol = 0))
      all.prob.pred <- data.frame(matrix(nrow = length(predictions[[1]]$getAll()[[1]]$getPrediction(type = "raw")),
                                         ncol = 0))

      for (pos in seq_len(length(predictions))) {
        metric <- names(predictions)[[pos]]
        predictions.metric <- predictions[[pos]]
        private$voting.schemes$execute(predictions = predictions.metric,
                                       verbose = verbose)
        all.raw.pred <- cbind(all.raw.pred,
                              self$getVotingSchemes()$getFinalPred(type = "raw"))
        names(all.raw.pred)[length(all.raw.pred)] <- metric

        clusterPredictions <- sapply(predictions.metric$getAll(), function(x) {
          x$getPrediction(type = "prob")
        })
        names(clusterPredictions) <- rep_len(x = metric,
                                             length(clusterPredictions))
        all.prob.pred <- cbind(all.prob.pred,
                               clusterPredictions)
      }

      final.raw.pred <- c()
      final.prob.pred <- data.frame()

      for (row in seq_len(dim(all.raw.pred)[1])) {
        row.raw.pred <- all.raw.pred[row, ]
        row.prob.pred <- all.prob.pred[row, ]
        names(row.raw.pred) <- names(all.raw.pred)
        names(row.prob.pred) <- names(all.prob.pred)
        if (self$getCombinedMetrics()$getFinalPrediction(raw.pred = row.raw.pred,
                                                         prob.pred = row.prob.pred,
                                                         positive.class = positive.class,
                                                         negative.class = negative.class)) {
          final.raw.pred <- c(final.raw.pred, positive.class)

        } else { final.raw.pred <- c(final.raw.pred, negative.class) }

        prob.pred <- self$getMethodology()$compute(raw.pred = row.raw.pred,
                                                   prob.pred = row.prob.pred,
                                                   positive.class = positive.class,
                                                   negative.class = negative.class)

        final.prob.pred <- rbind(final.prob.pred, data.frame(prob.pred, abs(1 - prob.pred)))
      }

      private$final.pred$set(prob = final.prob.pred, raw = final.raw.pred,
                              class.values = class.values,
                              positive.class = positive.class)

      combined.voting <- list(self)
      names(combined.voting) <- class(self$getMethodology())[1]
      combined.voting <- list(combined.voting)
      names(combined.voting) <- as.character(self$getVotingSchemes()$getCutoff())
      combined.voting <- list(combined.voting)
      names(combined.voting) <-  paste0(self$getMetrics(),
                                        collapse = "-")
      combined.voting
    }
  ),
  private = list(
    combined.metrics = NULL,
    methodology = NULL,
    final.pred = NULL
  )
)
