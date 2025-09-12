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

#' @title Manages the execution of Simple Votings.
#'
#' @description The class is responsible of initializing and executing voting
#' schemes. Additionally, to ensure a proper operation, the class automatically
#' checks the compatibility of defined voting schemes.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{SimpleVoting}},
#' \code{\link{CombinedVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export SingleVoting

SingleVoting <- R6::R6Class(
  classname = "SingleVoting",
  portable = TRUE,
  inherit = VotingStrategy,
  public = list(
    #'
    #' @description The function initializes the object arguments during runtime.
    #'
    #' @param voting.schemes A \link{vector} of voting schemes inheriting from
    #' \code{\link{SimpleVoting}} class.
    #' @param metrics A \link{list} containing the metrics used as basis to
    #' perform the voting strategy.
    #'
    initialize = function(voting.schemes, metrics) {
      if (is.null(voting.schemes) || !is.vector(voting.schemes) ||
          !all(sapply(voting.schemes, function(voting) {
        inherits(voting, "SimpleVoting")
      }))) {
        stop("[", class(self)[1], "][FATAL] Voting schemes parameter must be a ",
             "list comprised of 'SimpleVoting' objects. Aborting...")
      }

      if (!all(is.character(metrics))) {
        stop("[", class(self)[1], "][FATAL] Metrics parameter must be a list ",
             "comprised of 'character' objects. Aborting... ")
      }

      super$initialize()
      private$voting.schemes <- voting.schemes
      private$metrics <- metrics
    },
    #'
    #' @description The function is used to execute all the previously defined
    #' (and compatible) voting schemes.
    #'
    #' @param predictions A \code{\link{ClusterPredictions}} object containing
    #' all the predictions computed in the classification stage.
    #' @param verbose A \link{logical} value to specify if more verbosity is needed.
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
      single.votings <- list()
      for (preds in seq_len(length(predictions))) {
        metric <- names(predictions)[preds]
        votings.list <- list()
        for (voting.scheme in private$voting.schemes) {
          voting.name <- class(voting.scheme)[1]
          message("[", class(self)[1], "][INFO] ------------------------------",
                  "-------------------------")
          message("[", class(self)[1], "][INFO] Executing '", voting.name,
                  "' for '", metric, "' metric with '",
                  voting.scheme$getCutoff(), "' cutoff")
          message("[", class(self)[1], "][INFO] ------------------------------",
                  "-------------------------")
          voting.scheme$execute(predictions[[preds]])
          list.element <- list(voting.scheme)
          names(list.element) <- paste0(voting.name)
          entry.name <- as.character(voting.scheme$getCutoff())
          votings.list[[entry.name]] <- append(votings.list[[entry.name]],
                                               list.element)
        }
        single.votings[[metric]] <- votings.list
      }
      single.votings
    }
  )
)
