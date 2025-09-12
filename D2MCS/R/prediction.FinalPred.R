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

#' @title Stores the prediction for a specific voting scheme.
#'
#' @description The class is used to store the computed probability after
#' executing an specific voting scheme.
#'
#' @seealso \code{\link{Prediction}}, \code{\link{SimpleVoting}},
#' \code{\link{SingleVoting}}, \code{\link{CombinedVoting}},
#' \code{\link{VotingStrategy}}
#'
#' @keywords internal
#'
#' @import R6

FinalPred <- R6::R6Class(
  classname = "FinalPred",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object variables during runtime.
    #'
    initialize = function() {
      private$prob <- NULL
      private$raw <- NULL
      private$positive.class <- NULL
      private$negative.class <- NULL
    },
    #'
    #' @description Sets the computed probabilities after executing an specific
    #' voting scheme.
    #'
    #' @param prob A \link{vector} containing the probabilities of the
    #' prediction for a specific voting scheme.
    #' @param raw A \link{vector} containing the raw results of the prediction
    #' for a specific voting scheme.
    #' @param class.values A \link{vector} containing the class values.
    #' @param positive.class A \link{character} value containing the positive
    #' class.
    #'
    set = function(prob, raw, class.values, positive.class) {
      if (length(positive.class) != 1 || !(positive.class %in% class.values)) {
        stop("[", class(self)[1], "][FATAL] Positive class is invalid. ",
             "Must be one of (", paste0(class.values, collapse = ", "),
             "). Aborting...")
      }

      if (any(is.null(prob), is.null(raw), nrow(prob) == 0,
             ncol(row) == 0, length(raw) == 0)) {
        stop("[", class(self)[1], "][FATAL] Predictions were not computed. ",
             "Aborting...")
      }

      private$negative.class <- setdiff(class.values, positive.class)
      private$positive.class <- positive.class

      private$prob <- prob

      if (!is.factor(raw)) {
        private$raw <- factor(raw, levels = union(private$positive.class,
                                                  private$negative.class))
        private$raw <- relevel(private$raw, ref = as.character(private$positive.class))
      } else {
        private$raw <- raw
        private$prob <- prob
      }

      if (any(is.na(private$raw))) {
        stop("[", class(self)[1], "][FATAL] Class values contains NA's. ",
             "Aborting...")
      }

      names(private$prob) <- self$getClassValues()

    },
    #'
    #' @description Gets the probabilities of the prediction for a specific
    #' voting scheme.
    #'
    #' @return The \link{vector} value of probabilities of the prediction for a
    #' specific voting scheme.
    #'
    getProb = function() { private$prob },
    #'
    #' @description Gets the raw results of the prediction for a specific voting
    #' scheme.
    #'
    #' @return The \link{vector} value of raw results of the prediction for a
    #' specific voting scheme.
    #'
    getRaw = function() { private$raw },
    #'
    #' @description Gets the class values (positive class + negative class).
    #'
    #' @return The \link{vector} value of class values.
    #'
    getClassValues = function() {
      union(private$positive.class, private$negative.class)
    },
    #'
    #' @description Gets the positive class.
    #'
    #' @return The \link{character} value of positive class.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description Gets the negative class.
    #'
    #' @return The \link{character} value of negative class.
    #'
    getNegativeClass = function() { private$negative.class }
  ),
  private = list(
    prob = NULL,
    raw = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)
