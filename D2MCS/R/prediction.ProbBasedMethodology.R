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

#' @title Methodology to obtain the combination of the probability of different
#' metrics.
#'
#' @description Calculates the mean of the probabilities of the different
#' metrics.
#'
#' @seealso \code{\link{Methodology}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export ProbBasedMethodology

ProbBasedMethodology <- R6::R6Class(
  classname = "ProbBasedMethodology",
  portable = TRUE,
  inherit = Methodology,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param required.metrics A \link{character} vector of length
    #' greater than 2 with the name of the required metrics.
    #'
    initialize = function(required.metrics = c("MCC", "PPV")) {
      if (any(is.null(required.metrics),
              !is.character(required.metrics),
              length(required.metrics) < 2)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of required.metrics. Aborting...")
      }
      super$initialize(required.metrics = required.metrics)
    },
    #'
    #' @description Function to compute the probability of the final prediction
    #' based on different metrics.
    #'
    #' @param raw.pred A \link{character} list of length greater than 2 with the
    #' class value of the predictions made by the metrics.
    #' @param prob.pred A \link{numeric} list of length greater than 2 with the
    #' probability of the predictions made by the metrics.
    #' @param positive.class A \link{character} with the value of the positive
    #' class.
    #' @param negative.class A \link{character} with the value of the
    #' negative class.
    #'
    #' @return A \link{numeric} value indicating the probability of the instance
    #' is predicted as positive class.
    #'
    compute = function(raw.pred, prob.pred, positive.class, negative.class) {
      if (is.null(raw.pred) || !is.list(raw.pred)) {
        stop("[", class(self)[1], "][FATAL] Raw.pred parameter must be defined ",
             "as 'list' type. Aborting...")
      }
      if (!all(self$getRequiredMetrics() %in% names(raw.pred))) {
        stop("[", class(self)[1], "][FATAL] Raw.pred parameter must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (is.null(prob.pred) || !is.list(prob.pred)) {
        stop("[", class(self)[1], "][FATAL] Prob.pred parameter must be defined ",
             "as 'list' type. Aborting...")
      }
      if (!all(self$getRequiredMetrics() %in% names(prob.pred))) {
        stop("[", class(self)[1], "][FATAL] Prob.pred parameter must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (is.null(positive.class) || (!is.character(positive.class) && !is.numeric(positive.class))) {
        stop("[", class(self)[1], "][FATAL] Positive class parameter must be defined. Aborting...")
      }
      if (is.null(negative.class) || (!is.character(negative.class) && !is.numeric(negative.class))) {
        stop("[", class(self)[1], "][FATAL] Negative class parameter must be defined. Aborting...")
      }

      Reduce(prod, prob.pred[which(names(prob.pred) %in% self$getRequiredMetrics())])
    }
  )
)
