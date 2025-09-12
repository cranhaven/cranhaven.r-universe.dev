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

#' @title Abstract class to compute the class prediction based on combination
#' between metrics.
#'
#' @description Abstract class used as a template to define new customized
#' strategies to combine the class predictions made by different metrics.
#'
#' @seealso \code{\link{CombinedVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export CombinedMetrics

CombinedMetrics <- R6::R6Class(
  classname = "CombinedMetrics",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param required.metrics A \link{character} vector of length greater than
    #' 2 with the name of the required metrics.
    #'
    initialize = function(required.metrics) {
      if (is.null(required.metrics) || !is.character(required.metrics) || length(required.metrics) < 2) {
        stop("[", class(self)[1], "][FATAL] The required.metrics parameter must be ",
             "defined as 'character' type. Aborting...")
      }
      private$required.metrics <- required.metrics
    },
    #'
    #' @description The function returns the required metrics that will
    #' participate in the combined metric process.
    #'
    #' @return A \link{character} vector of length greater than 2 with the name
    #' of the required metrics.
    #'
    getRequiredMetrics = function() { private$required.metrics },
    #'
    #' @description Function used to implement the strategy to obtain the final
    #' prediction based on different metrics.
    #'
    #' @param raw.pred A \link{character} list of length greater than 2 with the
    #' class value of the predictions made by the metrics.
    #' @param prob.pred A \link{numeric} list of length greater than 2 with the
    #' probability of the predictions made by the metrics.
    #' @param positive.class A \link{character} with the value of the positive
    #' class.
    #' @param negative.class A \link{character} with the value of the negative
    #' class.
    #'
    #' @return A \link{logical} value indicating if the instance is predicted as
    #' positive class or not.
    #'
    getFinalPrediction = function(raw.pred, prob.pred, positive.class, negative.class) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    required.metrics = c()
  )
)
