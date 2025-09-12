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

#' @title Encapsulates the achieved predictions.
#'
#' @description The class used to encapsulates all the computed predictions to
#' facilitate their access and maintenance.
#'
#' @seealso \code{\link{D2MCS}}
#'
#' @keywords math misc
#'
#' @import R6
#'
#' @export PredictionOutput

PredictionOutput <- R6::R6Class(
  classname = "PredictionOutput",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param predictions A \link{list} of \code{\link{FinalPred}}
    #' elements.
    #' @param type A \link{character} to define which type of predictions
    #' should be returned. If not defined all type of probabilities will be
    #' returned. Conversely if "prob" or "raw" is defined then computed
    #' 'probabilistic' or 'class' values are returned.
    #' @param target A \link{character} defining the value of the
    #' positive class.
    #'
    initialize = function(predictions, type, target) {
      private$predictions <- predictions
      private$type <- type
      private$target <- target
    },
    #'
    #' @description The function returns the final predictions.
    #'
    #' @return A \link{list} containing the final predictions or \link{NULL} if
    #' classification stage was not successfully performed.
    #'
    getPredictions = function() { private$predictions },
    #'
    #' @description The function returns the type of prediction should be
    #' returned. If "prob" or "raw" is defined then computed 'probabilistic' or
    #' 'class' values are returned.
    #'
    #' @return A \link{character} value.
    #'
    getType = function() { private$type },
    #'
    #' @description The function returns the value of the target class.
    #'
    #' @return A \link{character} value.
    #'
    getTarget = function() { private$target }
  ),
  private = list(
    predictions = NULL,
    type = NULL,
    target = NULL
  )
)
