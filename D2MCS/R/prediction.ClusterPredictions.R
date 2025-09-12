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

#' @title Manages the predictions achieved on a cluster.
#'
#' @description Stores the predictions achieved by the best M.L. of each cluster.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{ClassificationOutput}},
#' \code{\link{Prediction}}
#'
#' @keywords methods math
#'
#' @import R6
#'
#' @export ClusterPredictions

ClusterPredictions <- R6::R6Class(
  classname = "ClusterPredictions",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param class.values A \link{character} vector containing the values of
    #' the target class.
    #' @param positive.class A \link{character} with the value of the positive
    #' class.
    #'
    initialize = function(class.values, positive.class) {

      if (is.null(positive.class) || !(positive.class %in% class.values)) {
        stop("[", class(self)[1], "][FATAL] Positive class not found. Should be ",
             paste0(class.values, collapse = " or "), ". Aborting...")
      }

      private$positive.class <- positive.class
      private$class.values <- class.values
      private$pred <- list()
    },
    #'
    #' @description The function is used to add the prediction achieved by a
    #' specific M.L. model.
    #'
    #' @param prediction A \code{\link{Prediction}} object containing the
    #' computed predictions.
    #'
    add = function(prediction) {
      if (!"Prediction" %in% class(prediction)) {
        stop("[", class(self)[1], "][FATAL] Prediction parameter must be ",
             "defined as 'Prediction' object. Aborting... ")
      }
      private$pred <- append(private$pred, prediction)
    },
    #'
    #' @description The function returns the predictions placed at specific
    #' position.
    #'
    #' @param position A \link{numeric} value indicating the position of the
    #' predictions to be obtained.
    #'
    #' @return A \code{\link{Prediction}} object.
    #'
    get = function(position) {
      if (!all(position > 0, position <= length(private$pred))) {
        stop("[", class(self)[1], "][FATAL] Position exceeds list size. Aborting...")
      }
      private$pred[[position]]
    },
    #'
    #' @description The function returns all the predictions.
    #'
    #' @return A \link{list} containing all computed predictions.
    #'
    getAll = function() { private$pred },
    #'
    #' @description The function returns the number of computed predictions.
    #'
    #' @return A \link{numeric} value.
    #'
    size = function() { length(private$pred) },
    #'
    #' @description The function gets the value of the positive class.
    #'
    #' @return A \link{character} vector of size 1.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function returns all the values of the target class.
    #'
    #' @return A \link{character} vector containing all target values.
    #'
    getClassValues = function() { private$class.values }
  ),
  private = list(
    pred = NULL,
    positive.class = NULL,
    class.values = NULL
  )
)
