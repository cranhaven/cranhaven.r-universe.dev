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

#' @title Control parameters for train stage (Bi-class problem).
#'
#' @description Implementation to control the computational nuances of train
#' function for bi-class problems.
#'
#' @seealso \code{\link{TrainFunction}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export TwoClass

TwoClass <- R6::R6Class(
  classname = "TwoClass",
  portable = TRUE,
  inherit = TrainFunction,
  public = list(
    #'
    #' @param method The resampling method: "boot", "boot632", "optimism_boot",
    #' "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV" (for repeated
    #' training/test splits), "none" (only fits one model to the entire training
    #' set), "oob" (only for random forest, bagged trees, bagged earth, bagged
    #' flexible discriminant analysis, or conditional tree forest models),
    #' timeslice, "adaptive_cv", "adaptive_boot" or "adaptive_LGOCV"
    #' @param number Either the number of folds or number of resampling
    #' iterations
    #' @param savePredictions An indicator of how much of the hold-out
    #' predictions for each resample should be saved. Values can be either
    #' "all", "final", or "none". A logical value can also be used that convert
    #' to "all" (for true) or "none" (for false). "final" saves the predictions
    #' for the optimal tuning parameters.
    #' @param classProbs A \link{logical} value. Should class probabilities be
    #' computed for classification models (along with predicted values) in each
    #' resample?
    #' @param allowParallel A \link{logical} value. If a parallel backend is
    #' loaded and available, should the function use it?
    #' @param verboseIter A \link{logical} for printing a training log.
    #' @param seed An optional \link{integer} that will be used to set the seed
    #' during model training stage.
    #'
    initialize = function(method, number, savePredictions, classProbs,
                          allowParallel, verboseIter, seed = NULL) {

      super$initialize(method, number, savePredictions, classProbs,
                       allowParallel, verboseIter, seed)
    },
    #'
    #' @description Creates a \code{\link[caret]{trainControl}} requires for the
    #' training stage.
    #'
    #' @param summaryFunction An object inherited from
    #' \code{\link{SummaryFunction}} class.
    #' @param search.method Either "grid" or "random", describing how the tuning
    #' parameter grid is determined.
    #' @param class.probs A \link{logical} indicating if class probabilities
    #' should be computed for classification models (along with predicted values)
    #' in each resample
    #'
    #' @import caret
    #'
    create = function(summaryFunction, search.method = "grid", class.probs = NULL) {
      if (is.null(summaryFunction) ||  !"SummaryFunction" %in% class(summaryFunction)) {
        stop("[", class(self)[1], "][FATAL] SummaryFunction parameter must be ",
             "defined as 'SummaryFunction' type. Aborting...")
      } else {
        if (all(!is.null(search.method), search.method %in% c("grid", "random"))) {
          private$search <- search.method
        } else {
          message("[", class(self)[1], "][WARNING] Invalid search method. ",
                  "Only 'random' or 'grid' search method are available. ",
                  "Assuming grid method")
          private$search <- "grid"
        }
        class.probability <- ifelse((!is.null(class.probs) &&
                                         is.logical(class.probs)),
                                     class.probs,  super$getClassProbs())

        private$trFunction <- caret::trainControl(method = super$getResamplingMethod(),
                                                  number = super$getNumberFolds(),
                                                  savePredictions = super$getSavePredictions(),
                                                  classProbs = class.probability,
                                                  summaryFunction = summaryFunction$execute,
                                                  search = private$search,
                                                  allowParallel = super$getAllowParallel(),
                                                  verboseIter = super$getVerboseIter())

        private$measures <- summaryFunction$getMeasures()
      }
    },
    #'
    #' @description Function used to return the
    #' \code{\link[caret]{trainControl}} object.
    #'
    #' @return A \code{\link[caret]{trainControl}} object.
    #'
    getTrFunction = function() {
      if (is.null(private$trFunction))
        message("[", class(self)[1], "][WARNING] TrainFunction is not created. ",
                "Execute create method first. Task not performed")
      private$trFunction
    },
    #'
    #' @description The function allows changing the class computation
    #' capabilities.
    #'
    #' @param class.probs A \link{logical} value. \link{TRUE} implies
    #' classification probabilities should be computed for classification models
    #' and \link{FALSE} otherwise.
    #'
    setClassProbs = function(class.probs) {
      if (is.null(class.probs) || !is.logical(class.probs)) {
        message("[", class(self)[1], "][WARNING] Class probabilities parameter ",
                "is null or erroneous. Task not performed")
      } else {
        if (is.null(private$trFunction)) {
          message("[", class(self)[1], "][WARNING] TrainFunction is not created. ",
                  "Execute create method first. Task not performed")
        } else {
          private$trFunction$classProbs <- class.probs
        }
      }
    },
    #'
    #' @description Returns the measures used to optimize model hyperparameters.
    #'
    #' @return A \link{character} vector.
    #'
    getMeasures = function() { private$measures },
    #'
    #' @description Obtains the type of classification problem ("Bi-class" or
    #' "Multi-class").
    #'
    #' @return A \link{character} vector with "Bi-class" value.
    #'
    getType = function() { private$type },
    #'
    #' @description Function used to change the \code{\link{SummaryFunction}}
    #' used in the training stage.
    #'
    #' @param summaryFunction An object inherited from
    #' \code{\link{SummaryFunction}} class.
    #'
    setSummaryFunction = function(summaryFunction) {
      if (is.null(summaryFunction) || !"SummaryFunction" %in% class(summaryFunction)) {
        message("[", class(self)[1], "][WARNING] SummaryFunction parameter ",
                "is null or incorrect type. Task not performed")
      } else {
        if (is.null(private$trFunction)) {
          message("[", class(self)[1], "][WARNING] TrainFunction is not created. ",
                  "Execute create method first. Task not performed")
        } else {
          private$trFunction$summaryFunction <- summaryFunction$execute
        }
      }
    }
  ),
  private = list(
    measures = NULL,
    search = "grid",
    trFunction = NULL,
    type = "Bi-Class"
  )
)
