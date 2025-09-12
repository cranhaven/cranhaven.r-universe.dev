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

#' @title Control parameters for train stage.
#'
#' @description Abstract class used as template to define customized functions
#' to control the computational nuances of train function.
#'
#' @seealso \code{\link{TwoClass}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export TrainFunction

TrainFunction <- R6::R6Class(
  classname = "TrainFunction",
  portable = TRUE,
  public = list(
    #'
    #' @description Function used to initialize the object parameters during
    #' execution time.
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
                          allowParallel, verboseIter, seed) {
      private$method <- method
      private$folds <- number
      private$savePredictions <- savePredictions
      private$classProbs <- classProbs
      private$allowParallel <- allowParallel
      private$verboseIter <- verboseIter
      if (!is.numeric(seed)) {
        private$seed <- .Random.seed[ceiling(runif(1, 0, length(.Random.seed)))]
        message("[", class(self)[1], "][INFO] Using random seed '", private$seed, "'")
      } else {
        private$seed <- seed
        message("[", class(self)[1], "][INFO] Using static seed '", private$seed, "'")
      }
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
    #' in each resample.
    #'
    create = function(summaryFunction, search.method = "grid", class.probs) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Returns the resampling method used during training staged.
    #'
    #' @return A \link{character} vector or length 1 or \link{NULL}
    #' if not defined.
    #'
    getResamplingMethod = function() { private$method },
    #'
    #' @description Returns the number or folds or number of iterations used
    #' during training.
    #'
    #' @return An \link{integer} vector or length 1 or \link{NULL} if not
    #' defined.
    #'
    getNumberFolds = function() { private$folds },
    #'
    #' @description Indicates if the predictions for each resample should be
    #' saved.
    #'
    #' @return A \link{logical} value or \link{NULL} if not defined.
    #'
    getSavePredictions = function() { private$savePredictions },
    #'
    #' @description Indicates if class probabilities should be computed for
    #' classification models in each resample.
    #'
    #' @return A \link{logical} value.
    #'
    getClassProbs = function() { private$classProbs },
    #'
    #' @description Determines if model training is performed in parallel.
    #'
    #' @return A \link{logical} value. \link{TRUE} indicates parallelization is
    #' enabled and \link{FALSE} otherwise.
    #'
    getAllowParallel = function() { private$allowParallel },
    #'
    #' @description Determines if training log should be printed.
    #'
    #' @return A \link{logical} value. \link{TRUE} indicates training log is
    #' enabled and \link{FALSE} otherwise.
    #'
    getVerboseIter = function() { private$verboseIter },
    #'
    #' @description Function used to return the
    #' \code{\link[caret]{trainControl}} object.
    #'
    #' @return A \code{\link[caret]{trainControl}} object.
    #'
    getTrFunction = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Returns the measures used to optimize model hyperparameters.
    #'
    #' @return A \link{character} vector.
    #'
    getMeasures = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Obtains the type of classification problem ("Bi-class" or
    #' "Multi-class").
    #'
    #' @return A \link{character} vector with length 1. Either "Bi-class"
    #' or "Multi-class".
    #'
    getType = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Indicates seed used during model training stage.
    #'
    #' @return An \link{integer} value or \link{NULL} if not defined.
    #'
    getSeed = function() { private$seed },
    #'
    #' @description Function used to change the \code{\link{SummaryFunction}}
    #' used in the training stage.
    #'
    #' @param summaryFunction An object inherited from
    #' \code{\link{SummaryFunction}} class.
    #'
    setSummaryFunction = function(summaryFunction) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function allows changing the class computation
    #' capabilities.
    #'
    #' @param class.probs A \link{logical} indicating if class probabilities
    #' should be computed for classification models (along with predicted values)
    #' in each resample
    #'
    setClassProbs = function(class.probs) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    method = NULL,
    folds = NULL,
    savePredictions = NULL,
    classProbs = NULL,
    allowParallel = NULL,
    verboseIter = NULL,
    seed = NULL
  )
)
