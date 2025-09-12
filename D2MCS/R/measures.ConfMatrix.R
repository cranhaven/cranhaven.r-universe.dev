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

#' @title Confusion matrix wrapper.
#'
#' @description Creates a \code{\link{R6}} confusion matrix from the
#' \code{\link[caret]{confusionMatrix}} caret package.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{MeasureFunction}},
#' \code{\link{ClassificationOutput}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export ConfMatrix

ConfMatrix <- R6::R6Class(
  classname = "ConfMatrix",
  portable = TRUE,
  public = list(
    #'
    #' @description Method to create a confusion matrix object from a
    #' \code{caret} \code{\link[caret]{confusionMatrix}}
    #'
    #' @param confMatrix A \code{caret} \link[caret]{confusionMatrix} argument.
    #'
    initialize = function(confMatrix) {
      if (!inherits(confMatrix, "confusionMatrix"))
        stop("[", class(self)[1], "][FATAL] ConfMatrix parameter must be defined ",
             "as 'caret::confusionMatrix' type. Aborting...")

      private$positive.class <- confMatrix$positive
      private$negative.class <- colnames(confMatrix$table)[which(colnames(confMatrix$table) != confMatrix$positive)]
      private$confusionMatrix <- confMatrix
    },
    #'
    #' @description The function obtains the \code{\link[caret]{confusionMatrix}}
    #' following the same structured as defined in the \code{caret} package
    #'
    #' @return A \code{\link[caret]{confusionMatrix}} object.
    #'
    getConfusionMatrix = function() { private$confusionMatrix },
    #'
    #' @description The function is used to compute the number of True Positive
    #' values achieved.
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getTP = function() { private$confusionMatrix$table[private$positive.class, private$positive.class] },
    #'
    #' @description The function computes the True Negative values.
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getTN = function() { private$confusionMatrix$table[private$negative.class, private$negative.class] },
    #'
    #' @description The function returns the number of Type II errors
    #' (False Negative).
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getFN = function() { private$confusionMatrix$table[private$negative.class, private$positive.class] },
    #'
    #' @description The function returns the number of Type I errors
    #' (False Negative).
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getFP = function() { private$confusionMatrix$table[private$positive.class, private$negative.class] }
  ),
  private = list(
    confusionMatrix = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)
