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

#' @title Computes the Matthews correlation coefficient.
#'
#' @description The Matthews correlation coefficient is used in machine learning
#' as a measure of the quality of binary (two-class) classifications. It takes
#' into account true and false positives and negatives and is generally regarded
#' as a balanced measure which can be used even if the classes are of very
#' different sizes. The MCC is in essence a correlation coefficient between the
#' observed and predicted binary classifications; it returns a value between -1
#' and +1.
#'
#' @details \deqn{MCC = (TP × (TN - FP) × FN)/(\sqrt{(TP + FP) × (TP + FN) × (TN + FP) × (TN + FN)})}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export MCC

MCC <- R6::R6Class(
  classname = "MCC",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' used as basis to compute the \code{MCC} measure.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{MCC} achieved by the M.L.
    #' model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the \code{MCC}
    #' measure.
    #'
    #' @details This function is automatically invoke by the
    #' \link{ClassificationOutput} object.
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if an error
    #' occurred.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- mltools::mcc(TP = performance.output$getTP(), FP = performance.output$getFP(),
                                FN = performance.output$getFN(), TN = performance.output$getTN())
      else output <- mltools::mcc(TP = private$performance$getTP(), FP = private$performance$getFP(),
                                   FN = private$performance$getFN(), TN = private$performance$getTN())
      names(output) <- class(self)[1]
      output
    }
  )
)
