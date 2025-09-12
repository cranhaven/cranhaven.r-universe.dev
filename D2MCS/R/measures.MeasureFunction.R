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

#' @title Archetype to define customized measures.
#'
#' @description Abstract class used as a template to define new M.L. performance
#' measures.
#'
#' @details The \code{\link{GenericHeuristic}} is an full-abstract class so it cannot
#' be instantiated. To ensure the proper operation, \code{compute} method is
#' automatically invoke by \code{\link{D2MCS}} framework when needed.
#'
#' @seealso \code{\link{MeasureFunction}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export MeasureFunction

MeasureFunction <- R6::R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance An optional \code{\link{ConfMatrix}} parameter to
    #' define the type of object used to compute the measure.
    #'
    initialize = function(performance = NULL) {
      if (!is.null(performance) && !inherits(performance, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      private$performance <- performance
    },
    #'
    #' @description The function implements the metric used to measure the
    #' performance achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used to compute the measure.
    #'
    #' @details This function is automatically invoke by the \code{\link{D2MCS}}
    #' framework.
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if an error
    #' occurred.
    #'
    compute = function(performance.output = NULL) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    performance = NULL
  )
)
