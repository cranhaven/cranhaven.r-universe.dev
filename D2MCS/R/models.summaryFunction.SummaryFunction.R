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

#' @title Abstract class to computing performance across resamples.
#'
#' @description Abstract used as template to define customized metrics to
#' compute model performance during train.
#'
#' @details This class is an archetype, so it cannot be instantiated.
#'
#' @seealso \code{\link{NoProbability}}, \code{\link{UseProbability}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export SummaryFunction

SummaryFunction <- R6::R6Class(
  classname = "SummaryFunction",
  portable = TRUE,
  public = list(
    # '
    #' @description The function carries out the initialization of parameters
    #' during runtime.
    #'
    #' @param measures A \link{character} vector with the measures used.
    #'
    initialize = function(measures) {
      if (is.null(measures))
        stop("[", class(self)[1], "][FATAL] Measures were not defined. Aborting...")
      private$measures <- measures
    },
    #'
    #' @description Abstract function used to implement the performance
    #' calculator method. To guarantee a proper operation, this method is
    #' automatically invoked by \code{\link{D2MCS}} framework.
    #'
    execute = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function obtains the measures used to compute the
    #' performance across resamples.
    #'
    #' @return A \link{character} vector of \link{NULL} if measures are not
    #' defined.
    #'
    getMeasures = function() {
      private$measures
    }
  ),
  private = list(
    measures = NULL
  )
)
