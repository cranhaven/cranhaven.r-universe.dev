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

#' @title Feature-clustering based on Mutual Information Computation theory.
#'
#' @description Performs the feature-clustering using MCC score.
#' Valid for both bi-class and multi-class problems. Only valid for bi-class
#' problems.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[infotheo]{mutinformation}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export MultinformationHeuristic

MultinformationHeuristic <- R6::R6Class(
  classname = "MultinformationHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    # Heuristic valid for discrete variables
    #'
    #' @description Mutinformation takes two random variables as input and
    #' computes the mutual information in nats according to the entropy
    #' estimator method.
    #'
    #' @param col1 A vector/factor denoting a random variable or a data.frame
    #' denoting a random vector where columns contain variables/features and
    #' rows contain outcomes/samples.
    #' @param col2 An another random variable or random vector (vector/factor or
    #' data.frame).
    #' @param column.names An optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return Returns the mutual information I(X;Y) in nats.
    #'
    #' @importFrom infotheo mutinformation
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        message("[", class(self)[1], "][WARNING] Columns must be binary. ",
                "Returning NA")
        NA
      } else {
        infotheo::mutinformation(col1, col2)
      }
    }
  )
)
