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

#' @title Feature-clustering based on Fisher's Exact Test.
#'
#' @description Performs feature-clustering based on Fisher's exact test for
#' testing the null of independence of rows and columns in a contingency table
#' with fixed marginals.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[stats]{fisher.test}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export FisherTestHeuristic

FisherTestHeuristic <- R6::R6Class(
  classname = "FisherTestHeuristic",
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
    #' @description Performs the Fisher's exact test for testing the null of
    #' independence between two columns (col1 and col2).
    #'
    #' @param col1 A \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 A \link{numeric} vector or matrix to perform the clustering
    #' operation.
    #' @param column.names An optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return A \link{numeric} vector of length 1 or \link{NA} if an error
    #' occurs.
    #'
    #' @importFrom stats fisher.test
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (private$isBinary(col1) && private$isBinary(col2)) {
        col2.factor <- factor(col2)
        col1.factor <- factor(col1, levels =  levels(col2.factor))
        tryCatch(
        stats::fisher.test(col1.factor, col2.factor)$p.value,
        # stats::fisher.test(table(col1.factor, col2.factor))$p.value
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "fisher.test heuristic: '", e, "' . Returning NA")
          NA
        })
      } else {
        if (!private$isBinary(col1))
          message("[", class(self)[1], "][WARNING] Column '",
                  column.names[1], "' is not binary. Returning NA")
        else message("[", class(self)[1], "][WARNING] Column '",
                     column.names[2], "' is not binary. Returning NA")
        NA
      }
    }
  )
)
