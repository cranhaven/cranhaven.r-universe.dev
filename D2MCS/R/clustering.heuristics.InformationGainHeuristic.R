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

#' @title Feature-clustering based on InformationGain methodology.
#'
#' @description Performs the feature-clustering using entropy-based filters.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[FSelector]{information.gain}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export InformationGainHeuristic

InformationGainHeuristic <- R6::R6Class(
  classname = "InformationGainHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    #'
    #' @description The algorithm find weights of discrete attributes basing on
    #' their correlation with continuous class attribute. Particularly
    #' Information Gain uses \code{H(Class) + H(Attribute) - H(Class, Attribute)}
    #'
    #' @param col1 A \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 A \link{numeric} vector or matrix to perform the clustering
    #' operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return A \link{numeric} vector of length 1 or \link{NA} if an error
    #' occurs.
    #'
    #' @importFrom FSelector information.gain
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      data <- as.data.frame(cbind(col1, col2))
      names(data) <- column.names
      tryCatch(
      FSelector::information.gain(as.formula(sprintf("`%s` ~.", column.names[2])), data)$attr_importance,
      error = function(e) {
        message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                "information.gain heuristic: '", e, "' . Returning NA")
        NA
      })
    }
  )
)
