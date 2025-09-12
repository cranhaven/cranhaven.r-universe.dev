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

#' @title Abstract class for defining model fitting method.
#'
#' @description Template to create a \code{\link[recipes]{recipe}} or
#' \code{\link{formula}} objects used in model training stage.
#'
#' @seealso \code{\link{DefaultModelFit}}, \code{\link[caret]{train}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export GenericModelFit

GenericModelFit <- R6::R6Class(
  classname = "GenericModelFit",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    initialize = function() { },
    #'
    #' @description The function is responsible of creating a
    #' \code{\link{formula}} for M.L. model.
    #'
    #' @param instances A \link{data.frame} containing the instances used to
    #' create the recipe.
    #' @param class.name A \link{character} vector representing the name of the
    #' target class.
    #' @param simplify A \link{logical} argument defining whether the formula
    #' should be generated as simple as possible.
    #'
    #' @return A \code{\link{formula}} object.
    #'
    createFormula = function(instances, class.name, simplify = TRUE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function is responsible of creating a
    #' \code{\link[recipes]{recipe}} for M.L. model.
    #'
    #' @param instances A \link{data.frame} containing the instances used to
    #' create the recipe.
    #' @param class.name A \link{character} vector representing the name of the
    #' target class.
    #'
    #' @return A object of class \code{\link[recipes]{recipe}}.
    #'
    createRecipe = function(instances, class.name) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list()
)
