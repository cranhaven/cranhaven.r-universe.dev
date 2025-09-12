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

#' @title Default model fitting implementation.
#'
#' @description Creates a default \code{\link[recipes]{recipe}} and
#' \code{\link{formula}} objects used in model training stage.
#'
#' @seealso \code{\link{GenericModelFit}}, \code{\link[caret]{train}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export DefaultModelFit

DefaultModelFit <- R6::R6Class(
  classname = "DefaultModelFit",
  inherit = GenericModelFit,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    initialize = function() { super$initialize() },
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
    createFormula = function(instances, class.name, simplify = FALSE) {
      if (isTRUE(simplify))
        as.formula(paste0(sprintf("`%s`", class.name), " ~ ."))
      else as.formula(paste0(paste0(sprintf("`%s`", class.name), " ~ "),
                             paste0(sprintf("`%s`", names(instances)),
                                    collapse = "+")))
    },
    #'
    #' @description The function is responsible of creating a
    #' \code{\link[recipes]{recipe}} with five operations over the data:
    #' \code{\link[recipes]{step_zv}}, \code{\link[recipes]{step_nzv}},
    #' \code{\link[recipes]{step_corr}}, \code{\link[recipes]{step_center}},
    #' \code{\link[recipes]{step_scale}}
    #'
    #' @details This function is automatically invoked by \code{\link{D2MCS}}
    #' during model training stage.
    #'
    #' @param instances A \code{\link{data.frame}} containing the instances used
    #' to create the recipe.
    #' @param class.name A \code{\link{character}} vector representing the name
    #' of the target class.
    #' @return An object of class \code{\link[recipes]{recipe}}.
    #'
    #' @import recipes
    #'
    createRecipe = function(instances, class.name) {
      recipe <- recipes::recipe(self$createFormula(instances, class.name,
                                                   simplify = TRUE),
                                data = instances)
      recipe %>% recipes::step_zv(recipes::all_predictors()) %>%
        recipes::step_nzv(recipes::all_predictors()) %>%
        recipes::step_corr(recipes::all_predictors()) %>%
        recipes::step_center(recipes::all_predictors()) %>%
        recipes::step_scale(recipes::all_predictors())
    }
  )
)
