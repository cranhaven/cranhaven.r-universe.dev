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

#' @title Abstract Feature Clustering Strategy class.
#'
#' @description Abstract class used as a template to ensure the proper
#' definition of new customized clustering strategies.
#'
#' @details The \link{GenericClusteringStrategy} is an archetype class so it
#' cannot be instantiated.
#'
#' @seealso \code{\link{Subset}}, \code{\link{GenericHeuristic}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export GenericClusteringStrategy

GenericClusteringStrategy <- R6::R6Class(
  classname = "GenericClusteringStrategy",
  public = list(
    #'
    #' @description A function responsible for creating a
    #' \link{GenericClusteringStrategy} object.
    #'
    #' @param subset A \code{\link{Subset}} object to perform the clustering strategy.
    #' @param heuristic The heuristic to be applied. Must inherit from
    #' \code{\link{GenericHeuristic}} class.
    #' @param description A \link{character} vector describing the strategy
    #' operation.
    #' @param configuration Optional customized configuration parameters for the
    #' strategy. Must inherited from \code{\link{StrategyConfiguration}}
    #' abstract class.
    #'
    initialize = function(subset, heuristic, description, configuration) {

      if (is.null(description) || !is.character(description)) {
        stop("[", class(self)[1], "][FATAL] Strategy description parameter must ",
             "be defined as 'character' type. Aborting...")
      }

      if (!inherits(subset, "Subset")) {
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }

      if (is.list(heuristic)) {
        if (length(Filter(function(x) inherits(x, "GenericHeuristic"), heuristic)) == 0) {
          stop("[", class(self)[1], "][FATAL] Adequate heuristics not found ",
               "(must inherit from 'GenericHeuristic' class). Aborting...")
        }
      } else {
        if (inherits(heuristic, "GenericHeuristic")) {
          heuristic <- list(heuristic)
        } else { stop("[", class(self)[1], "][FATAL] Heuristics is not correct ",
                      "(must inherit from 'GenericHeuristic' class). Aborting...") }
      }

      if (!inherits(configuration, "StrategyConfiguration")) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must be ",
             "inherit from 'StrategyConfiguration' class. Aborting...")
      }

      private$description <- description
      private$subset <- subset
      private$heuristic <- heuristic
      private$configuration <- configuration
    },
    #'
    #' @description The function is used to obtain the description of the
    #' strategy.
    #'
    #' @return A \link{character} vector of \link{NULL} if not defined.
    #'
    getDescription = function() { private$description },
    #'
    #' @description The function returns the heuristic applied for the
    #' clustering strategy.
    #'
    #' @return An object inherited from \code{\link{GenericClusteringStrategy}}
    #' class.
    #'
    getHeuristic = function() { private$heuristic },
    #'
    #' @description The function returns the configuration parameters used to
    #' perform the clustering strategy.
    #'
    #' @return An object inherited from \code{\link{StrategyConfiguration}}
    #' class.
    #'
    getConfiguration = function() { private$configuration },
    #'
    #' @description The function obtains the best clustering distribution.
    #'
    #' @return A \link{list} of clusters. Each list element represents a feature
    #' group.
    #'
    getBestClusterDistribution = function() { private$best.distribution },
    #'
    #' @description The function is used to return the features that cannot be
    #' clustered due to incompatibilities with the used heuristic.
    #'
    #' @return A \link{character} vector containing the unclassified features.
    #'
    getUnclustered = function() { private$not.distribution },
    #'
    #' @description Abstract function responsible of performing the clustering
    #' strategy over the defined \code{\link{Subset}}.
    #'
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #' @param ... Further arguments passed down to \code{execute} function.
    #'
    execute = function(verbose, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Abstract function used to obtain the set of features
    #' following an specific clustering distribution.
    #'
    #' @param num.clusters A \link{numeric} value to select the number of
    #' clusters (define the distribution).
    #' @param num.groups A single or \link{numeric} vector value to identify a
    #' specific group that forms the clustering distribution.
    #' @param include.unclustered A \link{logical} value to determine if
    #' unclustered features should be included.
    #'
    #' @return A \link{list} with the features comprising an specific clustering
    #' distribution.
    #'
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Abstract function in charge of creating a
    #' \code{\link{Trainset}} object for training purposes.
    #'
    #' @param subset A \code{\link{Subset}} object used as a basis to create the
    #' \link{Trainset}
    #' @param num.cluster A \link{numeric} value to select the number of
    #' clusters (define the distribution).
    #' @param num.groups A single or \link{numeric} vector value to identify a
    #' specific group that forms the clustering distribution.
    #' @param include.unclustered A \link{logical} value to determine if
    #' unclustered features should be included.
    #'
    createTrain = function(subset, num.cluster = NULL, num.groups = NULL,
                           include.unclustered = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Abstract function responsible of creating a plot to
    #' visualize the clustering distribution.
    #'
    #' @param dir.path An optional \link{character} argument to define the name
    #' of the directory where the exported plot will be saved. If not defined,
    #' the file path will be automatically assigned to the current working
    #' directory, '\code{getwd()}'.
    #' @param file.name The name of the PDF file where the plot is exported.
    #' @param ... Further arguments passed down to \code{execute} function.
    #'
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Abstract function to save the clustering distribution to a
    #' CSV file.
    #'
    #' @param dir.path The name of the directory to save the CSV file.
    #' @param name Defines the name of the CSV file.
    #' @param num.clusters An optional parameter to select the number of
    #' clusters to be saved. If not defined, all clusters will be saved.
    #'
    saveCSV = function(dir.path, name, num.clusters = NULL) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    description = NULL,
    subset = NULL,
    heuristic = NULL,
    configuration = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    not.distribution = NULL
  )
)
