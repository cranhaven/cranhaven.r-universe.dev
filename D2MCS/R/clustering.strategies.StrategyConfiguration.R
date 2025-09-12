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

#' @title Default Strategy Configuration handler.
#'
#' @description Define default configuration parameters for the clustering
#' strategies.
#'
#' @details The \code{\link{StrategyConfiguration}} can be used to define the
#' default configuration parameters for a feature clustering strategy or as an
#' archetype to define new customized parameters.
#'
#' @seealso \code{\link{DependencyBasedStrategyConfiguration}}
#'
#' @import R6
#'
#' @keywords cluster manip
#'
#' @export StrategyConfiguration

StrategyConfiguration <- R6::R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    #'
    #' @description Function used to return the minimum number of clusters
    #' distributions used. By default the minimum is set in 2.
    #'
    #' @param ... Further arguments passed down to \code{minNumClusters}
    #' function.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    minNumClusters = function(...) {
      message("[", class(self)[1], "][INFO] Using default minCluster configuration: 2 clusters minimun")
      2
    },
    #'
    #' @description The function is responsible of returning the maximum number
    #' of cluster distributions used. By default the maximum number is set in 50.
    #'
    #' @param ... Further arguments passed down to \code{maxNumClusters}
    #' function.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    maxNumClusters = function(...) {
      message("[", class(self)[1], "][INFO] Using default maxCluster configuration: 50 clusters maximun")
      50
    }
  )
)
