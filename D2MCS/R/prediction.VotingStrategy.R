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

#' @title Voting Strategy template.
#'
#' @description Abstract class used to define new \code{\link{SingleVoting}} and
#' \code{\link{CombinedVoting}} schemes.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{SingleVoting}},
#' \code{\link{CombinedVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export VotingStrategy

VotingStrategy <- R6::R6Class(
  classname = "VotingStrategy",
  portable = TRUE,
  public = list(
    #'
    #' @description Abstract method used to initialize the object arguments
    #' during runtime.
    #'
    initialize = function() { },
    #'
    #' @description The function returns the voting schemes that will
    #' participate in the voting strategy.
    #'
    #' @return A vector of object inheriting from \code{\link{VotingStrategy}}
    #' class.
    #'
    getVotingSchemes = function() { private$voting.schemes },
    #'
    #' @description The function is used to get the metric that will be used
    #' during the voting strategy.
    #'
    #' @return A \link{character} vector.
    #'
    getMetrics = function() { private$metrics },
    #'
    #' @description Abstract function used to implement the operation of the
    #' voting schemes.
    #'
    #' @param predictions A \code{\link{ClusterPredictions}} object containing
    #' the prediction achieved for each cluster.
    #' @param ... Further arguments passed down to \code{execute} function.
    #'
    execute = function(predictions, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function returns the name of the voting scheme.
    #'
    #' @return A \link{character} vector of size 1.
    #'
    getName = function() { class(self)[1] }
  ),
  private = list(
    voting.schemes = NULL,
    metrics = NULL
  )
)
