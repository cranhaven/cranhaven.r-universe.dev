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

#' @title Trainning set.
#'
#' @description The \code{\link{Trainset}} is used to perform training
#' operations over M.L. models. A target class should be defined to guarantee a
#' full compatibility with supervised models.
#'
#' @details Use \code{\link{Dataset}} object to ensure the creation of a valid
#' \code{\link{Trainset}} object.
#'
#' @seealso \code{\link{Dataset}}, \code{\link{DatasetLoader}},
#' \code{\link{Subset}}, \code{\link{GenericClusteringStrategy}}
#'
#' @keywords datasets manip attribute programming utilities
#'
#' @import R6
#'
#' @export Trainset

Trainset <- R6::R6Class(
  classname = "Trainset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param cluster.dist The type of cluster distribution used as basis
    #' to build the \code{\link{Trainset}}. See
    #' \code{\link{GenericClusteringStrategy}} for more information.
    #' @param class.name Used to specify the name of the column containing the
    #' target class.
    #' @param class.values Specifies all the possible values of the target class.
    #' @param positive.class A \link{character} with the value of the
    #' positive class.
    #'
    initialize = function(cluster.dist, class.name, class.values, positive.class) {

      if (!is.vector(cluster.dist) || length(cluster.dist) == 0) {
        stop("[", class(self)[1], "][FATAL] Clusters empty or incorrect (must be a list). ",
             "Aborting...")
      }

      if (!is.factor(class.values)) {
        stop("[", class(self)[1], "][FATAL] Class.values parameter must be ",
             "defined as 'factor' type. Aborting...")
      }

      if (is.null(positive.class) || !positive.class %in% class.values) {
        stop("[", class(self)[1], "][FATAL] Positive Class parameter is incorrect. Must be '",
             paste(levels(class.values), collapse = "' '"), "'. Aborting...")
      }

      private$clusters <- cluster.dist
      private$positive.class <- positive.class
      private$class.name <- class.name
      private$class.values <- class.values
    },
    #'
    #' @description The function is used to obtain the value of the positive
    #' class.
    #'
    #' @return A \link{numeric} value with the positive class value.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function is used to return the name of the target class.
    #'
    #' @return A \link{character} vector with length 1.
    #'
    getClassName = function() { private$class.name },
    #'
    #' @description The function is used to compute all the possible target class
    #' values.
    #'
    #' @return A \link{factor} value.
    #'
    getClassValues = function() { private$class.values },
    #'
    #' @description The function returns the name of the columns comprising
    #' an specific cluster distribution.
    #'
    #' @param num.cluster A \link{numeric} value used to specify the cluster
    #' number of the cluster distribution used when creating the
    #' \code{\link{Trainset}}.
    #'
    #' @return A \link{character} vector with all column names.
    #'
    getColumnNames = function(num.cluster) {
      if (any(!is.numeric(num.cluster),
              !num.cluster %in% c(1:length(private$clusters)))) {
        stop("[", class(self)[1], "][FATAL] Position not defined or incorrect. ",
             "Must be included between 1 and ", length(private$clusters),
             ". Aborting...")
      }
      names(private$clusters[[num.cluster]])
    },
    #'
    #' @description The function returns the values of the columns comprising
    #' an specific cluster distribution. Target class is omitted.
    #'
    #' @param num.cluster A \link{numeric} value used to specify the cluster
    #' number of the cluster distribution used when creating the
    #' \code{\link{Trainset}}.
    #'
    #' @return A \link{data.frame} with the values of the features comprising
    #' the selected cluster distribution.
    #'
    getFeatureValues = function(num.cluster) {
      if (any(!is.numeric(num.cluster),
              !num.cluster %in% c(1:length(private$clusters))))
      {
        stop("[", class(self)[1], "][FATAL] Position not defined or incorrect. ",
             "Must be included between 1 and ", length(private$clusters),
             ". Aborting...")
      }
      private$clusters[[num.cluster]]
    },
    #'
    #' @description The function returns the values of the columns comprising
    #' an specific cluster distribution. Target class is included as the last
    #' column.
    #'
    #' @param num.cluster A \link{numeric} value used to specify the cluster
    #' number of the cluster distribution used when creating the
    #' \code{\link{Trainset}}.
    #'
    #' @return A \link{data.frame} with the values of the features comprising
    #' the selected cluster distribution.
    #'
    getInstances = function(num.cluster) {
      if (any(is.null(num.cluster), !is.numeric(num.cluster),
              !num.cluster %in% c(1:length(private$clusters))))
      {
        stop("[", class(self)[1], "][FATAL] Position not defined or incorrect. ",
             "Must be included between 1 and ", length(private$clusters),
             ". Aborting...")
      }
      instances <- cbind(private$clusters[[num.cluster]], private$class.values)
      names(instances)[length(instances)] <- private$class.name
      instances
    },
    #'
    #' @description The function obtains the number of groups (clusters) that
    #' forms the cluster distribution.
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getNumClusters = function() { length(private$clusters) }
  ),
  private = list(
    clusters = list(),
    class.name = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)
