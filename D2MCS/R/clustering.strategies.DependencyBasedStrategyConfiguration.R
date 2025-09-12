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

#' @title Custom Strategy Configuration handler for the DependencyBasedStrategy
#' strategy.
#'
#' @description Define the default configuration parameters for the
#' \link{DependencyBasedStrategy} strategy.
#'
#' @seealso \code{\link{StrategyConfiguration}},
#' \code{\link{DependencyBasedStrategy}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export DependencyBasedStrategyConfiguration

DependencyBasedStrategyConfiguration <- R6::R6Class(
  classname = "DependencyBasedStrategyConfiguration",
  inherit = StrategyConfiguration,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param binaryCutoff The \link{numeric} value of binary cutoff.
    #' @param realCutoff The \link{numeric} value of real cutoff.
    #' @param tiebreakMethod The \link{character} value of tie-break method. The
    #' two tiebreak methods available are "lfdc" (less dependence cluster with
    #' the features) and "ltdc" (less dependence cluster with the target). These
    #' methods are used to add the features in the candidate feature clusters.
    #' @param metric The \link{character} value of the metric to apply the mean
    #' to obtain the quality of a cluster. The two metrics available are
    #' "dep.tar" (Dependence of cluster features on the target) and "dep.fea"
    #' (Dependence between cluster features).
    #'
    initialize = function(binaryCutoff = 0.6,
                          realCutoff = 0.6,
                          tiebreakMethod = "lfdc",
                          metric = "dep.tar") {

      if (!(is.numeric(binaryCutoff) && binaryCutoff >= 0 && binaryCutoff <= 1)) {
        stop("[", class(self)[1], "][FATAL] Invalid binary cut-off value. ",
             "Aborting...")
      }

      if (!(is.numeric(realCutoff) && realCutoff >= 0 && realCutoff <= 1)) {
        stop("[", class(self)[1], "][FATAL] Invalid real cut-off value. ",
             "Aborting...")
      }

      if (!(is.character(tiebreakMethod) && tiebreakMethod %in% c("lfdc", "ltdc"))) {
        stop("[", class(self)[1], "][FATAL] Invalid real tiebreak method ('lfdc' ",
             "or 'ltdc'). Aborting...")
      }

      if (!(is.character(metric) && metric %in% c("dep.tar", "dep.fea"))) {
        stop("[", class(self)[1], "][FATAL] Invalid metric value ('dep.fea' or ",
             "'dep.tar'). Aborting...")
      }

      private$binaryCutoff <- binaryCutoff
      private$realCutoff <- realCutoff
      private$tiebreakMethod <- tiebreakMethod
      private$metric <- metric
    },
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
      features <- eval.parent(substitute(alist(...))[["features"]])
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
      features <- eval.parent(substitute(alist(...))[["features"]])
      lengths <- lengths(features)
      if (length(lengths) == 0) {
        max <- NULL
      } else {
        max <- max(lengths)
      }
      if (is.null(max)) { 3 } else { max }
    },
    #'
    #' @description Gets the cutoff to consider the dependency between binary
    #' features.
    #'
    #' @return The \link{numeric} value of binary cutoff.
    #'
    getBinaryCutoff = function() { private$binaryCutoff },
    #'
    #' @description Gets the cutoff to consider the dependency between real
    #' features.
    #'
    #' @return The \link{numeric} value of real cutoff.
    #'
    getRealCutoff = function() { private$realCutoff },
    #'
    #' @description Sets the cutoff to consider the dependency between binary
    #' features.
    #'
    #' @param cutoff The new \link{numeric} value of binary cutoff.
    #'
    setBinaryCutoff = function(cutoff) {
      if (!(is.numeric(cutoff) && cutoff >= 0 && cutoff <= 1)) {
        stop("[", class(self)[1], "][FATAL] Invalid binary cut-off value. ",
             "Aborting...")
      }
      private$binaryCutoff <- cutoff
    },
    #'
    #' @description Sets the cutoff to consider the dependency between real
    #' features.
    #'
    #' @param cutoff The new \link{numeric} value of real cutoff.
    #'
    setRealCutoff = function(cutoff) {
      if (!(is.numeric(cutoff) && cutoff >= 0 && cutoff <= 1)) {
        stop("[", class(self)[1], "][FATAL] Invalid real cut-off value. ",
             "Aborting...")
      }
      private$realCutoff <- cutoff
    },
    #'
    #' @description The function solves the ties between two (or more) features.
    #'
    #' @param feature A \link{character} containing the name of the feature
    #' @param clus.candidates A single or \link{numeric} vector value to
    #' identify the candidate groups to insert the feature.
    #' @param fea.dep.dist.clus A \link{list} containing the groups chosen for
    #' the features.
    #' @param corpus A \link{data.frame} containing the features of the initial
    #' data.
    #' @param heuristic The heuristic used to compute the relevance of each
    #' feature. Must inherit from \link{GenericHeuristic} abstract class.
    #' @param class A \link{character} vector containing all the values of the
    #' target class.
    #' @param class.name A \link{character} value representing the name of the
    #' target class.
    #'
    tiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus,
                        heuristic, class, class.name) {
      if (private$tiebreakMethod == "lfdc") {
        private$lfdcTiebreak(feature, clus.candidates, fea.dep.dist.clus,
                             corpus, heuristic)
      } else {
        private$ltdcTiebreak(feature, clus.candidates, fea.dep.dist.clus,
                             corpus, heuristic, class, class.name)
      }
    },
    #'
    #' @description The function determines the quality of a cluster.
    #'
    #' @param clusters A \link{list} with the feature distribution of each
    #' cluster.
    #' @param metrics A numeric \link{list} with the metrics associated to the
    #' cluster (dependency between all features and dependency between the
    #' features and the class).
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    qualityOfCluster = function(clusters, metrics) {
      mean(metrics[[private$metric]])
    },
    #'
    #' @description The function indicates if clustering is getting better as
    #' the number of them increases.
    #'
    #' @param clusters.deltha A \link{numeric} vector value with the quality
    #' values of the built clusters.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    isImprovingClustering = function(clusters.deltha) {
      clusters.deltha <- clusters.deltha * 100

      diff <- clusters.deltha[[length(clusters.deltha)]] - min(clusters.deltha)
      # If it does not worsen more than 0.01 %
      ifelse(0.01 > diff, TRUE, FALSE)

    }
  ),
  private = list(
    binaryCutoff = 0.6,
    realCutoff = 0.6,
    tiebreakMethod = "lfdc",
    metric = "dep.fea",
    getFeaturesInCluster = function(features, cluster) {
      features.return <- c()
      for (fea in names(features)) {
        if (cluster %in% features[[fea]]) {
          features.return <- c(features.return, fea)
        }
      }
      features.return
    },
    lfdcTiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus, heuristic) {
      # Search for the cluster set with less dependence on the features
      means.cluster <- list()
      for (clus in clus.candidates) {
        mean <- 0
        pos <- 0
        for (feature.cluster in private$getFeaturesInCluster(fea.dep.dist.clus, clus)) {
          result.heuristic <- abs(heuristic$heuristic(corpus[, feature],
                                                      corpus[, feature.cluster],
                                                      column.names = c(feature,
                                                                       feature.cluster)))
          mean <- (mean * pos + result.heuristic) / (pos + 1)
          pos <- pos + 1
        }
        means.cluster <- append(means.cluster, mean)
      }
      append(fea.dep.dist.clus[[feature]], clus.candidates[[which.min(means.cluster)]])
    },
    ltdcTiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus,
                            heuristic, class, class.name) {
      # Search for the cluster set with less dependence with the target
      means.cluster <- list()
      for (clus in clus.candidates) {
        mean <- 0
        pos <- 0
        for (feature.cluster in append(private$getFeaturesInCluster(fea.dep.dist.clus, clus), feature)) {
          result.heuristic <- abs(heuristic$heuristic(corpus[, feature.cluster],
                                                      class,
                                                      column.names = c(feature.cluster,
                                                                       class.name)))
          mean <- (mean * pos + result.heuristic) / (pos + 1)
          pos <- pos + 1
        }
        means.cluster <- append(means.cluster, mean)
      }
      append(fea.dep.dist.clus[[feature]], clus.candidates[[which.min(means.cluster)]])
    }
  )
)
