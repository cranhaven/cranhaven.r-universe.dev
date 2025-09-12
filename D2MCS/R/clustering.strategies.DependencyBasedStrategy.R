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

#' @title Clustering strategy based on dependency between features.
#'
#' @description Features are distributed according to their independence values.
#' This strategy is divided into two steps. The first phase focuses on forming
#' groups with those features most dependent on each other. This step also
#' identifies those that are independent from all the others in the group.
#' The second step is to try out different numbers of clusters until you find
#' the one you think is best. These clusters are formed by inserting in all the
#' independent characteristics identified previously and trying to distribute
#' the features of the groups formed in the previous step in separate clusters.
#' In this way, it seeks to ensure that the features are as independent as
#' possible from those found in the same cluster.
#'
#' @details The strategy is suitable only for binary and real features. Other
#' features are automatically grouped into a specific cluster named as
#' 'unclustered'. This class requires the \code{\link{StrategyConfiguration}}
#' type object implements the following methods:
#'
#' - \code{getBinaryCutoff()}: The function is used to define the interval to
#' consider the dependency between binary features.
#'
#' - \code{getRealCutoff()}: The function allows defining the cutoff to consider
#' the dependency between real features.
#'
#' - \code{tiebreak(feature, clus.candidates, fea.dep.dist.clus, corpus,
#' heuristic, class, class.name)}: The function solves the ties between two
#' (or more) features.
#'
#' - \code{qualityOfCluster(clusters, metrics)}: The function determines the
#' quality of a cluster
#'
#' - \code{isImprovingClustering(clusters.deltha)}: The function indicates if
#' clustering is getting better as the number of them increases.
#'
#' An example of implementation with the description of each parameter is the
#' \code{\link{DependencyBasedStrategyConfiguration}} class.
#'
#' @seealso \code{\link{GenericClusteringStrategy}},
#' \code{\link{StrategyConfiguration}},
#' \code{\link{DependencyBasedStrategyConfiguration}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export DependencyBasedStrategy

DependencyBasedStrategy <- R6::R6Class(
  classname = "DependencyBasedStrategy",
  inherit = GenericClusteringStrategy,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object parameters during runtime.
    #'
    #' @param subset The \code{\link{Subset}} used to apply the feature-clustering
    #' strategy.
    #' @param heuristic The heuristic used to compute the relevance of each
    #' feature. Must inherit from \code{\link{GenericHeuristic}} abstract class.
    #' @param configuration optional parameter to customize configuration
    #' parameters for the strategy. Must inherited from
    #' \code{\link{StrategyConfiguration}} abstract class.
    #'
    initialize = function(subset, heuristic, configuration = DependencyBasedStrategyConfiguration$new()) {
      if (!inherits(subset, "Subset")) {
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }
      if (!is.list(heuristic) || length(heuristic) != 2) {
        stop("[", class(self)[1], "][FATAL] Heuristic parameter is not defined ",
             "or incorrect. Must contain two elements. Aborting...")
      }

      if (!any(sapply(heuristic, inherits, "GenericHeuristic"))) {
        stop("[", class(self)[1], "][FATAL] Defined heuristics are not correct. ",
             "Must be inherit from 'GenericHeuristic' class. Aborting...")
      }
      if (is.null(heuristic[[1]])) {
        message("[", class(self)[1], "][WARNING] Heuristic for binary data not defined")
      } else {
        message("[", class(self)[1], "][INFO] Heuristic for binary data defined ",
                "as '", class(heuristic[[1]])[1], "'")
      }

      if (is.null(heuristic[[2]])) {
        message("[", class(self)[1], "][WARNING] Heuristic for real data not defined")
      } else {
        message("[", class(self)[1], "][INFO] Heuristic for real data defined ",
                "as '", class(heuristic[[2]])[1], "'")
      }

      if (!"StrategyConfiguration" %in% class(configuration)) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must be inherit ",
             "from 'StrategyConfiguration' class. Aborting...")
      }
      if (!exists("getBinaryCutoff", configuration)) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must have ",
             "'getBinaryCutoff' method. Aborting...")
      }
      if (!exists("getRealCutoff", configuration)) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must have ",
             "'getRealCutoff' method. Aborting...")
      }
      if (!exists("tiebreak", configuration)) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must have ",
             "'tiebreak' method. Aborting...")
      }
      if (!exists("qualityOfCluster", configuration)) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must have ",
             "'qualityOfCluster' method. Aborting...")
      }
      if (!exists("isImprovingClustering", configuration)) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must have ",
             "'isImprovingClustering' method. Aborting...")
      }
      description <- paste0("DependencyBasedStrategy is a clustering strategy ",
                            "based on dependency between features. This ",
                            "strategy is divided into two steps. The first ",
                            "phase focuses on forming groups with those ",
                            "features most dependent on each other. This step ",
                            "also identifies those that are independent from ",
                            "all the others in the group. The second step is ",
                            "to try out different numbers of clusters until ",
                            "you find the one you think is best. These ",
                            "clusters are formed by inserting in all the ",
                            "independent characteristics identified ",
                            "previously and trying to distribute the features ",
                            "of the groups formed in the previous step in ",
                            "separate clusters. In this way, it seeks to ",
                            "ensure that the features are as independent as ",
                            "possible from those found in the same cluster.")

      super$initialize(subset = subset, heuristic = heuristic,
                       description = description, configuration = configuration)
    },
    #'
    #' @description Function responsible of performing the dependency-based
    #' feature clustering strategy over the defined \code{\link{Subset}}.
    #'
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #'
    execute = function(verbose = TRUE) {

      private$not.distribution[[1]] <- list()
      private$not.clus.fea[[1]] <- list()
      private$not.distribution[[2]] <- list()
      private$not.clus.fea[[2]] <- list()

      private$best.distribution <- vector(mode = "list", length = 2)
      private$all.distribution <- vector(mode = "list", length = 2)
      private$not.distribution <- vector(mode = "list", length = 2)

      binary.cutoff <- private$configuration$getBinaryCutoff()
      binary.heuristic <- private$heuristic[[1]]
      binary.data <- private$getBinaryFeatures(private$subset$getFeatures())
      ## COMPUTING HEURISTIC FOR BINARY DATA
      if (!is.null(binary.heuristic)) {
        message("[", class(self)[1], "][INFO] Using '", class(binary.heuristic)[1],
                "' heuristic to distribute binary features")
        if (ncol(binary.data) > 0) {
          private$computeGrouping(binary.data, binary.heuristic, binary.cutoff,
                                  verbose, binary = T)
          message("[", class(self)[1], "][INFO] Computing the distributions to ",
                  "binary features with the strategy")
          private$all.distribution[[1]] <- private$computeDistribution(binary.data,
                                                                       binary.heuristic,
                                                                       private$dep.fea[[1]], # dependent features grouping
                                                                       private$indep.fea[[1]],  # independent features grouping
                                                                       verbose)

          bestK <- which.min(private$all.distribution[[1]]$deltha)

          if (nrow(private$all.distribution[[1]]) == 1) {
            aux.dist <- list(unlist(private$all.distribution[[1]][bestK, ]$dist,
                                    recursive = TRUE))
          } else {
            aux.dist <- unlist(private$all.distribution[[1]][bestK, ]$dist,
                               recursive = FALSE)
          }

          private$best.distribution[[1]] <- data.frame(cluster = integer(),
                                                       dist = I(list()))

          for (i in 1:length(aux.dist)) {
            df <- data.frame(cluster = i,
                             dist = I(list(aux.dist[[i]])))
            private$best.distribution[[1]] <- rbind(private$best.distribution[[1]], df)
          }
          if (length(private$not.clus.fea[[1]]) > 0) {
            message("[", class(self)[1], "][WARNING] ",
                    length(private$not.clus.fea[[1]]), " features were incompatible with '",
                    class(private$heuristic[[1]])[1], "' heuristic")
            private$not.distribution[[1]] <- data.frame(cluster = 1,
                                                        dist = I(list(private$not.clus.fea[[1]])))
          }
        } else {
          message("[", class(self)[1], "][INFO] Not binary features for clustering")
          private$best.distribution[[1]] <- append(private$best.distribution[[1]],
                                                   list(NULL))
          private$all.distribution[[1]] <- append(private$all.distribution[[1]],
                                                  list(NULL))
        }
      } else {
        message("[", class(self)[1], "][INFO] ", class(self)[1],
                " has not heuristic to binary features. Assuming one cluster by default")
        private$all.distribution[[1]] <- data.frame(k = 1, deltha = 0,
                                                    dist = I(list(names(binary.data))))
        private$best.distribution[[1]] <- data.frame(cluster = 1,
                                                     dist = I(list(names(binary.data))))
      }

      real.cutoff <- private$configuration$getRealCutoff()
      real.heuristic <- private$heuristic[[2]]
      real.data <- private$getRealFeatures(private$subset$getFeatures())
      ## COMPUTING HEURISTIC FOR REAL DATA
      if (!is.null(real.heuristic)) {
        message("[", class(self)[1], "][INFO] Using '", class(real.heuristic)[1],
                "' heuristic to distribute real features")
        if (ncol(real.data) > 0) {
          private$computeGrouping(real.data, real.heuristic, real.cutoff,
                                  verbose, binary = F)
          message("[", class(self)[1], "][INFO] Computing the distributions to ",
                  "real features with the strategy")
          private$all.distribution[[2]] <- private$computeDistribution(real.data,
                                                                       real.heuristic,
                                                                       private$dep.fea[[2]], # dependent features grouping
                                                                       private$indep.fea[[2]],  # independent features grouping
                                                                       verbose)

          bestK <- which.min(private$all.distribution[[2]]$deltha)

          if (nrow(private$all.distribution[[2]]) == 1) {
            aux.dist <- list(unlist(private$all.distribution[[2]][bestK, ]$dist,
                                    recursive = TRUE))
          } else {
            aux.dist <- unlist(private$all.distribution[[2]][bestK, ]$dist,
                               recursive = FALSE)
          }

          private$best.distribution[[2]] <- data.frame(cluster = integer(),
                                                       dist = I(list()))

          for (i in 1:length(aux.dist)) {
            df <- data.frame(cluster = i,
                             dist = I(list(aux.dist[[i]])))
            private$best.distribution[[2]] <- rbind(private$best.distribution[[2]],
                                                    df)
          }
          if (length(private$not.clus.fea[[2]]) > 0) {
            message("[", class(self)[1], "][WARNING] ",
                    length(private$not.clus.fea[[2]]), " features were incompatible with '",
                    class(real.heuristic)[1], "' heuristic")
            private$not.distribution[[2]] <- data.frame(cluster = 1,
                                                        dist = I(list(private$not.clus.fea[[2]])))
          }

        } else {
          message("[", class(self)[1], "][INFO] Not real features for clustering")
          private$best.distribution[[2]] <- append(private$best.distribution[[2]],
                                                   list(NULL))
          private$all.distribution[[2]] <- append(private$all.distribution[[2]],
                                                  list(NULL))
        }
      } else {
        message("[", class(self)[1], "][INFO] ", class(self)[1],
                " has not heuristic to real features. Assuming one cluster by default")
        private$all.distribution[[2]] <- data.frame(k = 1, deltha = 0,
                                                    dist = I(list(names(real.data))))
        private$best.distribution[[2]] <- data.frame(cluster = 1,
                                                     dist = I(list(names(real.data))))
      }
    },
    #'
    #' @description Function used to obtain a specific cluster distribution.
    #'
    #' @param num.clusters A \link{numeric} value to select the number of
    #' clusters (define the distribution).
    #' @param num.groups A single or \link{numeric} vector value to identify a
    #' specific group that forms the clustering distribution.
    #' @param include.unclustered A \link{logical} value to determine if
    #' unclustered features should be included.
    #'
    #' @return A \link{list} with the features comprising an specific
    #' clustering distribution.
    #'
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE) {
      distribution <- list()
      if (is.null(private$best.distribution) ||
          is.null(private$all.distribution) ||
          all(sapply(private$best.distribution, is.null)) ||
          all(sapply(private$all.distribution, is.null))) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if (is.null(num.clusters) || !is.numeric(num.clusters)) {
        dist.binary <- lapply(private$best.distribution[[1]]$dist, function(x) { x })
        dist.real <- lapply(private$best.distribution[[2]]$dist, function(x) { x })
      } else {
        all.binary <- private$all.distribution[[1]]
        all.real <- private$all.distribution[[2]]

        if (length(num.clusters) >= length(private$all.distribution)) {
          num.clusters <- num.clusters[c(1:length(private$all.distribution))]
        } else {
          num.clusters <- c(num.clusters, rep(0, length(private$all.distribution) - length(num.clusters)))
        }

        if (!(num.clusters[1] %in% c(min(all.binary$k):max(all.binary$k)))) {
          message("[", class(self)[1], "][WARNING] Number of clusters incorrect. ",
                  "Must be between ", min(all.binary$k), " and ", max(all.binary$k),
                  ". Ignoring clustering for binary type features...")
          dist.binary <- NULL
        } else {
          dist.binary <- unlist(all.binary[which(all.binary$k == num.clusters[1]), ]$dist,
                                recursive = FALSE)
        }

        if (!(num.clusters[2] %in% c(min(all.real$k):max(all.real$k)))) {
          message("[", class(self)[1], "][WARNING] Number of clusters incorrect. ",
                  "Must be between ", min(all.real$k), " and ", max(all.real$k),
                  ". Ignoring clustering for real type features...")
          dist.real <- NULL
        } else {
          dist.real <- unlist(all.real[which(all.real$k == num.clusters[2]), ]$dist,
                              recursive = FALSE)
        }
      }

      if (!is.null(num.groups) && is.numeric(num.groups)) {
        if (length(num.groups) >= length(private$all.distribution)) {
          num.groups <- num.groups[c(1:length(private$all.distribution))]
        } else {
          num.groups <- c(num.groups, rep(0, length(private$all.distribution) - length(num.groups)))
        }
        if (!(num.groups[1] %in% c(1:length(dist.binary)))) {
          message("[", class(self)[1], "][WARNING] Number of clusters incorrect. ",
                  "Returning all groups...")

        } else { dist.binary <- dist.binary[num.groups[1]] }

        if (!(num.groups[2] %in% c(1:length(dist.real)))) {
          message("[", class(self)[1], "][WARNING] Number of clusters incorrect. ",
                  "Returning all groups...")

        } else { dist.real <- dist.real[num.groups[2]] }
      }

      distribution <- append(distribution, c(dist.binary, dist.real))

      if (isTRUE(include.unclustered)) {
        if (!is.null(private$not.distribution[[1]]) && nrow(private$not.distribution[[1]]) > 0) {
          distribution <- append(distribution, lapply(private$not.distribution[[1]]$dist,
                                                      function(x) {x}))
        }
        if (!is.null(private$not.distribution[[2]]) && nrow(private$not.distribution[[2]]) > 0) {
          distribution <- append(distribution, lapply(private$not.distribution[[2]]$dist,
                                                      function(x) {x}))
        }
      }

      distribution
    },
    #'
    #' @description The function is used to create a \code{\link{Trainset}}
    #' object from a specific clustering distribution.
    #'
    #' @param subset The \code{\link{Subset}} object used as a basis to create
    #' the train set (see \code{\link{Trainset}} class).
    #' @param num.clusters A \link{numeric} value to select the number of
    #' clusters (define the distribution).
    #' @param num.groups A single or \link{numeric} vector value to identify a
    #' specific group that forms the clustering distribution.
    #' @param include.unclustered A \link{logical} value to determine if
    #' unclustered features should be included.
    #'
    #' @details If \code{num.clusters} and \code{num.groups} are not defined,
    #' best clustering distribution is used to create the train set.
    #'
    createTrain = function(subset, num.clusters = NULL, num.groups = NULL,
                           include.unclustered = FALSE) {
      if (!inherits(subset, "Subset")) {
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }

      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      distribution <- self$getDistribution(num.clusters = num.clusters,
                                           num.groups = num.groups,
                                           include.unclustered = include.unclustered)

      train.dist <- lapply(distribution, function(group) {
        subset$getFeatures(feature.names = group)
      })

      Trainset$new(cluster.dist = train.dist, class.name = subset$getClassName(),
                   class.values = subset$getClassValues(),
                   positive.class = subset$getPositiveClass())
    },
    #'
    #' @description The function is responsible for creating a plot to visualize
    #' the clustering distribution.
    #'
    #' @param dir.path An optional argument to define the name of the directory
    #' where the exported plot will be saved. If not defined, the file path will
    #' be automatically assigned to the current working directory,
    #' '\code{getwd()}'.
    #' @param file.name A character to define the name of the PDF file where the
    #' plot is exported.
    #'
    #' @import ggplot2
    #' @importFrom gridExtra grid.arrange
    #'
    plot = function(dir.path = NULL, file.name = NULL) {

      binary.summary <- data.frame(k = private$all.distribution[[1]]$k,
                                   dispersion = private$all.distribution[[1]]$deltha,
                                   row.names = NULL)

      real.summary <- data.frame(k = private$all.distribution[[2]]$k,
                                 dispersion = private$all.distribution[[2]]$deltha,
                                 row.names = NULL)

      if (nrow(binary.summary) > 0 && nrow(real.summary) > 0) {
        plot <- gridExtra::grid.arrange(BinaryPlot$new()$plot(binary.summary),
                                        BinaryPlot$new()$plot(real.summary),
                                        nrow = 2, ncol = 1)
      } else {
        if (nrow(binary.summary) > 0) {
          plot <- BinaryPlot$new()$plot(binary.summary)
        } else { plot <- BinaryPlot$new()$plot(real.summary) }
      }

      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE)
          if (dir.exists(dir.path)) {
            message("[", class(self)[1], "][INFO] Directory '", dir.path,
                    "'has been succesfully created")
          } else {
            stop("[", class(self)[1], "][FATAL] Cannot create directory '", dir.path,
                 "'. Aborting...")
          }
        }
        ggplot2::ggsave(paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf",
                         plot = plot, limitsize = FALSE)
        message("[", class(self)[1], "][INFO] Plot has been succesfully saved at: ",
                file.path(dir.path, file.name, ".pdf"))
      } # else { show(plot) }
      plot
    },
    #'
    #' @description The function is used to save the clustering distribution to
    #' a CSV file.
    #'
    #' @param dir.path The name of the directory to save the CSV file.
    #' @param name Defines the name of the CSV file.
    #' @param num.clusters An optional parameter to select the number of
    #' clusters to be saved. If not defined, all cluster distributions will be
    #' saved.
    #'
    saveCSV = function(dir.path = NULL, name = NULL, num.clusters = NULL) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Path not defined. Aborting...")

      if (is.null(name)) {
        name <- paste(class(private$heuristic[[1]])[1],
                      class(private$heuristic[[2]])[1],
                      sep = "-")
        message("[", class(self)[1], "][WARNING] File name not defined. Using '",
                name, ".csv'")
      }

      if (is.null(private$all.distribution) ||
           length(private$all.distribution) == 0) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Directory '", dir.path,
                  "'has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                      dir.path, "'. Aborting...") }
      }

      if (is.null(num.clusters)) {
        message("[", class(self)[1], "][WARNING] Number of clusters not defined. ",
                 "Saving all cluster configurations")
        num.clusters <- list(list(2:max(private$all.distribution[[1]]$k)),
                             list(2:max(private$all.distribution[[2]]$k)))
      } else {
        if (!(is.list(num.clusters) && length(num.clusters) >= 0)) {
          message("[", class(self)[1], "][WARNING] Type of num.clusters not valid ",
                   "(must be NULL or list type). Saving all cluster configurations")
          num.clusters <- list(list(2:max(private$all.distribution[[1]]$k)),
                               list(2:max(private$all.distribution[[2]]$k)))
        } else {
          if (is.null(num.clusters[[1]]) || !is.list(num.clusters[[1]])) {
            num.clusters[[1]] <- list(2:max(private$all.distribution[[1]]$k))
          }
          if ((length(num.clusters) >= 1 && (!is.list(num.clusters[[2]]) || is.null(num.clusters[[2]])))) {
            num.clusters[[2]] <- list(2:max(private$all.distribution[[2]]$k))
          }
        }
      }

      all.binary <- private$all.distribution[[1]]
      all.real <- private$all.distribution[[2]]

      if (!all(unlist(num.clusters[[1]]) %in% all.binary$k)) {
        message("[", class(self)[1], "][WARNING] Number of clusters incorrect. ",
                "Must be between ", min(all.binary$k), " and ", max(all.binary$k),
                ". Ignoring clustering for binary type features...")
        dist.binary <- data.frame(k = numeric(), dispersion = numeric(),
                                  feature_type = character())
      } else {
        dist.binary <- data.frame(k = all.binary[c(all.binary$k %in% unlist(num.clusters[[1]])), "k"],
                                  dispersion = all.binary[c(all.binary$k %in% unlist(num.clusters[[1]])), "deltha"],
                                  feature_type = "binary", row.names = NULL)
      }

      if (!all(unlist(num.clusters[[2]]) %in% all.real$k)) {
        message("[", class(self)[1], "][WARNING] Number of clusters incorrect. ",
                "Must be between ", min(all.real$k), " and ", max(all.real$k),
                ". Ignoring clustering for real type features...")
        dist.real <- data.frame(k = numeric(), dispersion = numeric(),
                                feature_type = character())
      } else {
        dist.real <- data.frame(k = all.real[c(all.real$k %in% unlist(num.clusters[[2]])), "k"],
                                dispersion = all.real[c(all.real$k %in% unlist(num.clusters[[2]])), "deltha"],
                                feature_type = "real", row.names = NULL)
      }

      write.table(rbind(dist.binary, dist.real),
                  file = file.path(dir.path, paste0(name, ".csv")),
                  row.names = FALSE, col.names = TRUE, sep = ";")
    }
  ),
  private = list(
    not.clus.fea = vector(mode = "list", length = 2),
    dep.fea = vector(mode = "list", length = 2), # dependentFeatures
    indep.fea = vector(mode = "list", length = 2), # independentFeatures
    computeDistribution = function(corpus, heuristic, dep.fea.groups, indep.fea.list, verbose) {
      # Initializing variables ----

      heu <- heuristic
      cluster.data <- data.frame(k = integer(),
                                 deltha = numeric(),
                                 dist = I(list()))

      min.num.clusters <- private$configuration$minNumClusters(features = dep.fea.groups)
      max.num.clusters <- private$configuration$maxNumClusters(features = dep.fea.groups)

      mean.indep.fea <- c()
      mean.indep.tar <- c()

      positive.class <- private$subset$getPositiveClass()
      class.values <- private$subset$getClassValues()
      col.index <- which(levels(class.values) == positive.class)
      class <- varhandle::to.dummy(as.character(class.values),
                                   as.character(positive.class))[, col.index]

      class.name <- private$subset$getClassName()

      all.fea.dep <- unique(unlist(dep.fea.groups))

      # Computing the metrics with independent features ----

      message("[", class(self)[1], "][INFO] Computing metric of dependency between ",
              "independent features and the target")
      message("[", class(self)[1], "][INFO] Computing metric of dependency between ",
              "the ", length(indep.fea.list), " independent features")

      if (length(indep.fea.list) > 1) {
        if (isTRUE(verbose)) {
          pb <- txtProgressBar(min = 0, max = (length(indep.fea.list)), style = 3)
        }

        for (feature1 in 1:(length(indep.fea.list) - 1)) {
          # Updates metric of dependency between features and target
          result.heuristic <- abs(heu$heuristic(corpus[, indep.fea.list[[feature1]]],
                                                class,
                                                column.names = c(indep.fea.list[[feature1]],
                                                                 class.name)))

          mean.indep.tar <- c(mean.indep.tar, result.heuristic)
          # Updates metric of dependency between features
          for (feature2 in (feature1 + 1):length(indep.fea.list)) {
            result.heuristic <- abs(heu$heuristic(corpus[, indep.fea.list[[feature1]]],
                                                  corpus[, indep.fea.list[[feature2]]],
                                                  column.names = c(indep.fea.list[[feature1]],
                                                                   indep.fea.list[[feature2]])))
            mean.indep.fea <- c(mean.indep.fea, result.heuristic)
          }
          if (isTRUE(verbose)) { setTxtProgressBar(pb, (feature1 - 1)) }
        }

        # Computing metric between class and the last independent feature
        result.heuristic <- abs(heu$heuristic(corpus[, indep.fea.list[[length(indep.fea.list)]]],
                                              class,
                                              column.names = c(names(corpus)[indep.fea.list[[length(indep.fea.list)]]],
                                                               className)))
        if (isTRUE(verbose)) { setTxtProgressBar(pb, (length(indep.fea.list))) }
        if (isTRUE(verbose)) { close(pb) }
        mean.indep.tar <- mean(c(mean.indep.tar, result.heuristic))
        mean.indep.fea <- mean(mean.indep.fea)
      } else {
        if (length(indep.fea.list) == 1) {
          mean.indep.tar <- 1
          mean.indep.fea <- 1
        } else {
          mean.indep.tar <- 0
          mean.indep.fea <- 0
        }
      }

      message("[", class(self)[1], "][INFO] Metric of dependency between independent ",
              "features and the target: ", mean.indep.tar)
      message("[", class(self)[1], "][INFO] Metric of dependency between independent ",
              "features: ", mean.indep.fea)
      message("---------------------------------------------------------------")

      # Clustering dependent features... ----
      message("[", class(self)[1], "][INFO] Start of clustering dependent features...")
      message("[", class(self)[1], "][INFO] Independent features: ", length(indep.fea.list))
      message("[", class(self)[1], "][INFO] Dependent features: ", length(dep.fea.groups))
      message("[", class(self)[1], "][INFO] A total of ", length(dep.fea.groups),
              " groups of dependent variables have been generated")

      if (length(dep.fea.groups) == 0) {
        message("[", class(self)[1], "][INFO] Creating only a one cluster ",
                "because all features are independent")

        # Initializing metrics of independent features
        metrics.indep <- list(
          dep.fea = vector(mode = "numeric", length = 1), # dependencyFeatures
          dep.tar  = vector(mode = "numeric", length = 1) # dependencyTarget
        )
        names(metrics.indep$dep.fea) <- paste("Cluster 1")
        names(metrics.indep$dep.tar) <- paste("Cluster 1")
        ## Updates metric of dependency between features
        metrics.indep[["dep.fea"]][[1]] <- mean.indep.fea
        ## Updates metric of dependency between features and target
        metrics.indep[["dep.tar"]][[1]] <- mean.indep.tar

        metrics <- list(
          dep.fea = vector(mode = "numeric", length = 1), # dependencyFeatures
          dep.tar  = vector(mode = "numeric", length = 1) # dependencyTarget
        )
        metrics$dep.fea <- c(metrics.indep$dep.fea)
        metrics$dep.tar <- c(metrics.indep$dep.tar)

        # Quality of the distribution of features between the clusters
        quality.cluster <- private$configuration$qualityOfCluster(indep.fea.list,
                                                                  metrics)

        message("[", class(self)[1], "][INFO] Added independent features to all clusters")
        cluster.data <- rbind(cluster.data,
                              data.frame(k = 1,
                                         deltha = quality.cluster,
                                         dist = I(list(indep.fea.list))))
      } else {

        message("[", class(self)[1], "][INFO] Checking set of clusters: ",
                min.num.clusters, ":", max.num.clusters)
        message("[", class(self)[1], "][INFO] Starting the distribution in clusters...")
        for (current.num.cluster in min.num.clusters:max.num.clusters) {
          message("[", class(self)[1], "][INFO] Checking next set of clusters: ",
                  current.num.cluster, "/", max.num.clusters)
          ########################## Independent features...#########################
          # Initializing clusters with independent features ----

          fea.indep.dist.clus <- vector(mode = "list", length = length(indep.fea.list))
          fea.indep.dist.clus <- rep_len(list(list()), length(indep.fea.list))
          names(fea.indep.dist.clus) <- indep.fea.list
          ## Adds independent features to all clusters
          fea.indep.dist.clus <- lapply(fea.indep.dist.clus, function(fea, current.num.cluster) {
            list(1:current.num.cluster)
          }, current.num.cluster)

          # Initializing metrics of independent features
          metrics.indep <- list(
            dep.fea = vector(mode = "numeric", length = current.num.cluster), # dependencyFeatures
            dep.tar  = vector(mode = "numeric", length = current.num.cluster) # dependencyTarget
          )
          names(metrics.indep$dep.fea) <- paste("Cluster", 1:current.num.cluster)
          names(metrics.indep$dep.tar) <- paste("Cluster", 1:current.num.cluster)
          ## Updates metric of dependecy between features
          metrics.indep[["dep.fea"]] <- sapply(metrics.indep[["dep.fea"]], function(fea, current.num.cluster) {
            mean.indep.fea
          }, current.num.cluster)
          ## Updates metric of dependency between features and target
          metrics.indep[["dep.tar"]] <- sapply(metrics.indep[["dep.tar"]], function(fea, current.num.cluster) {
            mean.indep.tar
          }, current.num.cluster)
          message("[", class(self)[1], "][INFO] Added independent features to all clusters")

          # Dependent features
          # Initializing clusters of dependent features ----

          fea.dep.dist.clus <- vector(mode = "list", length = length(all.fea.dep))
          fea.dep.dist.clus <- rep_len(list(list()), length(all.fea.dep))
          names(fea.dep.dist.clus) <- all.fea.dep
          metrics.dep <- list(
            dep.fea = vector(mode = "numeric", length = current.num.cluster), # dependencyFeatures
            dep.tar  = vector(mode = "numeric", length = current.num.cluster) # dependencyTarget
          )
          names(metrics.dep$dep.fea) <- paste("Cluster", 1:current.num.cluster)
          names(metrics.dep$dep.tar) <- paste("Cluster", 1:current.num.cluster)
          # Adding feature ----
          ## All features that have dependency are checked

          if (length(all.fea.dep) > 0) {
            if (isTRUE(verbose)) {
              message("[", class(self)[1], "][INFO] Beginning add features")
              pb <- txtProgressBar(min = 0, max = (length(all.fea.dep)), style = 3)
            }
            for (fea in all.fea.dep) {
              if (isTRUE(verbose)) { setTxtProgressBar(pb, (which(fea == all.fea.dep))) }
              ## Obtains the subgroups where feature are in the list of dependent feature groups
              pos.groups.list <- lapply(names(dep.fea.groups), function(pos, fea, dep.fea.groups) {
                if (fea %in% dep.fea.groups[[pos]]) { pos }
              }, fea, dep.fea.groups)
              pos.groups.list <- as.integer(pos.groups.list[lengths(pos.groups.list) != 0])
              clus.candidates <- 1:current.num.cluster
              ## Groups containing the current characteristic are traversed to
              ## get the candidate clusters
              for (group in dep.fea.groups[pos.groups.list]) {
                for (fea.group in group) {
                  if (fea.group == fea) {
                    next
                  }
                  clus.candidates <- setdiff(clus.candidates,
                                             fea.dep.dist.clus[[fea.group]])
                  if (length(clus.candidates) == 0) {
                    break
                  }
                }
              }
              ## If there is a single candidate cluster, the element is introduced into it
              if (length(clus.candidates) == 1) {
                fea.dep.dist.clus[[fea]] <- c(fea.dep.dist.clus[[fea]],
                                              clus.candidates[[1]])
              } else {
                ## In the case that no candidate clusters have been found, it is looking
                ## for the best option among all.
                if (length(clus.candidates) == 0) {
                  clus.candidates <- 1:current.num.cluster
                }
                fea.dep.dist.clus[[fea]] <- private$configuration$tiebreak(fea,
                                                                           clus.candidates,
                                                                           fea.dep.dist.clus,
                                                                           corpus,
                                                                           heu,
                                                                           class,
                                                                           class.name)
              }
              # Computing metrics of dependent features on clusters ----
              clus.recal.metrics <- fea.dep.dist.clus[[fea]]
              result.heuristic.tar <- abs(heu$heuristic(corpus[, fea],
                                                        class,
                                                        column.names = c(fea,
                                                                         class.name)))
              for (clus in clus.recal.metrics) {
                ## Update metric of dependency between features and target
                numerator <- metrics.dep[["dep.tar"]][[clus]] * (length(which(unlist(fea.dep.dist.clus) == clus)) - 1) + result.heuristic.tar
                denominator <- length(which(unlist(fea.dep.dist.clus) == clus))
                metrics.dep[["dep.tar"]][[clus]] <- numerator / denominator
                mean.fea <- c()
                for (fea.clus in names(fea.dep.dist.clus)) {
                  if (clus %in% fea.dep.dist.clus[[fea.clus]]) {
                    result.heuristic.fea <- abs(heu$heuristic(corpus[, fea],
                                                              corpus[, fea.clus],
                                                              column.names = c(fea,
                                                                               fea.clus)))
                    mean.fea <- c(mean.fea, result.heuristic.fea)
                  }
                }
                # Update metric of dependency between features of the cluster
                numerator <- metrics.dep[["dep.fea"]][[clus]] * (length(which(unlist(fea.dep.dist.clus) == clus)) - 1) + mean(mean.fea)
                denominator <- length(which(unlist(fea.dep.dist.clus) == clus))
                metrics.dep[["dep.fea"]][[clus]] <- numerator / denominator
              }
            }
            if (isTRUE(verbose)) { setTxtProgressBar(pb, (length(all.fea.dep))) }
            if (isTRUE(verbose)) { close(pb) }
          }
          # Computing final distribution ----
          message("[", class(self)[1], "][INFO] Added the ", length(all.fea.dep),
                  " dependent features to all clusters")

          metrics <- list(
            dep.fea = vector(mode = "numeric", length = current.num.cluster), # dependencyFeatures
            dep.tar  = vector(mode = "numeric", length = current.num.cluster) # dependencyTarget
          )
          metrics$dep.fea <- c(metrics.dep$dep.fea,
                               metrics.indep$dep.fea)
          metrics$dep.tar <- c(metrics.dep$dep.tar,
                               metrics.indep$dep.tar)

          features.dis.current.cluster <- append(fea.dep.dist.clus,
                                                 fea.indep.dist.clus)

          final.dist.current.cluster <- vector(mode = "list", length = length(1:current.num.cluster))
          for (fea in names(features.dis.current.cluster)) {
            clus.dist <- features.dis.current.cluster[[fea]]

            for (clus in unlist(clus.dist)) {
              final.dist.current.cluster[[clus]] <- append(final.dist.current.cluster[[clus]], fea)
            }
          }
          aux <- paste0("[", class(self)[1], "][INFO] Number of clusters:\t")
          for (clus in 1:length(final.dist.current.cluster)) {
            aux <- paste0(aux, clus, "\t")
          }

          aux <- paste0("[", class(self)[1], "][INFO] Number of features:\t")
          for (clus in 1:length(final.dist.current.cluster)) {
            aux <- paste0(aux, length(final.dist.current.cluster[[clus]]), "\t")
          }

          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " independencyTarget: ", mean(metrics.indep[["dep.tar"]]))
          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " independencyFeatures: ", mean(metrics.indep[["dep.fea"]]))
          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " dependencyTarget: ", mean(metrics.dep[["dep.tar"]]))
          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " dependencyFeatures: ", mean(metrics.dep[["dep.fea"]]))
          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " Target: ", mean(metrics[["dep.tar"]]))
          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " Features: ", mean(metrics[["dep.fea"]]))

          # Quality of the distribution of features between the clusters
          quality.cluster <- private$configuration$qualityOfCluster(final.dist.current.cluster,
                                                                    metrics)
          message("[", class(self)[1], "][INFO] Metric of clusters ", current.num.cluster,
                  " : ", quality.cluster)

          cluster.data <- rbind(cluster.data,
                                data.frame(k = current.num.cluster,
                                           deltha = quality.cluster,
                                           dist = I(list(final.dist.current.cluster))))
          # Checking the progression of the groups, taking into account deltha ----
          clusters.deltha <- c()
          clusters.deltha <- cluster.data$deltha
          names(clusters.deltha) <- cluster.data$k
          if (!private$configuration$isImprovingClustering(clusters.deltha)) {
            message("[", class(self)[1], "][INFO] Clustering is not considered to ",
                    "improve from the number of clusters: ", current.num.cluster)
            message("[", class(self)[1], "][WARNING] Stopping to check the following ",
                    "clusters (", min.num.clusters, ":", max.num.clusters, ")")
            return(cluster.data)
          }
        }
      }
      cluster.data
    },
    computeGrouping = function(corpus, heuristic, cutoff, verbose, binary = TRUE) {
      names.corpus <- names(corpus)
      heu <- heuristic
      dep.fea <- list()
      indep.fea <- list()
      not.clus.fea <- list()
      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing feature grouping through ",
                "dependency between them using '", class(heu)[1], "' heuristic")
      }
      for (i in 1:(length(corpus) - 1)) {
        for (j in (i + 1):length(corpus)) {
          val.heu <- abs(heu$heuristic(corpus[, i],
                                       corpus[, j],
                                       column.names = c(names.corpus[i],
                                                        names.corpus[j])))
          # INVALID VALUE OF HEURISTIC, FEATURE IS DISCARDED
          if (is.na(val.heu)) {
            not.clus.fea <- append(not.clus.fea,
                                   names.corpus[i])
            break
          }
          if (val.heu >= cutoff) {
            added <- FALSE
            included <- FALSE
            pos.list <- 1
            while (pos.list < length(dep.fea)) {
              # If "i" and "j" are not in the current group, continue the checking to
              # the next group.
              if (names.corpus[[i]] %in% dep.fea[[pos.list]] ||
                   names.corpus[[j]] %in% dep.fea[[pos.list]]) {
                # If there is "i" in the sublist, checks the dependent.
                if (names.corpus[[i]] %in% dep.fea[[pos.list]]) {
                  # If "j" is not included in the group, checks the dependence with the
                  # all elements on the list.
                  if (!names.corpus[[j]] %in% dep.fea[[pos.list]]) {
                    for (value.sub.list in  dep.fea[[pos.list]]) {
                      dependent.in.the.list <- TRUE
                      if (names.corpus[[i]] != value.sub.list) {
                        val.heu <- abs(heu$heuristic(corpus[, j],
                                                      corpus[, value.sub.list],
                                                      column.names = c(names.corpus[j],
                                                                       names.corpus[value.sub.list])))
                        if (is.na(val.heu)) {
                          not.clus.fea <- append(not.clus.fea,
                                                 names.corpus[j])
                          break
                        }
                        if (val.heu < cutoff) {
                          dependent.in.the.list <- FALSE
                          break
                        }
                      }
                    }
                    # If "j" is dependent with all elements on the list and it is not
                    # included on the subgroup, it is added to the sublist.
                    if (isTRUE(dependent.in.the.list) &
                         !names.corpus[[j]] %in% dep.fea[[pos.list]]) {
                      dep.fea[[pos.list]] <- append(dep.fea[[pos.list]],
                                                    names.corpus[[j]])
                      added <- TRUE
                      if (isTRUE(verbose)) {
                        message("[", class(self)[1], "][INFO] Added: ",
                                names.corpus[[j]], " ",
                                "to an existent group. Group ",
                                pos.list, " ",
                                "(Current ", ifelse(binary, "binary", "real"),
                                " column: ", i, ")")

                      }
                    }
                  } else {
                    included <- TRUE
                  }
                }
              }
              pos.list <- pos.list + 1
            }
            # If there are not any group which all colums dependent with "i" and "j".
            # And, it is already included in some group.
            if (isFALSE(added) & isFALSE(included)) {
              aux <- vector(mode = "list", length = 1)
              aux[[1]] <- append(aux[[1]],
                                 names.corpus[[i]])
              aux[[1]] <- append(aux[[1]],
                                 names.corpus[[j]])
              dep.fea <- append(dep.fea,
                                aux)
              if (isTRUE(verbose)) {
                message("[", class(self)[1], "][INFO] New group (",
                        length(dep.fea), "): ",
                        names(corpus)[[i]], " - ",
                        names.corpus[[j]], " ",
                        "(Current ", ifelse(binary, "binary", "real"),
                        " column: ", i, ")")
              }
            }
          }
        }

        if (is.na(val.heu)) {
          not.clus.fea <- append(not.clus.fea,
                                 names.corpus[i])
          if (isTRUE(verbose)) {
            message("[", class(self)[1], "][INFO] Column name: '",
                    names.corpus[[i]], "' is no clustering ",
                    "(Current ", ifelse(binary, "binary", "real"),
                    " column: ", i, ")")
          }
          next
        }

        if (!names.corpus[[i]] %in% unlist(dep.fea)) {
          indep.fea <- append(indep.fea,
                              names.corpus[[i]])
          if (isTRUE(verbose)) {
            message("[", class(self)[1], "][INFO] Column name: '",
                    names.corpus[[i]], "' is independent ",
                    "(Current ", ifelse(binary, "binary", "real"),
                    " column: ", i, ")")
          }
        } else {
          if (isTRUE(verbose)) {
            message("[", class(self)[1], "][INFO] Column name: '",
                    names.corpus[[i]], "' is dependent ",
                    "(Current ", ifelse(binary, "binary", "real"),
                    " column: ", i, ")")
          }
        }
      }

      # CHECKING LAST FEATURES
      if (!names(corpus)[[length(corpus)]] %in% unlist(dep.fea)) {
        indep.fea <- append(indep.fea,
                            names.corpus[[length(corpus)]])
        if (isTRUE(verbose)) {
          message("[", class(self)[1], "][INFO] Column name: '",
                  names.corpus[[length(corpus)]], "' is independent ",
                  "(Current ", ifelse(binary, "binary", "real"),
                  " column: ", length(corpus), ")")
        }
      } else {
        if (isTRUE(verbose)) {
          message("[", class(self)[1], "][INFO] Column name: '",
                  names.corpus[[length(corpus)]], "' is dependent ",
                  "(Current ", ifelse(binary, "binary", "real"),
                  " column: ", length(corpus), ")")
        }
      }
      if (isTRUE(binary)) {
        private$indep.fea[[1]] <- indep.fea
        private$dep.fea[[1]] <- dep.fea
        private$not.clus.fea[[1]] <- append(private$not.clus.fea[[1]],
                                            not.clus.fea)
        if (length(private$dep.fea[[1]]) != 0) {
          names(private$dep.fea[[1]]) <- 1:length(private$dep.fea[[1]])
        }
        if (length(private$indep.fea[[1]]) != 0) {
          names(private$indep.fea[[1]]) <- 1:length(private$indep.fea[[1]])
        }
      } else {
        private$indep.fea[[2]] <- indep.fea
        private$dep.fea[[2]] <- dep.fea
        private$not.clus.fea[[2]] <- append(private$not.clus.fea[[2]],
                                            not.clus.fea)
        if (length(private$dep.fea[[2]]) != 0) {
          names(private$dep.fea[[2]]) <- 1:length(private$dep.fea[[2]])
        }
        if (length(private$indep.fea[[2]]) != 0) {
          names(private$indep.fea[[2]]) <- 1:length(private$indep.fea[[2]])
        }
      }
    },
    getBinaryFeatures = function(data) {
      Filter(function(x) { length(levels(factor(x))) == 2 }, data)
    },
    getRealFeatures = function(data) {
      Filter(function(x) { !length(levels(factor(x))) == 2 }, data)
    }
  )
)
