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

#' @title Simple feature clustering strategy.
#'
#' @description Features are sorted by descendant according to the relevance
#' value obtained after applying an specific heuristic. Next, features are
#' distributed into N clusters following a card-dealing methodology. Finally
#' best distribution is assigned to the distribution having highest homogeneity.
#'
#' @details The strategy is suitable for all features that are valid for the
#' indicated heuristics. Invalid features are automatically grouped into a
#' specific cluster named as 'unclustered'.
#'
#' @seealso \code{\link{GenericClusteringStrategy}},
#' \code{\link{StrategyConfiguration}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export SimpleStrategy

SimpleStrategy <- R6::R6Class(
  classname = "SimpleStrategy",
  inherit = GenericClusteringStrategy,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param subset The \code{\link{Subset}} used to apply the
    #' feature-clustering strategy.
    #' @param heuristic The heuristic used to compute the relevance of each
    #' feature. Must inherit from \code{\link{GenericHeuristic}} abstract class.
    #' @param configuration Optional parameter to customize configuration
    #' parameters for the strategy. Must inherited from
    #' \code{\link{StrategyConfiguration}} abstract class.
    #'
    initialize = function(subset, heuristic, configuration = StrategyConfiguration$new()) {
      description <- paste0("SimpleStrategy is a clustering strategy whereby ",
                            "the features are sorted by descendant according ",
                            "to the relevance value obtained after applying ",
                            "an specific heuristic. Next, features are ",
                            "distributed into N clusters following a ",
                            "card-dealing methodology. Finally best ",
                            "distribution is assigned to the distribution ",
                            "having highest homogeneity. The strategy is ",
                            "suitable for all features that are valid for the ",
                            "indicated heuristics. Invalid features are ",
                            "automatically grouped into a specific cluster ",
                            "named as 'unclustered'.")
      super$initialize(subset = subset, heuristic = heuristic,
                       description = description, configuration = configuration)
    },
    #'
    #' @description Function responsible of performing the clustering
    #' strategy over the defined \code{\link{Subset}}.
    #'
    #' @param verbose A logical value to specify if more verbosity is needed.
    #'
    #' @importFrom varhandle to.dummy
    #'
    execute = function(verbose = FALSE) {
      private$all.distribution <- data.frame(k = integer(), deltha = numeric(), dist = I(list()))

      colIndex <- which(levels(as.factor(private$subset$getClassValues())) == private$subset$getPositiveClass())
      class <- varhandle::to.dummy(as.character(private$subset$getClassValues()),
                                   as.character(private$subset$getPositiveClass()))[, colIndex]

      minClusters <- private$configuration$minNumClusters()
      maxClusters <- private$configuration$maxNumClusters()

      ## COMPUTING HEURISTIC (BETWEEN EACH FEATURE AND THE CLASS)
      corpus <- private$subset$getFeatures()
      heuristic.values <- sapply(names(corpus), function(colName, class) {
        abs(private$heuristic[[1]]$heuristic(col1 = corpus[, colName], col2 = class,
                                             column.names = c(colName, private$subset$getClassName())))
      }, class)

      heuristic.valid <- heuristic.values[complete.cases(heuristic.values)]
      notHeuristic <- setdiff(names(heuristic.values), names(heuristic.valid))
      sorted.values <- heuristic.valid[order(heuristic.valid, decreasing = TRUE)]

      ## DISTRIBUTE FEATURES IN CLUSTERS (2 <= k <= maxClusters)
      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing feature clustering using '",
                 class(private$heuristic[[1]])[1], "' heuristic")
        pb <- txtProgressBar(min = 0, max = (maxClusters - 1), style = 3)
      }

      if (length(heuristic.valid) > 0) {
        for (k in minClusters:maxClusters) {
          clustering <- rep(c(1:k, (k:1)), length(sorted.values) / (2 * k) + 1)[1:length(sorted.values)]
          cluster <- vector(mode = "list", length = length(sorted.values))
          names(cluster) <- names(sorted.values)
          sumGroup <- vector(mode = "list", length = k)
          for (i in 1:k) {
            sumGroup[[i]] <- sorted.values[clustering == i]
            for (j in names(sorted.values[clustering == i])) { cluster[[j]] <- c(i) }
          }
          groupMeasure <- sapply(sumGroup, sum)
          deltha <- (max(groupMeasure) - min(groupMeasure))
          df <- data.frame(k = k, deltha = deltha, dist = I(list(cluster)))
          private$all.distribution <- rbind(private$all.distribution, df)
          if (isTRUE(verbose)) { setTxtProgressBar(pb, (k - 1)) }
        }

        if (isTRUE(verbose)) { close(pb) }

        for (i in 1:nrow(private$all.distribution)) {
          aux.dist <- unlist(private$all.distribution[i, ]$dist, recursive = FALSE)
          aux.list <- list()
          for (j in 1:private$all.distribution[i, ]$k) {
            aux.list <- append(aux.list, list(names(aux.dist[ aux.dist == j ])))
            private$all.distribution[i, ]$dist <- I(list(aux.list))
          }
        }

        bestK <- which.min(private$all.distribution$deltha)
        aux.dist <- unlist(private$all.distribution[bestK, ]$dist,
                           recursive = FALSE)
        private$best.distribution <- data.frame(cluster = integer(),
                                                dist = I(list()))
        for (i in 1:length(aux.dist)) {
          df <- data.frame(cluster = i, dist = I(list(aux.dist[[i]])))
          private$best.distribution <- rbind(private$best.distribution, df)
        }
      }
      if (length(notHeuristic) > 0) {
        message("[", class(self)[1], "][WARNING] ",
                 length(notHeuristic), " features were incompatible with '",
                 class(private$heuristic[[1]])[1], "' heuristic")
        private$not.distribution <- data.frame(cluster = 1,
                                               dist = I(list(notHeuristic)))
      } else private$not.distribution <- data.frame()
    },
    #'
    #' @description The function obtains the best clustering distribution.
    #'
    #' @return A \link{list} of clusters. Each list element represents a feature
    #' group.
    #'
    getBestClusterDistribution = function() { list(private$best.distribution) },
    #'
    #' @description The function is used to return the features that cannot be
    #' clustered due to incompatibilities with the used heuristic.
    #'
    #' @return A \link{character} vector containing the unclassified features.
    #'
    getUnclustered = function() { list(private$not.distribution) },
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
    #' @return A \link{list} with the features comprising an specific clustering
    #' distribution.
    #'
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE) {
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if (is.null(num.clusters)) {
        distribution <- lapply(private$best.distribution$dist, function(x) {x})
      } else {
        if (is.numeric(num.clusters) && (num.clusters %in% c(2:tail(private$all.distribution$k, n = 1)))) {
          distribution <- unlist(private$all.distribution[which(num.clusters == private$all.distribution$k), ]$dist, recursive = FALSE)
        } else {
          message("[", class(self)[1], "][WARNING] Number of clusters not found. ",
                  "Assuming best cluster distribution")
          distribution <- unlist(private$all.distribution[which.min(private$all.distribution$deltha), ]$dist, recursive = FALSE)
        }
      }

      if (!is.null(num.groups) && is.numeric(num.groups) &&
           num.groups %in% c(1:length(distribution))) {
        distribution <- distribution[num.groups]
      }

      if (isTRUE(include.unclustered) && nrow(private$not.distribution) > 0) {
        distribution <- append(distribution, lapply(private$not.distribution$dist,
                                                    function(x) {x}))
      }
      return(distribution)
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
    #' @return A \code{\link{Trainset}} object.
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

      summary <- data.frame(k = private$all.distribution$k,
                            dispersion = private$all.distribution$deltha,
                            row.names = NULL)
      plot <- BinaryPlot$new()$plot(summary) +
        ggplot2::labs(title = "Data") + ggplot2::theme_light() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5))
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
        message("[", class(self)[1], "][INFO] Plot has been succesfully saved ",
                "at: ", file.path(dir.path, file.name, ".pdf"))
      } else { show(plot) }
      plot
    },
    #'
    #' @description The function is used to save the clustering distribution to
    #' a CSV file.
    #'
    #' @param dir.path The name of the directory to save the CSV file.
    #' @param name Defines the name of the CSV file.
    #' @param num.clusters An optional parameter to select the number of
    #' clusters to be saved. If not defined, all cluster distributions will be saved.
    #'
    saveCSV = function(dir.path, name = NULL, num.clusters = NULL) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Path not defined. Aborting...")

      if (is.null(name)) {
        name <- class(private$heuristic[[1]])[1]
        message("[", class(self)[1], "][WARNING] File name not defined. Using '",
                name, ".csv'")
      }

      if (is.null(private$all.distribution) || nrow(private$all.distribution) == 0) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

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

      if (is.null(num.clusters)) {
        message("[", class(self)[1], "][WARNING] Number of clusters not defined. ",
                "Saving all cluster configurations")
        num.clusters <- list(2:max(private$all.distribution$k))
      } else {
        if (!is.list(num.clusters)) {
          message("[", class(self)[1], "][WARNING] Type of num.clusters not valid ",
                  "(must be NULL or list type). Saving all cluster configurations")
          num.clusters <- list(2:max(private$all.distribution$k))
        } else {
          if (any(unlist(num.clusters) > max(private$all.distribution$k))) {
            message("[", class(self)[1], "][WARNING] Number of clusters exceeds ",
                    "maximum number of clusters. Saving all cluster configurations")
            num.clusters <- list(2:max(private$all.distribution$k))
          } else {
            if (!all(unlist(num.clusters) <= max(private$all.distribution$k),
                     unlist(num.clusters) >= min(private$all.distribution$k))) {
              message("[", class(self)[1], "][WARNING] Number of clusters ",
                      "exceeds the range of minimum and maximum number of ",
                      "clusters. Saving all cluster configurations")
              num.clusters <- list(2:max(private$all.distribution$k))
            }
          }
        }
      }
      write.table(data.frame(k = private$all.distribution[private$all.distribution$k %in% unlist(num.clusters), "k"],
                             dispersion = private$all.distribution[private$all.distribution$k %in% unlist(num.clusters), "deltha"],
                             row.names = NULL),
                  file = file.path(dir.path, paste0(name, ".csv")),
                  row.names = FALSE,
                  col.names = TRUE,
                  sep = ";")
    }
  )
)
