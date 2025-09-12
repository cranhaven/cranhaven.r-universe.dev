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

#' @title Stores the results achieved during training.
#'
#' @description This class manages the results achieved during training stage
#' (such as optimized hyperparameters, model information, utilized metrics).
#'
#' @seealso \code{\link{D2MCS}}
#'
#' @keywords datasets manip attribute programming utilities
#'
#' @import R6
#'
#' @export TrainOutput

TrainOutput <- R6::R6Class(
  classname = "TrainOutput",
  portable = TRUE,
  public = list(
    #'
    #' @description Function used to initialize the object arguments during
    #' runtime.
    #'
    #' @param models A \link{list} containing the best M.L. model for each
    #' cluster.
    #' @param class.values A \link{character} vector containing the values of
    #' the target class.
    #' @param positive.class A \link{character} with the value of the positive
    #' class.
    #'
    initialize = function(models, class.values, positive.class) {
      if (is.null(models) || !is.list(models)) {
        stop("[", class(self)[1], "][FATAL] Models parameter must be defined as ",
             "'list' type. Aborting...")
      }
      if (is.null(class.values) || !is.factor(class.values) && levels(class.values) < 2) {
        stop("[", class(self)[1], "][FATAL] Class values parameter must be defined as ",
             "'factor' type. Aborting...")
      }
      if (is.null(positive.class) || !positive.class %in% class.values) {
        stop("[", class(self)[1], "][FATAL] Positive class parameter not found. Aborting...")
      }
      private$models <- models
      private$class.values <- class.values
      private$positive.class <- positive.class
    },
    #'
    #' @description The function is used to obtain the best M.L. model of each
    #' cluster.
    #'
    #' @param metric A \link{character} vector which specifies the metric(s)
    #' used for configuring M.L. hyperparameters.
    #'
    #' @return A \link{list} is returned of class train.
    #'
    getModels = function(metric) {
      if (is.null(metric) || is.list(metric) || !metric %in% self$getMetrics()) {
        stop("[", class(self)[1], "][FATAL] Metric not defined or invalid. Aborting...")
      }
      private$models[[metric]]
    },
    #'
    #' @description The function returns the performance value of M.L. models
    #' during training stage.
    #'
    #' @param metrics A \link{character} vector which specifies the metric(s)
    #' used to train the M.L. models.
    #'
    #' @return A \link{character} vector containing the metrics used for
    #' configuring M.L. hyperparameters.
    #'
    getPerformance = function(metrics = NULL) {
      if (is.null(metrics) || !is.character(metrics) ||
          !any(metrics %in% self$getMetrics())) {
        message("[", class(self)[1], "][INFO] Metrics not defined or invalid. ",
                "Asuming all available metrics '",
                paste0(self$getMetrics(), collapse = ", "), "'")
        metrics <- self$getMetrics()
      }

      performance <- list()
      for (metric in metrics) {
        models.performance <- data.frame(matrix(ncol = 0, nrow = 1))
        for (model in self$getModels(metric)) {
          models.performance <- cbind(models.performance, model$model.performance)
        }
        models.performance <- cbind(models.performance, rowMeans(models.performance))
        names(models.performance) <- c(sprintf("[Cluster %d]", seq_len(ncol(models.performance) - 1)), " ~Mean")
        performance[[metric]] <- models.performance
      }

      performance
    },
    #'
    #' @description The function is used to save into CSV file the performance
    #' achieved by the M.L. models during training stage.
    #'
    #' @param dir.path The location to store the into a CSV file the performance
    #' of the trained M.L.
    #' @param metrics An optional parameter specifying the metric(s) used to
    #' train the M.L. models. If not defined, all the metrics used in train
    #' stage will be saved.
    #'
    savePerformance = function(dir.path, metrics = NULL) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        message("[", class(self)[1], "][INFO] Folder '", dir.path,
                "' has been succesfully created")
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      if (is.null(metrics) && !is.character(metrics) &&
          !any(metrics %in% self$getMetrics())) {
        message("[", class(self)[1], "][INFO] Metrics not defined or invalid. ",
                "Asuming all available metrics '",
                paste0(self$getMetrics(), collapse = ", "), "'")
        metrics <- self$getMetrics()
      }

      output <- data.frame(matrix(ncol = 4, nrow = 0))
      for (metric in metrics) {
        for (num.model in seq_len(length(self$getModels(metric)))) {
          model <- self$getModels(metric)[[num.model]]
          row <- c(metric, paste0("[Cluster_", num.model, "]"), model$model.name,
                   model$model.performance)
          output <- rbind(output, row, stringsAsFactors = FALSE)
        }
      }

      names(output) <- c("Measure", "Cluster", "Model", "Performance")
      path <- file.path(dir.path, "Performance_Train_Measures.csv")
      write.table(output, file = path, sep = ";", dec = ".", row.names = FALSE)
      message("[", class(self)[1], "][INFO] Performances successfully saved at: ",
              path)
    },
    #'
    #' @description The function is responsible for creating a plot to visualize
    #' the performance achieved by the best M.L. model on each cluster.
    #'
    #' @param dir.path The location to store the exported plot will be saved.
    #' @param metrics An optional parameter specifying the metric(s) used to
    #' train the M.L. models. If not defined, all the metrics used in train
    #' stage will be plotted.
    #'
    plot = function(dir.path, metrics = NULL) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        message("[", class(self)[1], "][INFO] Folder '", dir.path,
                "' has been succesfully created")
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }


      if (is.null(metrics) &&
          !is.character(metrics) &&
          !any(metrics %in% self$getMetrics())) {
        message("[", class(self)[1], "][WARNING] Metrics are invalid. ",
                "Asuming all available metrics", self$getMetrics())
        metrics <- self$getMetrics()
      }

      invisible(sapply(metrics, function(metric) {
        summary <- do.call(rbind, lapply(self$getModels(metric), function(model) {
          data.frame(model$model.name, model$model.performance,
                     stringsAsFactors = FALSE)
        }))

        summary <- cbind(data.frame(sprintf("[Cluster %s]", seq(1, nrow(summary)))),
                         summary)
        names(summary) <- c("clusters", "models", "measure")

        min.pos <- which.min(summary$measure)
        min <- data.frame(x = summary[min.pos, ]$clusters, y = min(summary[, 3]))
        max.pos <- which.max(summary$measure)
        max <- data.frame(x = summary[max.pos, ]$clusters, y = max(summary[, 3]))
        avg <- round(mean(summary$measure), digits = 2)
        remainning <- data.frame(x = summary[-c(min.pos, max.pos), ]$clusters,
                                 y = summary[-c(min.pos, max.pos), ]$measure)
        measure <- metric

        ggplot2::ggplot(summary, ggplot2::aes(clusters, measure, group = 1)) + ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::geom_text(ggplot2::aes(x, y, label = sprintf("%.3f", y)), remainning,
                             size = 3, hjust = -.4, vjust = 1.5, color = 'black') +
          ggplot2::geom_point(ggplot2::aes(x, y), min, fill = "transparent", color = "red",
                              shape = 21, size = 3, stroke = 1) +
          ggplot2::geom_text(ggplot2::aes(x, y, label = sprintf("%.3f", y)), min, size = 3,
                             hjust = -.4, vjust = 1.5, color = 'red') +
          ggplot2::geom_text(ggplot2::aes(x, y, label = sprintf("%.3f", y)), max, size = 3,
                             hjust = -.4, vjust = 1.5, color = 'blue') +
          ggplot2::geom_point(ggplot2::aes(x, y), max, fill = "transparent", color = "blue",
                              shape = 21, size = 3, stroke = 1) +
          ggplot2::geom_hline(ggplot2::aes(yintercept = avg), linetype = "twodash",
                              color = "#696969", show.legend = TRUE) +
          ggplot2::geom_text(ggplot2::aes(0, avg, label = "Average"), hjust = -.2, vjust = -1) +
          ggplot2::geom_text(ggplot2::aes(label = models), hjust = -.2, vjust = 0) +
          ggplot2::labs(x = "Model name", y = paste0(measure, " value"),
                        title = paste0("Performance benchmarking plot during training")) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
                         plot.title = ggplot2::element_text(hjust = 0.5))

        save.path <- file.path(dir.path, paste0("Performance_Train_Plot_", metric, ".pdf"))
        message("[", class(self)[1], "][INFO] Plot saved has been succesfully saved at : '",
                save.path, "'")
        ggplot2::ggsave(filename = save.path, device = "pdf")
      }))
    },
    #'
    #' @description The function returns all metrics used for configuring M.L.
    #' hyperparameters during train stage.
    #'
    #' @return A \link{character} value.
    #'
    getMetrics = function() { names(private$models) },
    #'
    #' @description The function is used to get the values of the target class.
    #'
    #' @return A \link{character} containing the values of the target class.
    #'
    getClassValues = function() { private$class.values },
    #'
    #' @description The function returns the value of the positive class.
    #'
    #' @return A \link{character} vector of size 1.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function is used to get the number of the trained M.L.
    #' models. Each cluster contains the best M.L. model.
    #'
    #' @return A \link{numeric} value or \link{NULL} training was not
    #' successfully performed.
    #'
    getSize = function() { length(names(private$models)) }
  ),
  private = list(
    models = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)
