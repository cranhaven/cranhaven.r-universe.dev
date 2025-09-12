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

#' @title D2MCS Classification Output.
#'
#' @description Allows computing the classification performance values achieved
#' by D2MCS. The class is automatically created when \code{\link{D2MCS}}
#' classification method is invoked.
#'
#' @seealso \code{\link{D2MCS}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export ClassificationOutput

ClassificationOutput <- R6::R6Class(
  classname = "ClassificationOutput",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param voting.schemes A \link{list} containing the voting schemes used
    #' (inherited from \code{\link{VotingStrategy}}.
    #' @param models A \link{list} containing the used \code{\link{Model}}
    #' during classification stage.
    #'
    initialize = function(voting.schemes, models) {
      if (length(voting.schemes) == 0) {
        stop("[", class(self)[1], "][FATAL] Voting Schemes not executed. ",
             "Aborting...")
      }

      if (!all(names(voting.schemes) %in% c("SingleVoting", "CombinedVoting"))) {
        stop("[", class(self)[1], "][FATAL] Voting Schemes must inherit from ",
             "'SimpleVoting' or 'CombinedVoting' classes. Aborting...")
      }

      if (!is.list(models)) {
        stop("[", class(self)[1], "][FATAL] Models parameter must be defined ",
             "as 'list' type. Aborting...")
      }

      positive.class <- unique(as.vector(unlist(sapply(voting.schemes, function(metrics) {
        sapply(metrics, function(cutoffs) {
          sapply(cutoffs, function(voting.names) {
            sapply(voting.names, function(votings) {
              votings$getFinalPred()$getPositiveClass() })
          })
        }) }
      ))))

      class.values <- unique(as.vector(unlist(sapply(voting.schemes, function(metrics) {
        sapply(metrics, function(cutoffs) {
          sapply(cutoffs, function(voting.names) {
            sapply(voting.names, function(votings) {
              votings$getFinalPred()$getClassValues() })
          })
        }) }
      ))))

      if (length(positive.class) != 1) {
        stop("[", class(self)[1], "][FATAL] Defined positive class does ",
             "not match. Aborting...")
      }

      private$voting.schemes <- voting.schemes
      private$trained.models <- models
      private$positive.class <- positive.class
      private$negative.class <- setdiff(class.values, positive.class)
      private$available$cutoffs <- unique(as.vector(unlist(sapply(private$voting.schemes,
                                                                   function(metrics) {
                                                                     sapply(metrics, function(cutoff) {
                                                                       names(cutoff) })
                                                                   }))))

      private$available$metrics <- unique(as.vector(unlist(sapply(private$voting.schemes,
                                                                   function(metrics) { names(metrics) }))))

      private$available$votings <- unique(as.vector(unlist(sapply(private$voting.schemes,
                                                                   function(metrics) {
                                                                     sapply(metrics, function(cutoff) {
                                                                       sapply(cutoff, function(voting) {
                                                                         names(voting) }) })
                                                                   }))))
    },
    #'
    #' @description The function returns the measures used during training stage.
    #'
    #' @return A \link{character} vector or \link{NULL} if training was not
    #' performed.
    #'
    getMetrics = function() { unique(names(private$trained.models)) },
    #'
    #' @description The function gets the name of the positive class used for
    #' training/classification.
    #'
    #' @return A \link{character} vector of size 1.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function compiled all the information concerning to
    #' the M.L. models used during training/classification.
    #'
    #' @param metrics A \link{character} vector defining the metrics used during
    #' training/classification.
    #'
    #' @return A \link{list} with the information of each M.L. model.
    #'
    getModelInfo = function(metrics = NULL) {
      if (missing(metrics) ||
          !is.character(metrics) ||
          !all(metrics %in% self$getMetrics())) {
        message("[", class(self)[1], "][WARNING] Metrics are not defined or invalid. ",
                "Asuming all metrics of clasification.output (",
                paste(self$getMetrics(), collapse = ","), ")")
        metrics <- self$getMetrics()
      }
      models.info <- lapply(metrics, function(metric) {
        do.call(rbind, lapply(private$trained.models[[metric]], function(model) {
          aux <- data.frame(model$model.name, model$model.performance)
          rownames(aux) <- NULL
          names(aux) <- c("Model Name", "Performance")
          aux
        }))
      })
      names(models.info) <- metrics
      models.info
    },
    #'
    #' @description The function is used to compute the performance of D2MCS.
    #'
    #' @param dir.path A \link{character} vector with location where the plot
    #' will be saved.
    #' @param test.set A \code{\link{Subset}} object used to compute the
    #' performance.
    #' @param measures A \link{character} vector with the measures to be used to
    #' compute performance value (inherited from \code{\link{MeasureFunction}}).
    #' @param voting.names A \link{character} vector with the name of the
    #' voting schemes to analyze the performance. If not defined, all the voting
    #' schemes used during classification stage will be taken into account.
    #' @param metric.names A \link{character} containing the measures used
    #' during training stage. If not defined, all training metrics used during
    #' classification will be taken into account.
    #' @param cutoff.values A \link{character} vector defining the minimum
    #' probability used to perform a a positive classification. If is not
    #' defined, all cutoffs used during classification stage will be taken into
    #' account.
    #'
    #' @return A \link{list} of performance values.
    #'
    getPerformances = function(test.set, measures, voting.names = NULL,
                               metric.names = NULL, cutoff.values = NULL) {
      if (!inherits(test.set, "Subset"))
        stop("[", class(self)[1], "][FATAL] Testset parameter must be defined ",
             "as 'Subset' type. Aborting...")

      if (!is.list(measures) || !all(sapply(measures, inherits, "MeasureFunction"))) {
        stop("[", class(self)[1], "][FATAL] Measures should be a list comprised of ",
             "'MeasureFunction' objects. Aborting...")
      }

      if (private$positive.class != test.set$getPositiveClass()) {
        stop("[", class(self)[1], "][FATAL] Positive class values missmatch. ['",
             test.set$getPositiveClass(), "' vs '",
             private$positive.class, "'] used in classification ",
             "and test respectively. Aborting... ")
      }

      real.values <- test.set$getClassValues()

      if (!all(levels(real.values) %in% union(private$positive.class,
                                              private$negative.class))) {
        stop("[", class(self)[1], "][FATAL] Predicted values and Real values ",
             "missmatch. Aborting...")
      }

      if (!is.character(cutoff.values)) cutoff.values <- as.character(cutoff.values)

      if (length(cutoff.values) != 0) {
        if (any(cutoff.values %in% private$available$cutoffs)) {
          aval.cutoffs <- intersect(private$available$cutoffs, cutoff.values)
        } else {
          message("[", class(self)[1], "][WARNING] Defined cutoffs are not ",
                  "available. Using all cutoffs")
          aval.cutoffs <- private$available$cutoffs
        }
      } else {
        message("[", class(self)[1], "][INFO] Cutoff values not defined or invalid. ",
                "Using all cutoffs")
        aval.cutoffs <- private$available$cutoffs
      }

      if (!is.null(metric.names)) {
        if (any(metric.names %in% private$available$metrics)) {
          aval.metrics <- intersect(metric.names, private$available$metrics)
        } else {
          message("[", class(self)[1], "][WARNING] Defined metrics are not available. ",
                  "Using all metrics")
          aval.metrics <- private$available$metrics
        }
      } else {
        message("[", class(self)[1], "][INFO] Metrics are not defined. ",
                "Using all metrics")
        aval.metrics <- private$available$metrics
      }

      if (!is.null(voting.names)) {
        if (any(voting.names %in% private$available$votings)) {
          aval.votings <- intersect(voting.names, private$available$votings)
        } else {
          message("[", class(self)[1], "][WARNING] Defined votings are not available. ",
                  "Using all votings")
          aval.votings <- private$available$votings
        }
      } else {
        message("[", class(self)[1], "][INFO] Votings are not defined. ",
                "Using all votings")
        aval.votings <- private$available$votings
      }

      valid.votings <- private$getVotings(aval.metrics, aval.cutoffs, aval.votings)

      performances <- list()
      if (length(valid.votings) == 0) {
        message("[", class(self)[1], "][ERROR] There are no voting schemes ",
                "with '", paste0(metric.names, collapse = ", "), " metrics and ",
                "'", paste0(aval.cutoffs, collapse = ", "), "' cutoffs.")
        return(performances)
      }

      if (!is.factor(real.values)) {
        real.values <- relevel(x = factor(real.values,
                                          levels = c(private$positive.class,
                                                     private$negative.class)),
                               ref = as.character(private$positive.class))
      }

      for (voting.name in names(valid.votings)) {
        voting <- valid.votings[[voting.name]]
        if (!(test.set$getPositiveClass() %in% voting$getFinalPred()$getClassValues())) {
          stop("[", class(self)[1], "][FATAL] Positive class mismatch '",
               test.set$getPositiveClass(), "' vs [",
               paste0(voting$getFinalPred()$getClassValues(),
                      collapse = ", "), "]")
        }

        pred.values <- voting$getFinalPred()$getRaw()
        voting.positive <- voting$getFinalPred()$getPositiveClass()
        voting.negative <- voting$getFinalPred()$getNegativeClass()

        if (!all(levels(real.values) %in% levels(pred.values))) {
          stop("[", class(self)[1], "][FATAL] Real target values and predicted ",
               "target values missmatch. Aborting...")
        }

        if (!is.factor(pred.values)) {
          pred.values <- relevel(x = factor(pred.values,
                                            levels = c(voting.positive,
                                                       voting.negative)),
                                 ref = as.character(voting.positive))
        }

        performance <- do.call(rbind, lapply(measures, function(entry, cf) {
          result <- entry$compute(cf)
          df <- data.frame(class(entry)[1], result)
          rownames(df) <- NULL
          names(df) <- c("Measure", "Value")
          df
        }, cf = ConfMatrix$new(caret::confusionMatrix(data = pred.values,
                                                      reference = real.values,
                                                      positive = as.character(private$positive.class)))))
        performances[[voting.name]] <- performance
      }
      performances
    },
    #'
    #' @description The function is used to save the computed predictions into a
    #' CSV file.
    #'
    #' @param dir.path A \link{character} vector with location where the plot
    #' will be saved.
    #' @param test.set A \code{\link{Subset}} object used to compute the
    #' performance.
    #' @param measures A \link{character} vector with the measures to be used to
    #'  compute performance value (inherited from \code{\link{MeasureFunction}}).
    #' @param voting.names A \link{character} vector with the name of the voting
    #' schemes to analyze the performance. If not defined, all the voting
    #' schemes used during classification stage will be taken into account.
    #' @param metric.names A \link{character} containing the measures used
    #' during training stage. If not defined, all training metrics used during
    #' classification will be taken into account.
    #' @param cutoff.values A \link{character} vector defining the minimum
    #' probability used to perform a a positive classification. If is not
    #' defined, all cutoffs used during classification stage will be taken into
    #' account.
    #'
    savePerformances = function(dir.path, test.set, measures, voting.names = NULL,
                                metric.names = NULL, cutoff.values = NULL) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                      dir.path, "'. Aborting... ") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      performances <- self$getPerformances(test.set, measures, voting.names = NULL,
                                           metric.names = NULL, cutoff.values = NULL)

      summary <- data.frame()
      for (pos in seq_len(length(performances))) {
        path <- names(performances)[pos]
        split <- unlist(strsplit(path, "_"))
        df <- data.frame("Voting" = split[3], "Cutoff" = split[2], performances[[pos]])
        summary <- rbind(summary, df)
      }
      save.path <- file.path(dir.path, "Classification_Performances.csv")
      write.table(summary, file = save.path, sep = ";",
                   dec = ".", row.names = FALSE)
      message("[", class(self)[1], "][INFO] Classification performance saved at: ", save.path)

    },
    #'
    #' @description The function allows to graphically visualize the computed
    #' performance.
    #'
    #' @param dir.path A \link{character} vector with location where the plot
    #' will be saved.
    #' @param test.set A \code{\link{Subset}} object used to compute the
    #' performance.
    #' @param measures A \link{character} vector with the measures to be used to
    #' compute performance value (inherited from \code{\link{MeasureFunction}}).
    #' @param voting.names A \link{character} vector with the name of the voting
    #' schemes to analyze the performance. If not defined, all the voting
    #' schemes used during classification stage will be taken into account.
    #' @param metric.names A \link{character} containing the measures used
    #' during training stage. If not defined, all training metrics used during
    #' classification will be taken into account.
    #' @param cutoff.values A \link{character} vector defining the minimum
    #' probability used to perform a positive classification. If is not defined,
    #' all cutoffs used during classification stage will be taken into account.
    #'
    #' @import ggplot2
    #'
    plotPerformances = function(dir.path, test.set, measures, voting.names = NULL,
                                metric.names = NULL, cutoff.values = NULL) {
      if (is.null(dir.path) || !is.character(dir.path))
        stop("[", class(self)[1], "][FATAL] Path not defined. Aborting...")

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                      dir.path, "'. Aborting...") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      performances <- self$getPerformances(test.set = test.set,
                                           measures = measures,
                                           voting.names = voting.names,
                                           metric.names = metric.names,
                                           cutoff.values = cutoff.values)

      for (peformance.name in names(performances)) {
        performance <- performances[[peformance.name]]
        plot <- ggplot2::ggplot(performance, ggplot2::aes(x = Measure, y = Value)) + ggplot2::geom_bar(stat = "identity") +
          ggplot2::geom_point(ggplot2::aes(shape = 15, stroke = 1)) + ggplot2::scale_shape_identity() +
          ggplot2::ggtitle("Classifier performance Benchmarking") + ggplot2::guides(fill = FALSE) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.title = ggplot2::element_blank(),
                 legend.position = "none")

        ggplot2::ggsave(paste0(file.path(dir.path, peformance.name), ".pdf"), device = "pdf",
                plot = plot, limitsize = FALSE)
        message("[", class(self)[1], "][INFO] Plot has been succesfully saved at: ",
                paste0(file.path(dir.path, peformance.name), ".pdf"))
      }
    },
    #'
    #' @description The function is used to obtain the computed predictions.
    #'
    #' @param voting.names A \link{character} vector with the name of the voting
    #' schemes to analyze the performance. If not defined, all the voting
    #' schemes used during classification stage will be taken into account.
    #' @param metric.names A \link{character} containing the measures used
    #' during training stage. If not defined, all training metrics used during
    #' classification will be taken into account.
    #' @param cutoff.values A \link{character} vector defining the minimum
    #' probability used to perform a a positive classification. If is not
    #' defined, all cutoffs used during classification stage will be taken into
    #' account.
    #' @param type A \link{character} to define which type of predictions should
    #' be returned. If not defined all type of probabilities will be returned.
    #' Conversely if "prob" or "raw" is defined then computed 'probabilistic' or
    #' 'class' values are returned.
    #' @param target A \link{character} defining the value of the positive
    #' class.
    #' @param filter A \link{logical} value used to specify if only predictions
    #' matching the target value should be returned or not. If \link{TRUE} the
    #' function returns only the predictions matching the target value.
    #' Conversely if \link{FALSE} (by default) the function returns all the
    #' predictions.
    #'
    #' @return A \code{\link{PredictionOutput}} object.
    #'
    getPredictions = function(voting.names = NULL, metric.names = NULL,
                              cutoff.values = NULL, type = NULL, target = NULL,
                              filter = FALSE) {

      if (is.null(type) || !type %in% c("raw", "prob")) {
        message("[", class(self)[1], "][WARNING] Type value is invalid. ",
                "Must be 'raw' of 'prob'. Assuming 'raw'")
        type <- "raw"
      }

      if (!isTRUE(all.equal(type, "raw")) &&
          (is.null(target) || !(target %in% private$positive.class))) {
        message("[", class(self)[1], "][WARNING] Target value does not match with",
                " actual target values: '", paste(private$positive.class,
                                                  private$negative.class,
                                                  sep = ", "), "'. Assuming '",
                private$positive.class, "' as default value")
        target <- private$positive.class
      }

      if (!is.logical(filter)) {
        message("[", class(self)[1], "][WARNING] Filter parameter must be defined ",
                "as 'logical' type. Assuming 'FALSE' as default value")
        filter <- FALSE
      }

      if (!is.character(cutoff.values)) cutoff.values <- as.character(cutoff.values)

      if (length(cutoff.values) != 0) {
        if (any(cutoff.values %in% private$available$cutoffs)) {
          aval.cutoffs <- intersect(cutoff.values, private$available$cutoffs)
        } else {
          message("[", class(self)[1], "][WARNING] Defined Cutoffs are not ",
                  "available. Using all available cutoffs")
          aval.cutoffs <- private$available$cutoffs
        }
      } else {
        message("[", class(self)[1], "][INFO] Cutoffs are not defined. ",
                "Using all available cutoffs")
        aval.cutoffs <- private$available$cutoffs
      }

      if (!is.null(metric.names)) {
        if (any(metric.names %in% private$available$metrics)) {
          aval.metrics <- intersect(metric.names, private$available$metrics)
        } else {
          message("[", class(self)[1], "][WARNING] Defined Metrics are not ",
                  "available. Using all available metrics")
          aval.metrics <- private$available$metrics
        }
      } else {
        message("[", class(self)[1], "][INFO] Metrics are not defined. ",
                "Using all available metrics")
        aval.metrics <- private$available$metrics
      }

      if (!is.null(voting.names)) {
        if (any(voting.names %in% private$available$votings)) {
          aval.votings <- intersect(voting.names, private$available$votings)
        } else {
          message("[", class(self)[1], "][WARNING] Defined Votings are not ",
                  "available. Using all available votings")
          aval.votings <- private$available$votings
        }
      } else {
        message("[", class(self)[1], "][INFO] Votings are not defined. ",
                "Using all available votings")
        aval.votings <- private$available$votings
      }

      valid.votings <- private$getVotings(aval.metrics, aval.cutoffs, aval.votings)

      if (length(valid.votings) == 0) {
        message("[", class(self)[1], "][ERROR] There are no voting schemes ",
                "with '", paste(metric.names, collapse = ", "), " metrics and ",
                "'", paste(cutoff.values, collapse = " "), "' cutoffs.")
        return(NULL)
      }

      predictions <- list()

      for (voting.name in names(valid.votings)) {
        voting <- valid.votings[[voting.name]]
        prediction <- voting$getFinalPred(type = type, target = target,
                                          filter = filter)
        predictions[[voting.name]] <- prediction
      }

      PredictionOutput$new(predictions = predictions, type = type, target = target)
    },
    #'
    #' @description The function saves the predictions into a CSV file.
    #'
    #' @param dir.path A \link{character} vector with location defining the
    #' location of the CSV file.
    #' @param voting.names A \link{character} vector with the name of the
    #' voting schemes to analyze the performance. If not defined, all the voting
    #' schemes used during classification stage will be taken into account.
    #' @param type A \link{character} to define which type of predictions should
    #' be returned. If not defined all type of probabilities will be returned.
    #' Conversely if "prob" or "raw" is defined then computed 'probabilistic' or
    #' 'class' values are returned.
    #' @param metric.names A \link{character} containing the measures used
    #' during training stage. If not defined, all training metrics used during
    #' classification will be taken into account.
    #' @param cutoff.values A \link{character} vector defining the minimum
    #' probability used to perform a positive classification. If is not defined,
    #' all cutoffs used during classification stage will be taken into account.
    #' @param target A \link{character} defining the value of the positive class.
    #' @param filter A \link{logical} value used to specify if only predictions
    #' matching the target value should be returned or not. If \link{TRUE} the
    #' function returns only the predictions matching the target value.
    #' Conversely if \link{FALSE} (by default) the function returns all the
    #' predictions.
    #'
    savePredictions = function(dir.path, voting.names = NULL, metric.names = NULL,
                               cutoff.values = NULL, type = NULL,
                               target = NULL, filter = FALSE) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Save folder not set. Aborting...")

      dir.path <- gsub("\\/$", "", dir.path)

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Folder '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FATAL] Cannot create directory '",
                      dir.path, "'. Aborting... ") }
      } else { message("[", class(self)[1], "][INFO] Folder already exists") }

      if (!is.character(cutoff.values)) cutoff.values <- as.character(cutoff.values)

      if (length(cutoff.values) != 0) {
        if (any(cutoff.values %in% private$available$cutoffs)) {
          aval.cutoffs <- intersect(cutoff.values, private$available$cutoffs)
        } else {
          message("[", class(self)[1], "][WARNING] Defined cutoffs are not ",
                  "available. Using all cutoffs")
          aval.cutoffs <- private$available$cutoffs
        }
      } else {
        message("[", class(self)[1], "][INFO] Cutoff values not defined or invalid. ",
                " Using all cutoffs")
        aval.cutoffs <- private$available$cutoffs
      }

      if (!is.null(metric.names)) {
        if (any(metric.names %in% private$available$metrics)) {
          aval.metrics <- intersect(metric.names, private$available$metrics)
        } else {
          message("[", class(self)[1], "][WARNING] Defined metrics are not available. ",
                  "Using all metrics")
          aval.metrics <- private$available$metrics
        }
      } else {
        message("[", class(self)[1], "][INFO] Metrics are not defined. ",
                "Using all metrics")
        aval.metrics <- private$available$metrics
      }

      if (!is.null(voting.names)) {
        if (any(voting.names %in% private$available$votings)) {
          aval.votings <- intersect(voting.names, private$available$votings)
        } else {
          message("[", class(self)[1], "][WARNING] Defined votings are not available. ",
                  "Using all votings")
          aval.votings <- private$available$votings
        }
      } else {
        message("[", class(self)[1], "][INFO] Votings are not defined. ",
                "Using all votings")
        aval.votings <- private$available$votings
      }

      valid.votings <- private$getVotings(aval.metrics, aval.cutoffs, aval.votings)

      if (length(valid.votings) == 0) {
        stop("[", class(self)[1], "][ERROR] There are no voting schemes ",
             "with '", paste(aval.metrics, collapse = ", "), "' metrics and ",
             "'", paste(aval.cutoffs, collapse = " "), "' cutoffs. Aborting...")
      }

      predictions <- list()
      for (voting.name in names(valid.votings)) {
        voting <- valid.votings[[voting.name]]
        prediction <- voting$getFinalPred(type = type, target = target,
                                          filter = filter)
        predictions[[voting.name]] <- prediction
      }

      for (voting.name in names(valid.votings)) {
        voting.scheme <- valid.votings[[voting.name]]

        if (is.null(type) || (!type %in% c("prob", "raw"))) {
          message("[", class(self)[1], "][INFO] Prediction type not set or invalid.")
          if (is.null(target) || !(target %in% c(private$positive.class,
                                                 private$negative.class))) {
            message("[", class(self)[1], "][INFO] Target class not set or invalid. ",
                    "Saving all predictions with all target values")
            # SAVING PROB
            path <- file.path(dir.path, paste0(voting.name, "_prob_",
                                               private$positive.class, ".csv"))
            df <- data.frame(voting.scheme$getFinalPred("prob", private$positive.class, filter = filter),
                             voting.scheme$getFinalPred("prob", private$negative.class, filter = filter))
            rownames <- rownames(df)
            df <- cbind(rownames, df)
            names(df) <- c("ID", private$positive.class, private$negative.class)
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)

            # SAVING RAW
            path <- file.path(dir.path, paste0(voting.name, "_raw_Predictions.csv"))
            df <- data.frame(voting.scheme$getFinalPred("raw", filter = filter))
            rownames <- rownames(df)
            df <- cbind(rownames, df)
            names(df) <- c("ID", "Predictions")
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)

            # SAVING COMBINED
            path <- file.path(dir.path, paste0(voting.name, "_comb_Predictions.csv"))
            prob <- data.frame(voting.scheme$getFinalPred("prob", private$positive.class, filter = filter),
                               voting.scheme$getFinalPred("prob", private$negative.class, filter = filter))
            colnames(prob) <- c(private$positive.class, private$negative.class)
            rownames <- rownames(prob)
            raw <- as.vector(t(voting.scheme$getFinalPred("raw", filter = filter)))
            df <- data.frame(matrix(ncol = 2, nrow = 0))
            for (row in 1:length(raw)) {
              df <- rbind(df, data.frame(raw[row], prob[row, raw[row]]))
            }
            df <- cbind(rownames, df)
            names(df) <- c("ID", "Prediction", "Probability")
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
          } else {
            message("[", class(self)[1], "][INFO] Saving all predictions for target ",
                    "value '", target, "'")
            for (i in c("prob", "raw")) {
              path <- file.path(dir.path, paste0(voting.name, "_",
                                                 i, "_", target,
                                                 ".csv"))
              df <- data.frame(voting.scheme$getFinalPred(i, target, filter = filter))
              names(df) <- target
              write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
            }
          }
        } else {
          message("[", class(self)[1], "][INFO] Prediction type set as '", type, "'.")
          if (is.null(target) || !(target %in% c(private$positive.class,
                                                  private$negative.class))) {
            message("[", class(self)[1], "][INFO] Target class not set or invalid. ",
                    "Saving '", type, "' predictions for all target values")
            path <- file.path(dir.path, paste0(voting.name, "_", private$positive.class,
                                               ".csv"))
            df <- data.frame(voting.scheme$getFinalPred(type, private$positive.class, filter = filter),
                             voting.scheme$getFinalPred(type, private$negative.class, filter = filter))
            names(df) <- c(private$positive.class, private$negative.class)
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
          } else {
            message("[", class(self)[1], "][INFO] Saving '", type, "' predictions ",
                    "for '", target, "'target values")
            path <- file.path(dir.path, paste0(voting.name, "_", target, ".csv"))
            df <- data.frame(voting.scheme$getFinalPred(type, target, filter = filter))
            names(df) <- target
            write.table(df, file = path, sep = ";", dec = ".", row.names = FALSE)
          }
        }
      }
    }
  ),
  private = list(
    positive.class = NULL,
    negative.class = NULL,
    voting.schemes = NULL,
    trained.models = NULL,
    available = list(),
    getVotings = function(metrics, cutoffs, votings) {
      valid.votings <- list()
      for (voting.type in private$voting.schemes) {
        voting.metrics <- intersect(names(voting.type), metrics)
        for (metric in voting.metrics) {
          voting.metric <- voting.type[[metric]]
          voting.cutoffs <- intersect(names(voting.metric), cutoffs)
          for (cutoff in voting.cutoffs) {
            voting.cutoff <- voting.metric[[cutoff]]
            voting.names <- intersect(names(voting.cutoff), votings)
            for (voting.name in voting.names) {
              voting.scheme <- voting.cutoff[[voting.name]]
              entry.name <- paste(metric, cutoff, voting.name, sep = "_")
              valid.votings[[entry.name]] <- voting.scheme
            }
          }
        }
      }
      valid.votings
    }
  )
)
