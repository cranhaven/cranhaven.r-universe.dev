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

#' @title Stores a previously trained M.L. model.
#'
#' @description Encapsulates and handles all the information and operations
#' associated with a M.L. model.
#'
#' @keywords internal misc
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{TrainFunction}}
#'
#' @import R6
#'
#' @export Model
#'
Model <- R6::R6Class(
  classname = "Model",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param dir.path The location were the executed models will be saved.
    #' @param model A \code{\link{Model}} object.
    #'
    initialize = function(dir.path, model) {
      private$dir.path <- gsub("\\/$", "", dir.path)
      if (!dir.exists(private$dir.path)) {
        message("[", class(self)[1], "][INFO] Save directory not exist. Creating...")
        dir.create(private$dir.path, showWarnings = FALSE, recursive = TRUE)
        if (!dir.exists(private$dir.path))
          stop("[", class(self)[1], "][FATAL] Path '", private$dir.path,
               "' cannot be created. Aborting...")
      }

      if (is.null(model)) {
        stop("[", class(self)[1], "][FATAL] Model was not defined. ",
             "Aborting...")
      }

      private$RDS.path <- file.path(private$dir.path, paste0(model$name, ".rds"))
      private$model.info <- model
      private$model.train <- list(model.name = model$name, exec.time = NULL,
                                   model.performance = NULL, model.data = NULL,
                                   model.libs = model$library)
      private$metric <- NULL

      if (file.exists(private$RDS.path)) {
        message("[", class(self)[1], "][INFO] Model '", private$model.info$name,
                "' already exists. Loading...")
        private$model.train <- readRDS(private$RDS.path)

        if (is.null(private$model.train) ||
            any(sapply(private$model.train, is.null)) ||
            !inherits(private$model.train, "list") ||
            length(private$model.train) != 5) {
          message("[", class(self)[1], "][ERROR] Unable to load trained model. ",
                  "Task not performed")
        } else {
          message("[", class(self)[1], "][INFO] '",
                  paste(private$model.info[1:3], collapse = "', "),
                  "' has been succesfully loaded!")
        }
      }
    },
    #'
    #' @description The function is used to determine is the model has been
    #' already trained.
    #'
    #' @return A \link{logical} value. \link{TRUE} if the model
    #' has been trained and \link{FALSE} otherwise.
    #'
    isTrained = function() {
      ifelse(is.null(private$model.train$model.data), FALSE, TRUE)
    },
    #'
    #' @description The function returns the location path of the specific
    #' model.
    #'
    #' @return A \link{character} vector specifying the location of the
    #' model.
    #'
    getDir = function() { private$dir.path },
    #'
    #' @description The function is used to obtain the name of the model.
    #'
    #' @return A \link{character} vector with the name of the model.
    #'
    getName = function() { private$model.info$name },
    #'
    #' @description The function gets the family of the model.
    #'
    #' @return A \link{character} vector representing the family of the ML
    #' model.
    #'
    getFamily = function() { private$model.info$family },
    #'
    #' @description The function allows obtaining the description associated
    #' with an specific ML model.
    #'
    #' @return A \link{character} vector with the model description.
    #'
    getDescription = function() { private$model.info$description },
    #'
    #' @description The function is responsible of performing model training
    #' operation.
    #'
    #' @param train.set A \link{data.frame} with the data used for training the
    #' model.
    #' @param fitting The model fitting formula. Must inherit from
    #' \code{\link{GenericModelFit}} class.
    #' @param trFunction An object inherited from \code{\link{TrainFunction}}
    #' used to define how the training acts.
    #' @param metric A \link{character} vector containing the metrics used to
    #' optimized model parameters.
    #' @param logs A \link{character} vector containing the path to store the
    #' error logs.
    #'
    #' @import caret tictoc
    #'
    train = function(train.set, fitting, trFunction, metric, logs) {
      if (is.null(private$model.train) ||
          any(sapply(private$model.train, is.null))) {
        message("[", class(self)[1], "][INFO][", self$getName(), "] Model ",
                "has not been trained. Starting training process...")

        if (!inherits(train.set, "data.frame")) {
          stop("[", class(self)[1], "][FATAL][", self$getName(), "] ",
               "Cannot perform trainning stage. ",
               "Train set must be defined as 'data.frame' type. Aborting...")
        }

        if (nrow(train.set) == 0) {
          stop("[", class(self)[1], "][FATAL][", self$getName(), "] ",
               "Cannot perform trainning stage. Train set is empty. Aborting...")
        }

        if (!inherits(trFunction, "TrainFunction")) {
          stop("[", class(self)[1], "][FATAL][", self$getName(), "] ",
               "TrainFunction must be inherits from 'TrainFunction' class. ",
               "Aborting...")
        }

        valid.metrics <- trFunction$getMeasures()
        if (any(is.null(metric), !(metric %in% valid.metrics))) {
          stop("[", class(self)[1], "][FATAL][", self$getName(), "] ",
               "Metric is not defined or unavailable. ",
               "Must be a [", paste(valid.metrics, collapse = ", "), "] type. ",
               "Aborting...")
        }

        message("[", class(self)[1], "][INFO][", self$getName(), "] ",
                "Performing training and hyperparameter optimization stage...")

        tryCatch({
          private$metric <- metric
          tictoc::tic(quiet = TRUE)
          set.seed(trFunction$getSeed())
          private$model.train$model.data <- caret::train(x = fitting, data = train.set,
                                                         method = private$model.info$name,
                                                         trControl = trFunction$getTrFunction(),
                                                         metric = metric)
          time <- tictoc::toc(quiet = TRUE)
          private$model.train$model.performance <- self$getPerformance()

          if (!is.null(private$model.train$model.data)) {
            message("[", class(self)[1], "][INFO][", self$getName(), "] ",
                    "Finished in [", round(time$toc - time$tic, digits = 2), " segs]")
            private$model.train$exec.time <- (time$toc - time$tic)
          } else {
            message("[", class(self)[1], "][ERROR][", self$getName(), "] ",
                    "Unable to train model. Task not performed")
          }
        }, error = function(err) {
          message("[", class(self)[1], "][ERROR][", self$getName(), "] Model ",
                  "could not be trained for current data. See '", logs,
                  "' for more information.")
          cat(paste0(format(Sys.time(), "%H:%m:%S %d/%m/%Y"),
                             ": [", class(self)[1], "][", self$getName(), "] ", err),
                      file = file.path(logs, "error.log"), append = TRUE)

          private$model.train$model.performance <- 0.0
          private$model.train$exec.time <- 0.0
        })
      } else {
        message("[", class(self)[1], "][INFO][", self$getName(), "] ",
                "Model has already been trained")
      }
    },
    #'
    #' @description The function allows obtaining the trained model.
    #'
    #' @return A \code{\link[caret]{train}} class.
    #'
    getTrainedModel = function() {
      if (!self$isTrained()) {
        message("[", class(self)[1], "][WARNING] Model '", private$model.info$name,
                "' is not trained. Task not performed")
        NULL
      } else { private$model.train }
    },
    #'
    #' @description The function is used to compute the time taken to
    #' perform training operation.
    #'
    #' @return A \link{numeric} vector with length 1.
    #'
    getExecutionTime = function() {
      if (!self$isTrained()) {
        message("[", class(self)[1], "][WARNING] Model '", private$model.info$name,
                "' is not trained. Task not performed")
        0.0
      } else { private$model.train$exec.time }
    },
    #'
    #' @description The function obtains the performance achieved by the model
    #' during training stage.
    #'
    #' @param metric A \link{character} used to specify the measure used to
    #' compute the performance.
    #'
    #' @return A \link{numeric} value with the performance achieved.
    #'
    getPerformance = function(metric = private$metric) {
      if (!is.null(private$model.train$model.data) &&
          !is.null(private$metric))
      {
        model.result <- private$model.train$model.data
        if (metric %in% model.result$perfNames) {
          model.result <- private$model.train$model.data$results
          model.result[best(model.result, metric = metric, maximize = TRUE), ][[metric]]
        } else {
          stop("[", class(self)[1], "][FATAL] Metric is not defined or unavailable. ",
               "Must be a [", paste(model.result$perfNames, collapse = ", "), "] type. ",
               "Aborting...")
        }
      } else {
        # if (is.null(private$model.train$model.data))
        #  message("[",class(self)[1],"][ERROR] Model '",
        #          private$model.info$name,"' is not trained. Task not performed")
        # if (is.null(private$metric))
        #  message("[",class(self)[1],"][ERROR] Metric is NULL. Task not performed")
        private$model.train$model.performance
      }
    },
    #'
    #' @description The function is used to get the configuration parameters
    #' achieved by the ML model after the training stage.
    #'
    #' @return A \link{list} object with the configuration parameters.
    #'
    getConfiguration = function() {
      if (self$isTrained()) {
        private$model.train$model.data$bestTune
      } else {
        message("[", class(self)[1], "][WARNING] Model '", private$model.info$name,
                "' is not trained. Task not performed")
        NULL
      }
    },
    #'
    #' @description The function is responsible of saving the model to disc into
    #' a RDS file.
    #'
    #' @param replace A \link{logical} value used to determine if model should
    #' be resaved. \link{TRUE} forces to replace previous saved model while
    #' \link{FALSE} keeps unchanged the previous model.
    #'
    save = function(replace = TRUE) {
      if (is.null(private$model.train$model.data))
        message("[", class(self)[1], "][ERROR] Cannot save untrained model. ",
                "Task not performed")
      else {
        if (file.exists(private$RDS.path)) {
          if (replace) {
            message("[", class(self)[1], "][WARNING][", private$model.info$name,
                    "] Model already exists. Replacing previous model")
            saveRDS (object = private$model.train, file = private$RDS.path)
            message("[", class(self)[1], "][INFO][", private$model.info$name,
                    "] Model succesfully saved at: ", private$RDS.path)
          } else {
            message("[", class(self)[1], "][INFO][", private$model.info$name,
                    "] Model already exists. Model not saved")
          }
        } else {
          saveRDS(object = private$model.train, file = private$RDS.path)
          message("[", class(self)[1], "][INFO][", private$model.info$name, "] ",
                  "Model succesfully saved at: ", private$RDS.path)
        }
      }
    },
    #'
    #' @description The function is used to delete a model from disc.
    #'
    remove = function() {
      if (file.exists(private$RDS.path)) {
        file.remove(private$RDS.path)
      } else {
        message("[", class(self)[1], "][ERROR] Cannot remove unsaved model. ",
                "Task not performed")
      }
    }
  ),
  private = list(
    dir.path = NULL,
    model.data = NULL,
    model.train = NULL,
    model.info = NULL,
    metric = NULL,
    RDS.path = NULL
  )
)
