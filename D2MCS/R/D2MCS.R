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

#' @title Data Driven Multiple Classifier System.
#'
#' @description The class is responsible of managing the whole process.
#' Concretely builds the M.L. models (optimizes models hyperparameters), selects
#' the best M.L. model for each cluster and executes the classification stage.
#'
#' @seealso \code{\link{Dataset}}, \code{\link{Subset}}, \code{\link{Trainset}}
#'
#' @keywords programming methods utilities classif
#'
#' @import R6
#' @importFrom devtools loaded_packages
#'
#' @export D2MCS
#'
#' @examples
#'
#' # Specify the random number generation
#' set.seed(1234)
#'
#' ## Create Dataset Handler object.
#' loader <- DatasetLoader$new()
#'
#' ## Load 'hcc-data-complete-balanced.csv' dataset file.
#' data <- loader$load(filepath = system.file(file.path("examples",
#'                                                      "hcc-data-complete-balanced.csv"),
#'                                            package = "D2MCS"),
#'                     header = TRUE, normalize.names = TRUE)
#' ## Get column names
#' data$getColumnNames()
#'
#' ## Split data into 4 partitions keeping balance ratio of 'Class' column.
#' data$createPartitions(num.folds = 4, class.balance = "Class")
#'
#' ## Create a subset comprising the first 2 partitions for clustering purposes.
#' cluster.subset <- data$createSubset(num.folds = c(1, 2), class.index = "Class",
#'                                     positive.class = "1")
#'
#' ## Create a subset comprising second and third partitions for trainning purposes.
#' train.subset <- data$createSubset(num.folds = c(2, 3), class.index = "Class",
#'                                   positive.class = "1")
#'
#' ## Create a subset comprising last partitions for testing purposes.
#' test.subset <- data$createSubset(num.folds = 4, class.index = "Class",
#'                                  positive.class = "1")
#'
#' ## Distribute the features into clusters using MCC heuristic.
#' distribution <- SimpleStrategy$new(subset = cluster.subset,
#'                                    heuristic = MCCHeuristic$new())
#' distribution$execute()
#'
#' ## Get the best achieved distribution
#' distribution$getBestClusterDistribution()
#'
#' ## Create a train set from the computed clustering distribution
#' train.set <- distribution$createTrain(subset = train.subset)
#'
#' \dontrun{
#'
#' ## Initialization of D2MCS configuration parameters.
#' ##  - Defining training operation.
#' ##    + 10-fold cross-validation
#' ##    + Use only 1 CPU core.
#' ##    + Seed was set to ensure straightforward reproductivity of experiments.
#' trFunction <- TwoClass$new(method = "cv", number = 10, savePredictions = "final",
#'                            classProbs = TRUE, allowParallel = TRUE,
#'                            verboseIter = FALSE, seed = 1234)
#'
#' #' ## - Specify the models to be trained
#' ex.classifiers <- c("ranger", "lda", "lda2")
#'
#' ## Initialize D2MCS
#' #' d2mcs <- D2MCS$new(dir.path = tempdir(),
#'                       num.cores = 1)
#'
#' ## Execute training stage for using 'MCC' and 'PPV' measures to optimize model hyperparameters.
#' trained.models <- d2mcs$train(train.set = train.set,
#'                               train.function = trFunction,
#'                               ex.classifiers = ex.classifiers,
#'                               metrics = c("MCC", "PPV"))
#'
#' ## Execute classification stage using two different voting schemes
#' predictions <- d2mcs$classify(train.output = trained.models,
#'                               subset = test.subset,
#'                               voting.types = c(
#'                                     SingleVoting$new(voting.schemes = c(ClassMajorityVoting$new(),
#'                                                                         ClassWeightedVoting$new()),
#'                                                      metrics = c("MCC", "PPV"))))
#'
#' ## Compute the performance of each voting scheme using PPV and MMC measures.
#' predictions$getPerformances(test.subset, measures = list(MCC$new(), PPV$new()))
#'
#' ## Execute classification stage using multiple voting schemes (simple and combined)
#' predictions <- d2mcs$classify(train.output = trained.models,
#'                               subset = test.subset,
#'                               voting.types = c(
#'                                     SingleVoting$new(voting.schemes = c(ClassMajorityVoting$new(),
#'                                                                          ClassWeightedVoting$new()),
#'                                                       metrics = c("MCC", "PPV")),
#'                                     CombinedVoting$new(voting.schemes = ClassMajorityVoting$new(),
#'                                                         combined.metrics = MinimizeFP$new(),
#'                                                         methodology = ProbBasedMethodology$new(),
#'                                                         metrics = c("MCC", "PPV"))))
#'
#' ## Compute the performance of each voting scheme using PPV and MMC measures.
#' predictions$getPerformances(test.subset, measures = list(MCC$new(), PPV$new()))
#' }
#'
D2MCS <- R6::R6Class(
  classname = "D2MCS",
  portable = TRUE,
  public = list(
    #'
    #' @description The function is used to initialize all parameters needed
    #' to build a Multiple Classifier System.
    #'
    #' @param dir.path A \link{character} defining location were the
    #' trained models should be saved.
    #' @param num.cores An optional \link{numeric} value specifying
    #' the number of CPU cores used for training the models (only if
    #' parallelization is allowed). If not defined (num.cores - 2) cores will be
    #' used.
    #' @param socket.type A \link{character} value defining the type of socket
    #' used to communicate the workers. The default type, \code{"PSOCK"}, calls
    #' makePSOCKcluster. Type \code{"FORK"} calls makeForkCluster. For more
    #' information see \code{\link{makeCluster}}
    #' @param outfile Where to direct the stdout and stderr connection output
    #' from the workers. "" indicates no redirection (which may only be useful
    #' for workers on the local machine). Defaults to '/dev/null'
    #' @param serialize A \code{\link{logical}} value. If \link{TRUE} (default)
    #' serialization will use XDR: where large amounts of data are to be
    #' transferred and all the nodes are little-endian, communication may be
    #' substantially faster if this is set to false.
    #'
    #' @import parallel
    #'
    initialize = function(dir.path, num.cores = NULL, socket.type = "PSOCK", outfile = NULL,
                          serialize = FALSE) {

      if (is.null(dir.path) || !is.character(dir.path)) {
        stop("[", class(self)[1], "][FATAL] Path to store ML models should be defined")
      }
      dir.path <- gsub("\\/$", "", dir.path)

      if (is.null(outfile)) {
        message("[", class(self)[1], "][INFO] Path for Log file not defined")
        outfile <- "/dev/null"
      }
      else {
        if (!file.exists(outfile)) {
          dir.create(outfile, recursive = TRUE)
          message("[", class(self)[1], "][INFO] Logs path not defined '",
                  outfile, "' does not exist. Creating...")
        }
      }

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Directory '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FALTAL] Cannot create directory '",
                     dir.path, "'") }
      }
      else { message("[", class(self)[1], "][INFO] Directory already exists") }

      private$logs <- file.path(dir.path, "logs")

      if (!dir.exists(private$logs)) {
        dir.create(private$logs, recursive = TRUE)
        if (!dir.exists(private$logs)) {
          private$logs <- NULL
        }
      }

      if (any(is.null(num.cores), (num.cores >= parallel::detectCores()), num.cores > 10)) {
        if (parallel::detectCores() > 10) {
          message("[", class(self)[1], "][WARNING] Invalid number of cores ",
                  "(1>= num.cores <= 10)")
        } else {
          message("[", class(self)[1], "][WARNING] Invalid number of cores ",
                  "(1>= num.cores < ", parallel::detectCores(), ")")
        }
        cores <- min(max(1, parallel::detectCores() - 2), 10)
        message("[", class(self)[1], "][INFO] Using default number of cores (",
                cores, "/", parallel::detectCores(), ")")
      }
      else {cores <- num.cores}

      if (!socket.type %in% c("PSOCK", "FORK")) {
        message("[", class(self)[1], "][WARNING] Invalid socket type. ",
                "Assuming 'PSOCK' cluster")
        socket <- "PSOCK"
      }
      else { socket <- socket.type }

      if (!is.logical(serialize)) {
        message("[", class(self)[1], "][WARNING] Invalid serialization ",
                "option. Assuming not serialization")
        xdr <- FALSE
      }
      else xdr <- serialize

      private$availableModels <- private$loadAvailableModels()
      private$path <- dir.path

      private$cluster.conf <- list(cores = cores, socket = socket,
                                   outfile = outfile, xdr = xdr)
      private$cluster.obj <- NULL
    },
    #'
    #' @description The function is responsible of performing the M.L. model
    #' training stage.
    #'
    #' @param train.set A \code{\link{Trainset}} object used as training input
    #' for the M.L. models
    #' @param train.function A \code{\link{TrainFunction}} defining the training
    #' configuration options.
    #' @param model.recipe An unprepared recipe object inherited from
    #' \code{\link{GenericModelFit}} class.
    #' @param num.clusters An \link{numeric} value used to define the number of
    #' clusters from the \code{\link{Trainset}} that should be utilized during
    #' the training stage. If not defined all clusters will we taken into
    #' account for training.
    #' @param ex.classifiers A \link{character} vector containing the name of
    #' the M.L. models used in training stage. See
    #' \code{\link{getModelInfo}} and
    #' \url{https://topepo.github.io/caret/available-models.html} for more
    #' information about all the available models.
    #' @param ig.classifiers A \link{character} vector containing the name of
    #' the M.L. that should be ignored when performing the training stage. See
    #' \code{\link{getModelInfo}} and
    #' \url{https://topepo.github.io/caret/available-models.html} for more
    #' information about all the available models.
    #' @param metrics A \link{character} vector containing the metrics used to
    #' perform the M.L. model hyperparameter optimization during the training
    #' stage. See \code{\link{SummaryFunction}}, \code{\link{UseProbability}}
    #' and \code{\link{NoProbability}} for more information.
    #' @param saveAllModels A \link{logical} parameter. A \link{TRUE} saves all
    #' trained models while A \link{FALSE} saves only the M.L. model achieving
    #' the best performance on each cluster.
    #'
    #' @return A \code{\link{TrainOutput}} object containing all the information
    #' computed during the training stage.
    #'
    #' @import parallel
    #'
    train = function(train.set, train.function, num.clusters = NULL,
                     model.recipe = DefaultModelFit$new(),
                     ex.classifiers = c(), ig.classifiers = c(),
                     metrics = NULL, saveAllModels = FALSE) {

      # CHECK IF TRAIN.SET IS VALID
      if (!inherits(train.set, "Trainset")) {
        stop("[", class(self)[1], "][FATAL] Train set parameter must be ",
             "defined as 'Trainset' type. Aborting...")
      }

      if (!inherits(train.function, "TrainFunction")) {
        stop("[", class(self)[1], "][FATAL] Train function parameter must be ",
             "defined as 'TrainFunction' type. Aborting...")
      }

      if (!inherits(model.recipe, "GenericModelFit")) {
        message("[", class(self)[1], "][WARNING] Model fit must inherit from ",
                "'GenericModelFit' type. Using 'DefaultModelFit' class.")
        model.recipe <- DefaultModelFit$new()
      }

      if (any(is.null(num.clusters), !is.numeric(num.clusters),
               !is.vector(num.clusters))) {
        message("[", class(self)[1], "][WARNING] Number of clusters not set ",
                "(must be numeric or vector). Using all clusters")
        num.clusters <- c(1:train.set$getNumClusters())
      } else {
        if (all(is.numeric(num.clusters), num.clusters > train.set$getNumClusters())) {
          message("[", class(self)[1], "][WARNING] Number of clusters ",
                  "is higher than number of existing clusters. ",
                  "Using all clusters")
          num.clusters <- c(1:train.set$getNumClusters())
        } else num.clusters <- c(1:num.clusters)
      }

      # VERIFY IF EX.CLASSIFIERS PARAMETER IS DEFINED (AND VALID)
      if (all(is.character(ex.classifiers), length(ex.classifiers) > 0)) {
        usedModels <- intersect(ex.classifiers, private$availableModels$name)
      } else { usedModels <- private$availableModels$name }

      # VERIFY IF IG.CLASSIFIERS PARAMETER IS DEFINED (AND VALID)
      if (all(is.character(ig.classifiers), length(ig.classifiers) > 0,
              length(usedModels) > 0)) {
        message("[", class(self)[1], "][INFO] Ignoring '",
                length(ig.classifiers), "' M.L models")
        usedModels <- setdiff(usedModels, ig.classifiers)
        message("[", class(self)[1], "][INFO] Still '", length(usedModels),
                "' M.L models available")
      }

      if (length(usedModels) == 0) {
        stop("[", class(self)[1], "][FATAL] Not valid M.L models were selected.",
             " Aborting...")
      }

      # VERIFY IF METRIC PARAMETER IS DEFINED (AND VALID)
      if (!all(is.character(metrics), length(metrics) > 0)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metrics.",
             " Aborting...")
      }

      message("[", class(self)[1], "][INFO] Making parallel socket cluster with ",
              private$cluster.conf$cores, " cores")

      private$cluster.obj <- parallel::makeCluster(private$cluster.conf$cores,
                                                   type = private$cluster.conf$socket,
                                                   outfile = private$cluster.conf$outfile,
                                                   useXDR = private$cluster.conf$xdr)

      cluster.models <- lapply(metrics, function(x) vector(mode = "list",
                                                           length = train.set$getNumClusters()))
      names(cluster.models) <- metrics

      exec.models <- private$availableModels[private$availableModels$name %in% usedModels, ]
      exec.models <- exec.models[order(exec.models$name), ]

      # START TRAINING PROCESS
      for (row in 1:nrow(exec.models)) {
        current.model <- exec.models[row, ]
        message("[", class(self)[1], "][INFO][", current.model$name, "]",
                " ***********************************************************************")
        message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                "'Model [", row, "-", nrow(exec.models), "]': Start training")
        message("[", class(self)[1], "][INFO][", current.model$name, "]",
                " ***********************************************************************")
        loaded.packages <- FALSE

        for (current.metric in metrics) {
          message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                  "----------------------------------------------------------------------")
          message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                  "'Metric [", which(current.metric == metrics), "-", length(metrics), "]': ",
                  "Training model for metric '", current.metric, "'")

          for (current.cluster in 1:train.set$getNumClusters()) {

            model.path <- file.path(private$path, current.metric,
                                     paste0("C[", current.cluster, "-",
                                            train.set$getNumClusters(), "]"))
            executed.models <- ExecutedModels$new(model.path)

            if (!executed.models$exist(current.model$name)) {

              message("[", class(self)[1], "][INFO][", current.model$name, "]",
                      " ----------------------------------------------------------------------")
              message("[", class(self)[1], "][INFO][", current.model$name, "]",
                      " Training on cluster 'C[", current.cluster, "-", train.set$getNumClusters(), "]'")
              message("[", class(self)[1], "][INFO][", current.model$name, "]",
                      " ----------------------------------------------------------------------")
              # LOAD REQUIRED PACKAGES
              if (!isTRUE(loaded.packages)) {
                packages <- unlist(current.model$library)
                if (!any(is.null(packages), is.na(packages), identical(packages, "NA"))) {
                  message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                          "Loading required packages...")
                  private$loadPackages(packages)
                  loaded.packages <- TRUE
                }
              }

              ifelse(isTRUE(current.model$prob),
                     train.function$create(UseProbability$new(),
                                           search.method = "random",
                                           class.probs = TRUE),
                     train.function$create(NoProbability$new(),
                                           search.method = "random",
                                           class.probs = FALSE)
              )

              model.instances <- train.set$getInstances(current.cluster)

              class.values <- relevel(x = factor(make.names(model.instances[[train.set$getClassName()]])),
                                      levels = unique(make.names(model.instances[[train.set$getClassName()]])),
                                      ref = as.character(make.names(train.set$getPositiveClass())))
              model.instances[[train.set$getClassName()]] <- class.values

              recipe <- model.recipe$createRecipe(model.instances,
                                                  class.name = train.set$getClassName())
              model.type <- Model$new(dir = model.path, model = current.model)
              model.type$train(train.set = model.instances, fitting = recipe,
                               trFunction = train.function, metric = current.metric,
                               logs = private$logs)
              if (model.type$isTrained()) {
                message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                        "Model has been succesfully trained")
                executed.models$add(model.type, keep.best = !isTRUE(saveAllModels))
                executed.models$save()
              } else {
                message("[", class(self)[1], "][WARNING] Unable to train model '",
                        current.model$name, "'. Skipping...")
                executed.models$add(model.type, keep.best = !isTRUE(saveAllModels))
                executed.models$save()
              }

            } else {
              message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                      "'Cluster[", current.cluster, "-", train.set$getNumClusters(), "]': ",
                      "Model has been previously trained. Skipping...")
            }
            cluster.models[[current.metric]][[current.cluster]] <- executed.models$getBest()$train
          }
        }
      }

      message("[", class(self)[1], "][INFO] ----------------------------------",
              "---------------------")
      message("[", class(self)[1], "][INFO] Finished")
      message("[", class(self)[1], "][INFO] ----------------------------------",
              "---------------------")

      if (!is.null(private$cluster.obj)) {
        parallel::stopCluster(private$cluster.obj)
        private$cluster.obj <- NULL
      }

      TrainOutput$new(models = cluster.models,
                      class.values = train.set$getClassValues(),
                      positive.class = train.set$getPositiveClass())
    },
    #'
    #' @description The function is responsible for executing the classification
    #' stage.
    #'
    #' @param train.output The \code{\link{TrainOutput}} object computed in the
    #' train stage.
    #' @param subset A \code{\link{Subset}} containing the data to be classified.
    #' @param voting.types A \link{list} containing \code{\link{SingleVoting}}
    #' or \code{\link{CombinedVoting}} objects.
    #' @param positive.class An optional \link{character} parameter used
    #' to define the positive class value.
    #'
    #' @return A \code{\link{ClassificationOutput}} with all the values computed
    #' during classification stage.
    #'
    classify = function(train.output, subset, voting.types, positive.class = NULL) {

      if (!inherits(train.output, "TrainOutput"))
        stop("[", class(self)[1], "][FATAL] Train output parameter must be ",
             "defined as 'TrainOutput' type. Aborting...")

      if (!inherits(subset, c("Subset", "HDSubset")))
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' or 'HDSubset' type. Aborting...")

      if (is.null(voting.types)) {
        stop("[", class(self)[1], "][FATAL] Voting types parameter is not defined. ",
             "Aborting...")
      }

      if (!is.list(voting.types) || !is.vector(voting.types)) {
        voting.types <- list(voting.types)
      }

      if (!all(sapply(voting.types,  function(x) {
        inherits(x, c("SingleVoting", "CombinedVoting"))
      }))) {
        stop("[", class(self)[1], "][FATAL] Voting Schemes parameter must be ",
             "defined as 'SingleVoting' or 'CombinedVoting' types. Aborting...")
      }

      class.values <- levels(train.output$getClassValues())

      if (subset$isBlinded()) {
        if (is.null(positive.class)) {
          message("[", class(self)[1], "][WARNING] Positive class is not set. ",
                  "Asuming positive class value used during training stage '",
                  train.output$getPositiveClass(), "'")
          positive.class <- train.output$getPositiveClass()
        } else {
          if (!(positive.class %in% class.values)) {
            stop("[", class(self)[1], "][FATAL] Positive class has ",
                 "not being defined during training stage. Aborting...")
          }
        }
      } else {
        if (is.null(positive.class)) {
          message("[", class(self)[1], "][WARNING] Positive class not set. ",
                  "Asuming positive class value used during training stage '",
                  train.output$getPositiveClass(), "'")
          positive.class <- train.output$getPositiveClass()
        } else {
          if (!(positive.class %in% class.values)) {
            message("[", class(self)[1], "][WARNING] Positive class value is ",
                    "invalid. Must be [", paste0(class.values, collapse = ", "), "].",
                    " Assuming positive class used during training stage (",
                    train.output$getPositiveClass(), ")")
            positive.class <- train.output$getPositiveClass()
          }
        }
      }

      exec.metrics <- unique(unlist(sapply(voting.types, function(voting) {
        voting$getMetrics() })))

      cluster.predictions <- list()
      final.votings <- list()
      final.models <- list()

      for (metric in exec.metrics) {
        if (!metric %in% train.output$getMetrics())
        {
          message("[", class(self)[1], "][WARNING] Models were not trained for '",
                  metric, "' metric. Executing next metric...")
          next
        }
        predictions <- ClusterPredictions$new(class.values = class.values,
                                              positive.class = positive.class)
        num.clusters <- length(train.output$getModels(metric))
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        message("[", class(self)[1], "][INFO] Executing predictions for ",
                metric, " metric")
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        for (cluster in seq_len(num.clusters)) {
          message("[", class(self)[1], "][INFO] ------------------------------",
                  "-------------------------")
          message("[", class(self)[1], "][INFO] Computing cluster '",
                  cluster, "' of '", num.clusters, "'")

          pred <- Prediction$new(model = train.output$getModels(metric)[[cluster]],
                                 feature.id = subset$getID())
          final.models <- append(final.models, list(train.output$getModels(metric)))
          names(final.models)[[length(final.models)]] <- metric

          iterator <- subset$getIterator(chunk.size = 10000)
          while (!iterator$isLast()) {
            instances <- iterator$getNext()
            pred$execute(instances, class.values, positive.class)
          }
          # iterator$finalize()
          rm(iterator)
          predictions$add(pred)
        }
        message("[", class(self)[1], "][INFO] --------------------------------",
                "-----------------------")
        cluster.predictions[[metric]] <- predictions
      }

      for (voting.type in voting.types) {
        valid.metrics <- intersect(voting.type$getMetrics(),
                                   names(cluster.predictions))
        if (length(valid.metrics) == 0) {
          message("[D2MCS][INFO] Metrics for '", voting.type$getName(), "' were ",
                  "not computed. Ignoring voting type...")
          next
        }
        voting.name <- class(voting.type)[1]
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        message("[D2MCS][INFO] Computing final prediction values using '",
                voting.type$getName(), "' schemes")
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        voting.result <- voting.type$execute(cluster.predictions[valid.metrics])

        final.votings[[voting.name]] <- append(final.votings[[voting.name]],
                                               voting.result)
      }

      if (length(final.votings) == 0) {
        message("[", class(self)[1], "][ERROR] No voting system could be ",
                "executed for the indicated metrics. Task not performed")
        NULL
      } else {

        message("[", class(self)[1], "][INFO] ----------------------------------",
                "---------------------")
        message("[", class(self)[1], "][INFO] Finished")
        message("[", class(self)[1], "][INFO] ----------------------------------",
                "---------------------")

        ClassificationOutput$new(voting.schemes = final.votings, models = final.models)
      }
    },
    #'
    #' @description The function obtains all the available M.L. models.
    #'
    #' @return A \link{data.frame} containing the information of the available
    #' M.L. models.
    #'
    getAvailableModels = function() { private$availableModels[, c(1, 2)] }
  ),
  private = list(
    loadAvailableModels = function() {
      model.list <- caret::getModelInfo()

      if (is.null(model.list)) {
        stop("[", class(self)[1], "][FATAL] Models not found in caret library. ",
             "Aborting...")
      }

      supported.packages <- unique(available.packages(repos = "https://cloud.r-project.org",
                                                      filters = c("R_version", "OS_type"))[, 1])
      supported.models <- names(model.list[sapply(model.list, function(model) {
                                all(model$library %in% supported.packages) })])

      models <- do.call(rbind, apply(t(supported.models), 2, function(name, modelList) {
        if (!name %in% c("null") &&
            any(modelList[[name]]$type %in% "Classification")) {
          data.frame(name = name, description = modelList[[name]]$label,
                     family = base::trimws(modelList[[name]]$tags[1]),
                     library = I(list(modelList[[name]]$library)),
                     prob = (!is.null(modelList[[name]]$prob) &&
                               length(grep("response", deparse(modelList[[name]]$prob))) == 0),
                     stringsAsFactors = FALSE)
        }
      }, modelList = model.list))

      message("[", class(self)[1], "][INFO] ", nrow(models),
              " M.L. classifiers has been succesfully loaded")
      models <- with(models, models[order(models$family, models$name), ])
      models
    },
    loadPackages = function(pkgName) {
      if (length(pkgName) > 0) {
        new.packages <- pkgName[sapply(pkgName, function(pkg) system.file(package = pkg) == "")]
        if (length(new.packages) > 0) {
          message("[", class(self)[1], "][INFO] ", length(new.packages),
                  " packages needed. Installing packages '",
                  paste0(new.packages, collapse = ","), "'...")

          lapply(new.packages, function(pkg) caret::checkInstall(pkg = pkg))
        }
        lapply(pkgName, function(pkg) {
          if (!pkg %in% devtools::loaded_packages()) {
            suppressMessages(library(pkg, character.only = TRUE, warn.conflicts = FALSE,
                                     verbose = FALSE, quietly = TRUE,
                                     attach.required = TRUE))
          }
        })
      } else {
        message("[", class(self)[1], "][ERROR] Packages are not available ",
                "on CRAN...")
      }
    },
    cluster.conf = NULL,
    cluster.obj = NULL,
    availableModels = NULL,
    path = NULL,
    logs = NULL
  )
)
