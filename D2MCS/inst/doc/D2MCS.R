## ---- echo = TRUE, results = "hide", eval = FALSE-----------------------------
#  data.loader <- DatasetLoader$new()
#  data <- data.loader$load(filepath, header = TRUE, sep = ",",
#                           skip.lines = 0, normalize.names = FALSE,
#                           ignore.columns = NULL)

## ---- echo = TRUE, results = "hide", eval = FALSE-----------------------------
#  ## DATASET INFORMATION OBTAINER
#  data$getNcol()
#  data$getNrow()
#  data$getColumnNames()
#  data$getDataset()
#  
#  ## DATASET COLUMN REMOVAL
#  data$cleanData(columns = NULL,
#                 remove.funcs = NULL,
#                 remove.na = FALSE,
#                 remove.const = FALSE)
#  
#  ## DATASET HANDLING AND SPLITTING
#  data$createPartitions(num.folds = NULL,
#                        percent.folds = NULL,
#                        class.balance = NULL)
#  subset <- data$createSubset(num.folds = NULL,
#                              column.id = NULL,
#                              opts = list(remove.na = TRUE,
#                                          remove.const = FALSE),
#                              class.index = NULL,
#                              positive.class = NULL)
#  train <- data$createTrain(num.folds = NULL,
#                            class.index,
#                            positive.class,
#                            opts = list(remove.na = TRUE,
#                                        remove.const = FALSE))
#  

## ---- echo = TRUE, results = "hide", eval = FALSE-----------------------------
#  ## FEATURE-CLUSTERING ALGORITHM CREATION
#  conf <- DependencyBasedStrategyConfiguration$new()
#  dbs <- DependencyBasedStrategy$new(subset,
#                                     heuristic,
#                                     configuration = conf)
#  
#  ## FEATURE-CLUSTERING ALGORITHM EXECUTION
#  dbs$execute(verbose = FALSE)
#  
#  ## FEATURE-CLUSTERING ALGORITHM FUNCTIONALITIES
#  dbs$getBestClusterDistribution()
#  dbs.train <- dbs$createTrain(subset,
#                               num.clusters = NULL,
#                               num.groups = NULL,
#                               include.unclustered = FALSE)
#  

## ---- echo = TRUE, results = "hide", eval = FALSE-----------------------------
#  ## D2MCS FRAMEWORK INITIALIZATION
#  d2mcs <- D2MCS$new(dir.path,
#                     num.cores = 2,
#                     socket.type = "PSOCK",
#                     outfile = NULL)
#  
#  ## MCS BEHAVIOUR CUSTOMIZATION OPTIONS
#  trFunction <- TwoClass$new(method,
#                             number,
#                             savePredictions,
#                             classProbs,
#                             allowParallel,
#                             verboseIter,
#                             seed = NULL)
#  
#  ## EXECUTION OF MCS DISCOVERY OPERATION
#  trained <- d2mcs$train(train.set,
#                         train.function,
#                         num.clusters = NULL,
#                         model.recipe = DefaultModelFit$new(),
#                         ex.classifiers = c(),
#                         ig.classifiers = c(),
#                         metrics = NULL,
#                         saveAllModels = FALSE)
#  

## ---- echo = TRUE, results = "hide", eval = FALSE-----------------------------
#  ## VOTING SCHEMES AVAILABLE IN THE CLASSIFICATION STAGE
#  voting.types <- c(SingleVoting$new(voting.schemes,
#                                     metrics),
#                    CombinedVoting$new(voting.schemes,
#                                       combined.metrics,
#                                       methodology,
#                                       metrics))
#  
#  ## EXECUTE THE CLASSIFICATION STAGE
#  predictions <- d2mcs$classify(train.output,
#                                subset,
#                                voting.types,
#                                positive.class = NULL)
#  
#  ## COMPUTE THE PERFORMANCE OF EACH VOTING SCHEME
#  predictions$getPerformances(test.set,
#                              measures,
#                              voting.names = NULL,
#                              metric.names = NULL,
#                              cutoff.values = NULL)
#  
#  ## OBTAIN THE PREDICTIONS OBTAINED OF EACH VOTING SCHEME USED
#  prediction$getPredictions(voting.names = NULL,
#                            metric.names = NULL,
#                            cutoff.values = NULL,
#                            type = NULL,
#                            target = NULL,
#                            filter = FALSE)
#  

