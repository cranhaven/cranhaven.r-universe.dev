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

#' @title Simple Dataset handler.
#'
#' @description Creates a valid simple dataset object.
#'
#' @seealso \code{\link{HDDataset}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export Dataset

Dataset <- R6::R6Class(
  classname = "Dataset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param filepath The name of the file which the data are to be read from.
    #' Each row of the table appears as one line of the file. If it does not
    #' contain an _absolute_ path, the file name is _relative_ to the current
    #' working directory, '\code{getwd()}'.
    #' @param header A \link{logical} value indicating whether the file contains
    #' the names of the variables as its first line. If missing, the value is
    #' determined from the file format: '\code{header}' is set to '\link{TRUE}'
    #' if and only if the first row contains one fewer field than the number of
    #' columns.
    #' @param sep The field separator character. Values on each line of the file
    #' are separated by this character.
    #' @param skip Defines the number of header lines should be skipped.
    #' @param normalize.names A \link{logical} value indicating whether the
    #' columns names should be automatically renamed to ensure R compatibility.
    #' @param string.as.factor A \link{logical} value indicating if character
    #' columns should be converted to factors (\code{default = FALSE}).
    #' @param ignore.columns Specify the columns from the input file that should
    #' be ignored.
    #'
    #' @importFrom utils read.csv
    #'
    initialize = function(filepath, header = TRUE, sep = ",", skip = 0,
                          normalize.names = FALSE, string.as.factor = FALSE,
                          ignore.columns = NULL) {

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[", class(self)[1], "][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      message("[", class(self)[1], "][INFO] Dataset size: ",
              round(dt.size, digits = 4), " Gb.")

      if (dt.size > 1) {
        stop("[", class(self)[1], "][FATAL] High Dimensional Dataset is not ",
             "compatible with Dataset class loader. Aborting...")
      }

      message("[", class(self)[1], "][INFO] Loading Dataset...")

      if (isTRUE(header)) {
        private$corpus <- read.csv(filepath, header = header,
                                   skip = (skip + 1), sep = sep,
                                   stringsAsFactors = string.as.factor)

        columnNames <- unlist(strsplit(scan(file = filepath, nlines = 1,
                                            what = "character", quiet = TRUE, sep = sep),
                                       split = sep))

        if (isTRUE(normalize.names)) {
          columnNames <- make.names(columnNames, unique = TRUE)
        }
        names(private$corpus) <- columnNames
      } else {
        private$corpus <- read.csv(filepath, header = header, skip = skip,
                                   sep = sep, stringsAsFactors = string.as.factor)
        columnNames <- colnames(private$corpus)
      }

      private$removed.columns <- list()

      if (is.numeric(ignore.columns)) { self$removeColumns(ignore.columns) }

      message("[", class(self)[1], "][INFO] Load finished! Total: ",
              nrow(private$corpus), " rows and ",
              ncol(private$corpus), " columns")
    },
    #'
    #' @description Get the name of the columns comprising the dataset.
    #'
    #' @return A \link{character} vector with the name of each column.
    #'
    getColumnNames = function() { names(private$corpus) },
    #'
    #' @description Gets the full dataset.
    #'
    #' @return A \link{data.frame} with all the loaded information.
    #'
    getDataset = function() { private$corpus },
    #'
    #' @description Obtains the number of columns present in the dataset.
    #'
    #' @return An \link{integer} of length 1 or \link{NULL}
    #'
    getNcol = function() { ncol(private$corpus) },
    #'
    #' @description Obtains the number of rows present in the dataset.
    #'
    #' @return An \link{integer} of length 1 or \link{NULL}
    #'
    getNrow = function() { nrow(private$corpus) },
    #'
    #' @description Get the columns removed or ignored.
    #'
    #' @return A \link{list} containing the name of the removed columns.
    #'
    getRemovedColumns = function() { private$removed.columns },
    #'
    #' @description Removes \link{data.frame} columns matching some criterion.
    #'
    #' @param remove.funcs A vector of functions use to define which columns
    #' must be removed.
    #' @param remove.na A \link{logical} value indicating whether \link{NA}
    #' values should be removed.
    #' @param remove.const A \link{logical} value used to indicate if constant
    #' values should be removed.
    #'
    cleanData = function(remove.funcs = NULL, remove.na = TRUE,
                         remove.const = FALSE) {
      if (isTRUE(remove.na) || isTRUE(remove.const) ||
         (!is.null(remove.funcs) && length(remove.funcs) > 0)) {
        subset <- private$corpus
        for (func in remove.funcs) {
          subset.names <- names(subset)
          subset <- Filter(func, subset)
          subset.removed <- setdiff(subset.names, names(subset))
          if (length(subset.removed) > 0) {
            private$removed.columns[["remove.funcs"]] <- append(private$removed.columns[["remove.funcs"]],
                                                                subset.removed)
          }
        }
        message("[", class(self)[1], "][INFO] Total ",
                length(private$removed.columns[["remove.funcs"]]),
                " columns were succesfully removed")

        if (isTRUE(remove.na)) {
          subset.names <- names(subset)
          subset <- Filter(function(col) !all(is.na(col)), subset)
          subset.removed <- setdiff(subset.names, names(subset))
          if (length(subset.removed) > 0) {
            private$removed.columns[["remove.na"]] <- append(private$removed.columns[["na.remove"]],
                                                             subset.removed)
          }
          message("[", class(self)[1], "][INFO] Total ", length(subset.removed),
                  " NA columns were succesfully removed")
        }

        if (isTRUE(remove.const)) {
          subset.names <- names(subset)
          subset <- Filter(function(col) length(unique(col)) > 1, subset)
          subset.removed <- setdiff(subset.names, names(subset))
          if (length(subset.removed) > 0) {
            private$removed.columns[["remove.const"]] <- append(private$removed.columns[["remove.const"]],
                                                                subset.removed)
          }
          message("[", class(self)[1], "][INFO] Total ", length(subset.removed),
                  " const columns were succesfully removed")
        }

        private$corpus <- subset
      }
    },
    #'
    #' @description Applies \code{cleanData} function over an specific set of
    #' columns.
    #'
    #' @param columns Set of columns (\link{numeric} or \link{character}) where
    #' removal operation should be applied.
    #' @param remove.funcs A vector of functions use to define which columns
    #' must be removed.
    #' @param remove.na A \link{logical} value indicating whether
    #' \code{\link{NA}} values should be removed.
    #' @param remove.const A \link{logical} value used to indicate if constant
    #' values should be removed.
    #'
    #' @importFrom dplyr between
    #'
    removeColumns = function(columns, remove.funcs = NULL, remove.na = FALSE,
                             remove.const = FALSE) {
      if (is.character(columns)) {
        if (any(columns %in% names(private$corpus))) {

          valid.columns  <- intersect(names(private$corpus), columns)
          private$corpus <- private$corpus[, -which(names(private$corpus) %in% valid.columns)]
          private$removed.columns[["manually"]] <- append(private$removed.columns[["manually"]],
                                                          valid.columns)
          message("[", class(self)[1], "][INFO] Total ", length(valid.columns),
                  " columns were succesfully removed")
        } else {
          message("[", class(self)[1], "][ERROR] Defined column(s) are not valid.",
                  " Ignoring removal operation")
        }
      } else {
        if (is.numeric(columns) && all(dplyr::between(columns, 1, ncol(private$corpus)))) {
          private$removed.columns <- c(private$removed.columns,
                                       names(private$corpus)[columns])
          private$corpus <- private$corpus[, -columns]
          message("[", class(self)[1], "][INFO] ", length(columns),
                  " columns were manually removed")
        } else {
          message("[", class(self)[1], "][ERROR] Selected columns are not valid. ",
                  "Must be between [1-", ncol(private$corpus), "]. ",
                  "Task not performed")
        }
      }
      self$cleanData(remove.funcs, remove.na, remove.const)
      message("[", class(self)[1], "][INFO] Remaining ",
              ncol(private$corpus), " columns")
    },
    #'
    #' @description Creates a k-folds partition from the initial dataset.
    #'
    #' @param num.folds A \link{numeric} for the number of folds (partitions)
    #' @param percent.folds A \link{numeric} vector with the percentage of
    #' instances containing each fold.
    #' @param class.balance A \link{logical} value indicating if class balance
    #' should be kept.
    #'
    #' @importFrom caret createFolds
    #'
    createPartitions = function(num.folds = NULL, percent.folds = NULL,
                                class.balance = NULL) {

      if (!is.null(class.balance)) {
        if (all(is.numeric(class.balance), class.balance %in% 1:ncol(private$corpus))) {
          class.values <- factor(private$corpus[, class.balance])
          class.index <- class.balance
          class.balance <- TRUE
        } else {
          if (all(is.character(class.balance), class.balance %in% names(private$corpus))) {
            class.values <- factor(private$corpus[, class.balance])
            class.index <- which(class.balance == names(private$corpus))
            class.balance <- TRUE
          } else {
            class.values <- NULL
            class.index <- NULL
            message("[", class(self)[1], "][WARNING] Class not found into dataset ",
                    "limits")
          }
        }
      }

      if (((!is.numeric(num.folds) || length(num.folds) != 1) &&
           !is.numeric(percent.folds))) {

        if (is.null(class.balance) || is.null(class.index)) {
          stop("[", class(self)[1], "][FATAL] Class not defined. ",
               "Aborting...")
        }
        message("[", class(self)[1], "][WARNING] Parameters are invalid. ",
                "Assuming division with default k=10 folds")
        private$partitions <- caret::createFolds(private$corpus[, class.index],
                                                 k = 10, list = TRUE)
      } else {
        if (is.numeric(num.folds) && length(num.folds) == 1 && !is.numeric(percent.folds)) {

          if (is.null(class.balance) || is.null(class.index)) {
            stop("[", class(self)[1], "][FATAL] Class not defined. ",
                 "Aborting...")
          }

          message("[", class(self)[1], "][INFO] Perfoming dataset partitioning into ",
                   num.folds, " groups using class balance")
          private$partitions <- caret::createFolds(private$corpus[, class.index],
                                                   k = num.folds, list = TRUE)
        } else {
          if (is.numeric(num.folds) && length(num.folds) == 1 && is.numeric(percent.folds)) {
            if (length(percent.folds) == num.folds &&
                (sum(percent.folds) == 100 || sum(percent.folds) == 1)) {
              if (sum(percent.folds) == 100) {
                percent.folds <- percent.folds / 100
              }
              message("[", class(self)[1], "][INFO] Perfoming dataset ",
                      "partitioning into ", length(percent.folds), " groups")
              remaining <- private$corpus

              numElemFolds <- round(percent.folds * nrow(private$corpus))

              for (index in 1:(num.folds - 1)) {
                message("===============================================================")
                message("[", class(self)[1], "][INFO] Spliting ", index,
                        " group with ", percent.folds[index])
                message("===============================================================")
                if (isTRUE(class.balance)) {
                  if (length(levels(class.values)) != 2) {
                    stop("[", class(self)[1], "][ERROR] Create partitions with ",
                         "option of class.balance for multiclass data is not ",
                         "still available. Aborting...")
                  }
                  class.percents <- table(private$corpus[, class.index]) / nrow(private$corpus)

                  split1 <- sample(which(remaining[, class.index] == names(class.percents)[1]),
                                   round(numElemFolds[[index]] * class.percents[[1]]),
                                   replace = FALSE)
                  split2 <- sample(which(remaining[, class.index] == names(class.percents)[2]),
                                   round(numElemFolds[[index]] * class.percents[[2]]),
                                   replace = FALSE)
                  split <- c(split1, split2)
                } else {
                  split <- sample(1:nrow(remaining), numElemFolds[[index]], replace = FALSE)
                }
                private$partitions <- append(private$partitions,
                                             list(which(rownames(private$corpus) %in% rownames(remaining)[split])))
                remaining <- remaining[-split, ]
              }
              message("===============================================================")
              message("[", class(self)[1], "][INFO] Spliting ", index + 1,
                      " group with ", percent.folds[index + 1])
              message("===============================================================")
              private$partitions <-  append(private$partitions,
                                            list(which(rownames(private$corpus) %in% rownames(remaining))))
              names(private$partitions) <- paste0("Fold0",
                                                  which(1:num.folds < 10))
              if ((num.folds >= 10)) {
                names(private$partitions)[10:num.folds] <- paste0("Fold",
                                                                  which(1:num.folds >= 10))
              }
            } else { message("[", class(self)[1], "][ERROR] Fold partition and/or ",
                             "probability mismatch. Task not performed") }
          } else {
            if ((!is.numeric(num.folds) || length(num.folds) != 1) &&
                is.numeric(percent.folds) && (sum(percent.folds) == 100 ||
                                              sum(percent.folds) == 1)) {
              if (sum(percent.folds) == 100) {
                percent.folds <- percent.folds / 100
              }
              message("[", class(self)[1], "][INFO] Perfoming dataset partitioning into ",
                      length(percent.folds), " groups")
              remaining <- private$corpus

              numElemFolds <- round(percent.folds * nrow(private$corpus))

              for (index in 1:(length(percent.folds) - 1)) {
                message("===============================================================")
                message("[", class(self)[1], "][INFO] Spliting ", index,
                        " group with ", percent.folds[index])
                message("===============================================================")
                if (isTRUE(class.balance)) {
                  if (length(levels(class.values)) != 2) {
                    stop("[", class(self)[1], "][ERROR] Create partitions with ",
                         "option of class.balance for multiclass data is not ",
                         "still available. Aborting...")
                  }

                  class.percents <- table(private$corpus[, class.index]) / nrow(private$corpus)

                  split1 <- sample(which(remaining[, class.index] == names(class.percents)[1]),
                                   round(numElemFolds[[index]] * class.percents[[1]]),
                                   replace = FALSE)
                  split2 <- sample(which(remaining[, class.index] == names(class.percents)[2]),
                                   round(numElemFolds[[index]] * class.percents[[2]]),
                                   replace = FALSE)
                  split <- c(split1, split2)
                } else {
                  split <- sample(1:nrow(remaining), numElemFolds[[index]], replace = FALSE)
                }
                private$partitions <- append(private$partitions,
                                             list(which(rownames(private$corpus) %in% rownames(remaining)[split])))
                remaining <- remaining[-split, ]
              }
              message("===============================================================")
              message("[", class(self)[1], "][INFO] Spliting ", index + 1,
                      " group with ", percent.folds[index + 1])
              message("===============================================================")
              private$partitions <-  append(private$partitions,
                                            list(which(rownames(private$corpus) %in% rownames(remaining))))
              names(private$partitions) <- paste0("Fold0",
                                                  which(1:length(percent.folds) < 10))
              if ((length(percent.folds) >= 10)) {
                names(private$partitions)[10:length(percent.folds)] <- paste0("Fold",
                                                                              which(1:length(percent.folds) >= 10))
              }
            } else {
              message("[", class(self)[1], "][ERROR] Cannot perform ",
                      "partition process. Task not performed")
            }
          }
        }
      }
    },
    #'
    #' @description Creates a \code{\link{Subset}} for testing or classification
    #' purposes. A target class should be provided for testing purposes.
    #'
    #' @param num.folds A \link{numeric} defining the number of folds that
    #' should we used to build the \link{Subset}.
    #' @param opts A list with optional parameters. Valid arguments are
    #' \code{remove.na} (removes columns with \link{NA} values) and
    #' \code{remove.const} (ignore columns with constant values).
    #' @param class.index  A \link{numeric} value identifying the column
    #' representing the target class
    #' @param positive.class Defines the positive class value.
    #'
    #' @return A \link{Subset} object.
    #'
    #' @importFrom dplyr between
    #'
    createSubset = function(num.folds = NULL,
                            opts = list(remove.na = TRUE, remove.const = FALSE),
                            class.index = NULL,
                            positive.class = NULL) {
      subset <- NULL
      if (is.null(private$partitions)) {
        message("[", class(self)[1], "][ERROR] Dataset distribution is null. ",
                "Task not performed")
        return(NULL)
      }

      if (is.null(num.folds) || !is.numeric(num.folds) ||
           !(max(num.folds) %in% c(1:length(private$partitions))))
      {
        message("[", class(self)[1], "][WARNING] Incorrect number of folds. ",
                "Must be between 1 and ", length(private$partitions),
                ". Assuming whole dataset")
        num.folds <- length(private$partitions)
      }

      subset <- private$corpus[ sort(Reduce(union, private$partitions[num.folds])), ]

      if (is.null(class.index)) {
        message("[", class(self)[1], "][INFO] Creating subset without an associated class")
        class.values <- NULL
      } else {
        if (all(is.character(class.index), class.index %in% names(subset))) {
          class.index <- which(class.index == names(subset))
        } else {
          if (!all(is.numeric(class.index), class.index %in% 1:ncol(subset))) {
            stop("[", class(self)[1], "][FATAL] Class not found into dataset ",
                 "limits. Aborting...")
          }
        }

        if (any(is.null(positive.class), !positive.class %in% subset[, class.index])) {
          stop("[", class(self)[1], "][FATAL] Positive class value not found. ",
               "Aborting...")
        }
        class.values <- relevel(x = factor(subset[, class.index]),
                                levels = unique(subset[, class.index]),
                                ref = as.character(positive.class))
        class.name <- names(subset)[class.index]
      }

      if (is.list(opts) && (exists("remove.na", opts) && isTRUE(opts$remove.na) ||
                            exists("remove.const", opts) && isTRUE(opts$remove.const))) {
        na.remov <- 0
        const.remov <- 0
        if (!is.null(class.index)) {
          filtered <- subset[, -class.index]
        } else {
          filtered <- subset
        }

        if (exists("remove.na", opts) && isTRUE(opts$remove.na)) {
          filtered <- Filter(function(col) !all(is.na(col)), filtered)
          if (!is.null(class.index)) {
            na.remov <- ((ncol(subset) - 1) - ncol(filtered))
          } else {
            na.remov <- (ncol(subset) - ncol(filtered))
          }
          message("[", class(self)[1], "][INFO] Removed columns containing ",
                  "NA values (total of ", na.remov, ")")
        }
        if (exists("remove.const", opts) && isTRUE(opts$remove.const)) {
          filtered <- Filter(function(col) all(length(unique(col)) != 1), filtered)
          if (!is.null(class.index)) {
            const.remov <- ((ncol(subset) - 1) - ncol(filtered)) + na.remov
          } else {
            const.remov <- (ncol(subset) - ncol(filtered)) + na.remov
          }
          message("[", class(self)[1], "][INFO] Removed columns containing ",
                  "constant values (total of ", const.remov, ")")
        }
        if (!is.null(class.index)) {
          if (class.index >= ncol(filtered)) {
            subset <- cbind(filtered, subset[, class.index])
            class.index <- ncol(filtered) + 1
          } else {
            if (class.index == 1) {
              subset <- cbind(subset[, class.index], filtered)
            } else {
              subset <- cbind(filtered[1:class.index - 1],
                              subset[, class.index],
                              filtered[class.index:ncol(filtered)])
            }
          }
          names(subset)[class.index] <- class.name
        }
      }

      if (!is.null(class.index)) {
        subset[[class.index]] <- class.values
      }

      Subset$new(dataset = subset, class.index = class.index,
                 class.values = class.values,
                 positive.class = positive.class)
    },
    #'
    #' @description Creates a set for training purposes. A class should be
    #' defined to guarantee full-compatibility with supervised models.
    #'
    #' @param class.index A \link{numeric} value identifying the column
    #' representing the target class
    #' @param positive.class Defines the positive class value.
    #' @param num.folds A \link{numeric} defining the number of folds that
    #' should we used to build the \code{\link{Subset}}.
    #' @param opts A list with optional parameters. Valid arguments are
    #' \code{remove.na} (removes columns with \link{NA} values) and
    #' \code{remove.const} (ignore columns with constant values).
    #'
    #' @return A \code{\link{Trainset}} object.
    #'
    createTrain = function(class.index, positive.class, num.folds = NULL,
                           opts = list(remove.na = TRUE, remove.const = FALSE)) {
      trainSet <- NULL
      if (is.null(private$partitions)) {
        message("[", class(self)[1], "][ERROR] Dataset distribution is null. ",
                "Task not performed")
        return(NULL)
      }

      if (all(is.character(class.index), class.index %in% names(private$corpus))) {
        class.index <- which(class.index == names(private$corpus))
      } else {
        if (!all(is.numeric(class.index), class.index %in% 1:ncol(private$corpus))) {
          stop("[", class(self)[1], "][FATAL] Class not found into dataset ",
               "limits. Aborting...")
        }
      }

      if (is.null(num.folds) || !is.numeric(num.folds) ||
           !(max(num.folds) %in% c(1:length(private$partitions)))) {
        message("[", class(self)[1], "][WARNING] Incorrect number of folds. ",
                "Must be between 1 and ", length(private$partitions),
                ". Assuming whole dataset")
        num.folds <- length(private$partitions)
      }

      trainSet <- private$corpus[ sort(Reduce(union, private$partitions[num.folds])), ]

      if (is.null(positive.class) || !positive.class %in% trainSet[, class.index]) {
        stop("[", class(self)[1], "][FATAL] Positive class value not found. ",
             "Aborting...")
      }

      class.values <- relevel(x = factor(trainSet[, class.index],
                                         levels = unique(trainSet[, class.index])),
                              ref = as.character(positive.class))
      class.name <- names(trainSet)[class.index]

      if (is.list(opts)) {
        na.remov <- 0
        const.remov <- 0
        filtered <- trainSet[, -class.index]

        if (exists("remove.na", opts) && isTRUE(opts$remove.na)) {
          filtered <- Filter(function(col) !all(is.na(col)), filtered)
          na.remov <- ((ncol(trainSet) - 1) - ncol(filtered))
          message("[", class(self)[1], "][INFO] Removed columns containing NA ",
                  "values (total of ", na.remov, ")")
        }
        if (exists("remove.const", opts) && isTRUE(opts$remove.const)) {
          filtered <- Filter(function(col) all(length(unique(col)) != 1), filtered)
          const.remov <- ((ncol(trainSet) - 1) - ncol(filtered)) + na.remov
          message("[", class(self)[1], "][INFO] Removed columns containing ",
                  "constant values (total of ", const.remov, ")")
        }
        trainSet <- filtered
      }

      Trainset$new(cluster.dist = list(trainSet), class.name = class.name,
                   class.values = class.values,
                   positive.class = positive.class)
    }
  ),
  private = list(
    removed.columns = NULL,
    corpus = NULL,
    partitions = NULL
  )
)
