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

#' @title Classification set.
#'
#' @description The \code{\link{Subset}} is used for testing or classification
#' purposes. If a target class is defined the \code{\link{Subset}} can be used
#' as test and classification, otherwise the \code{\link{Subset}} only
#' classification is compatible.
#'
#' @details Use \code{\link{Dataset}} to ensure the creation of a valid
#' \code{\link{Subset}} object.
#'
#' @seealso \code{\link{Dataset}}, \code{\link{DatasetLoader}},
#' \code{\link{Trainset}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export Subset

Subset <- R6::R6Class(
  classname = "Subset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param dataset A fully filled \link{data.frame}.
    #' @param class.index A \link{numeric} value identifying the column
    #' representing the target class
    #' @param class.values A \link{character} vector containing all the values
    #' of the target class.
    #' @param positive.class A \link{character} value representing the positive
    #' class value.
    #' @param feature.id A \link{numeric} value specifying the column number
    #' used as identifier.
    #'
    initialize = function(dataset, class.index = NULL, class.values = NULL,
                          positive.class = NULL, feature.id = NULL) {
      if (any(is.null(dataset), nrow(dataset) == 0, !is.data.frame(dataset))) {
        stop("[", class(self)[1], "][FATAL] Dataset empty or incorrect ",
             "(must be a data.frame). Aborting...")
      }
      private$data <- dataset
      if (any(is.null(class.index), is.null(class.values), is.null(positive.class))) {
        message("[", class(self)[1], "][INFO] Subset created without an associated class")
        class.index <- NULL
        class.values <- NULL
        positive.class <- NULL
        private$class.name <- NULL
        private$positive.class <- NULL
        private$feature.names <- names(private$data)
      } else {
        if (!is.numeric(class.index) || !class.index %in% c(1:ncol(dataset))) {
          stop("[", class(self)[1], "][FATAL] Class index parameter is incorrect. ",
               "Must be between 1 and ", ncol(dataset), ". Aborting...")
        }

        if (!is.factor(class.values)) {
          stop("[", class(self)[1], "][FATAL] Class values parameter must be defined ",
               "as 'factor' type. Aborting...")
        }
        private$positive.class <- positive.class

        if (!private$positive.class %in% dataset[[class.index]]) {
          stop("[", class(self)[1], "][FATAL] Positive Class parameter is incorrect. ",
               "Must be '", paste(levels(class.values), collapse = "' '"), "'. Aborting...")
        }

        class.values <- relevel(x = factor(class.values,
                                           levels = unique(class.values)),
                                ref = as.character(private$positive.class))

        if (!all(class.values == relevel(x = factor(dataset[[class.index]],
                                                    levels = unique(dataset[[class.index]])),
                                         ref = as.character(private$positive.class)))) {
          stop("[", class(self)[1], "][FATAL] Class values parameter is incorrect. ",
               "Must match with the values in column ", class.index, " in the ",
               "dataset. Aborting...")
        }

        private$class.name <- names(private$data)[class.index]
        private$feature.names <- names(private$data[, -class.index])
      }

      private$class.index <- class.index
      private$feature.id <- feature.id
      private$class.values <- class.values
    },
    #'
    #' @description Get the name of the columns comprising the subset.
    #'
    #' @return A \link{character} vector containing the name of each column.
    #'
    getColumnNames = function() { private$feature.names },
    #'
    #' @description Gets the values of all features or those indicated by
    #' arguments.
    #'
    #' @param feature.names A \link{character} vector comprising the name of the
    #' features to be obtained.
    #'
    #' @return A \link{character} vector or NULL if subset is empty.
    #'
    getFeatures = function(feature.names = NULL) {
      if (is.vector(feature.names) && length(feature.names) > 0) {
        if (is.null(private$class.index)) {
          private$data[, feature.names]
        } else {
          private$data[intersect(names(private$data[, -private$class.index]),
                                 feature.names)]
        }
      } else {
        if (is.null(private$class.index)) {
          private$data
        } else {
          private$data[, -private$class.index]
        }
      }
    },
    #'
    #' @description Gets the column name used as identifier.
    #'
    #' @return A \link{character} vector of size 1 of NULL if column id is not
    #' defined.
    #'
    getID = function() {
      if (!is.null(private$feature.id))
        private$feature.names[private$feature.id]
      else private$feature.id
    },
    #'
    #' @description Creates the \link{DIterator} object.
    #'
    #' @param chunk.size An \link{integer} value indicating the size of chunks taken
    #' over each iteration. By default chunk.size is defined as 10000.
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #'
    #' @return A \code{\link{DIterator}} object to transverse through
    #' \code{\link{Subset}} instances.
    #'
    getIterator = function(chunk.size = private$chunk.size, verbose = FALSE) {
      if (!is.numeric(chunk.size)) {
        message("[", class(self)[1], "][WARNING] Chunk size is not valid. ",
                "Assuming default value")
        chunk.size <- private$chunk.size
      }

      if (!is.logical(verbose)) {
        message("[", class(self)[1], "][WARNING] Verbose type is not valid. ",
                "Assuming 'FALSE' as default value")
        verbose <- FALSE
      }
      DIterator$new(data = private$data, chunk.size = chunk.size,
                    verbose = verbose)
    },
    #'
    #' @description Gets all the values of the target class.
    #'
    #' @return A \link{factor} vector with all the values of the target class.
    #'
    getClassValues = function() { private$class.values },
    #'
    #' @description The function is used to compute the ratio of each class
    #' value in the \code{\link{Subset}}.
    #'
    #' @param target.value The class value used as reference to perform the
    #' comparison.
    #'
    #' @return A \link{numeric} value.
    #'
    getClassBalance = function(target.value = NULL) {
      if (is.null(private$class.index)) {
        message("[", class(self)[1], "][WARNING] Subset has no associated class. ",
                "Task not performed")
      } else {
        if (is.null(target.value)) {
          target.value <- private$positive.class
        } else {
          if (!(target.value %in% private$class.values)) {
            message("[", class(self)[1], "][WARNING] Target class not found. ",
                    "Assuming default '", private$positive.class, "' value")
            target.value <- private$positive.class
          }
        }
        count <- as.data.frame(t(as.matrix(table(private$data[, private$class.index]))))
        round(count[, target.value] / sum(count[, which(names(count) != target.value)]), digits = 3)
      }
    },
    #'
    #' @description The function is used to obtain the index of the column
    #' containing the target class.
    #'
    #' @return A \link{numeric} value.
    #'
    getClassIndex = function() { private$class.index },
    #'
    #' @description The function is used to specify the name of the column
    #' containing the target class.
    #'
    #' @return A \link{character} value.
    #'
    getClassName = function() { private$class.name },
    #'
    #' @description The function is in charge of obtaining the number of columns
    #' comprising the \code{\link{Subset}}. See \code{\link{ncol}} for more
    #' information.
    #'
    #' @return An \link{integer} of length 1 or \link{NULL}.
    #'
    getNcol = function() { ncol(private$data) },
    #'
    #' @description The function is used to determine the number of rows present
    #' in the \code{\link{Subset}}. See \code{\link{nrow}} for more information.
    #'
    #' @return An \link{integer} of length 1 or \link{NULL}.
    #'
    getNrow = function() { nrow(private$data) },
    #'
    #' @description The function returns the value of the positive class.
    #'
    #' @return A \link{character} vector of size 1 or \link{NULL} if not defined.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function is used to check if the \link{Subset} contains
    #' a target class.
    #'
    #' @return A \link{logical} value where \link{TRUE} represents the absence
    #' of target class and \link{FALSE} its presence.
    #'
    isBlinded = function() { FALSE }
  ),
  private = list(
    data = NULL,
    class.index = NULL,
    class.name = NULL,
    feature.names = NULL,
    class.values = NULL,
    positive.class = NULL,
    chunk.size = 10000,
    feature.id = NULL
  )
)
