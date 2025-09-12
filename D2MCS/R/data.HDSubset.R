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

#' @title High Dimensional Subset handler.
#'
#' @description Creates a high dimensional subset from a \code{\link{HDDataset}}
#' object. Only the required instances are loaded in memory to avoid unnecessary
#' use of resources and memory.
#'
#' @details Use \code{\link{HDDataset}} to ensure the creation of a valid
#' \code{\link{HDSubset}} object.
#'
#' @seealso \code{\link{HDDataset}}, \code{\link{DatasetLoader}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export HDSubset
#'
HDSubset <- R6::R6Class(
  classname = "HDSubset",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param file.path The name of the file which the data are to be read from.
    #' Each row of the table appears as one line of the file. If it does not
    #' contain an _absolute_ path, the file name is _relative_ to the current
    #' working directory, '\code{getwd()}'.
    #' @param feature.names A \link{character} vector specifying the name of the
    #' features that should be included in the \code{\link{HDDataset}} object.
    #' @param feature.id An \link{integer} or \link{character} indicating the
    #' column (number or name respectively) identifier. Default \link{NULL}
    #' value is valid ignores defining a identification column.
    #' @param start.at A \link{numeric} value to identify the reading start
    #' position.
    #' @param sep the field separator character. Values on each line of the file
    #' are separated by this character.
    #' @param chunk.size an \link{integer} value indicating the size of chunks
    #' taken over each iteration. By default chunk.size is defined as 10000.
    #'
    initialize = function(file.path, feature.names, feature.id, start.at = 0,
                          sep = ",", chunk.size) {
      if (is.null(feature.names) || ncol(feature.names) == 0) {
        stop("[", class(self)[1], "][FATAL] Dataset has not being preloaded. ",
             "Aborting...")
      }
      private$chunk.size <- chunk.size
      private$file.path <- file.path
      private$feature.names <- names(feature.names)
      private$index <- 0
      private$sep <- sep
      if (isFALSE(feature.id))
        private$feature.id <- NULL
      else private$feature.id <- feature.id

      if (!is.numeric(start.at) || start.at < 0) {
        message("[", class(self)[1], "][WARNING] Starting point must be a ",
                "non-negative numeric value. Assuming 0 as default value")
        private$start.at <- 0
      } else private$start.at <- start.at
    },
    #'
    #' @description Gets the name of the columns comprising the subset.
    #'
    #' @return A \link{character} vector containing the name of each column.
    #'
    getColumnNames = function() { private$feature.names },
    #'
    #' @description Obtains the number of columns present in the dataset.
    #'
    #' @return A \link{numeric} value or 0 if is empty.
    #'
    getNcol = function() { length(private$feature.names) },
    #'
    #' @description Obtains the column identifier.
    #'
    #' @return A \link{character} vector of size 1.
    #'
    getID = function() { private$feature.names[private$feature.id] },
    #'
    #' @description Creates the \code{\link{FIterator}} object.
    #'
    #' @param chunk.size An \link{integer} value indicating the size of chunks
    #' taken over each iteration. By default \code{chunk.size} is defined as
    #' 10000.
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #'
    #' @return A \code{\link{FIterator}} object to transverse through
    #' \code{\link{HDSubset}} instances
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

      it.params <- list(file.path = private$file.path,
                        feature.names = private$feature.names,
                        start = private$start.at, sep = private$sep)
      FIterator$new(it.params, chunk.size, verbose = verbose)
    },
    #'
    #' @description Checks if the subset contains a target class.
    #'
    #' @return A \link{logical} to specify if the subset contains a target class
    #' or not.
    #'
    isBlinded = function() { TRUE }
  ),
  private = list(
    feature.names = NULL,
    file.path = NULL,
    index = 0,
    start.at = 0,
    sep = 0,
    data.chunk = NULL,
    chunk.size = 100000,
    feature.id = NULL
  )
)
