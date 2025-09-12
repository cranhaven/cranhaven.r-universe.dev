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

#' @title Dataset creation.
#'
#' @description Wrapper class able to automatically create a
#' \code{\link{Dataset}}, \code{\link{HDDataset}} according to the input data.
#'
#' @seealso \code{\link{Dataset}}, \code{\link{HDDataset}}
#'
#' @keywords datasets manip attribute connection file datagen
#'
#' @import R6
#'
#' @export DatasetLoader
#'
#' @examples
#' \dontrun{
#' # Create Dataset Handler object.
#'   loader <- DatasetLoader$new()
#'
#'   # Load input file.
#'   data <- loader$load(filepath = system.file(file.path("examples",
#'                                                        "hcc-data-complete-balanced.csv"),
#'                                              package = "D2MCS"),
#'                       header = T, normalize.names = T)
#' }
#'
DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { self },
    #'
    #' @description Stores the input source into a \code{\link{Dataset}} or
    #' \code{\link{HDDataset}} type object.
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
    #' @param skip.lines Defines the number of header lines should be skipped.
    #' @param normalize.names A \link{logical} value indicating whether the
    #' columns names should be automatically renamed to ensure R compatibility.
    #' @param string.as.factor A \link{logical} value indicating if character
    #' columns should be converted to factors (default = FALSE).
    #' @param ignore.columns Specify the columns from the input file that should
    #' be ignored.
    #'
    #' @return A \code{\link{Dataset}} or \code{\link{HDDataset}} object.
    #'
    #' @importFrom dplyr between
    #'
    load = function(filepath, header = TRUE, sep = ",", skip.lines = 0,
                    normalize.names = FALSE, string.as.factor = FALSE,
                    ignore.columns = NULL) {

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[", class(self)[1], "][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if (dplyr::between(dt.size, 0, 1)) {

        dataset <- Dataset$new(filepath = filepath, header = header, sep = sep,
                               skip = skip.lines, normalize.names = normalize.names,
                               ignore.columns = ignore.columns)
      } else {
        dataset <- HDDataset$new(filepath = filepath, header = header, sep = sep,
                                 skip = skip.lines, normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)
