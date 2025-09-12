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

#' @title Feature-clustering based on Spearman Correlation Test.
#'
#' @description Performs the feature-clustering using Spearman's rho statistic.
#'
#' @details  Spearman's rho statistic is to estimate a rank-based measure of
#' association. These tests may be used if the data do not necessarily come from
#' a bivariate normal distribution.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[stats]{cor.test}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export SpearmanHeuristic

SpearmanHeuristic <- R6::R6Class(
  classname = "SpearmanHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Creates a \link{SpearmanHeuristic} object.
    #'
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    #'
    #' @description Test for correlation between paired samples using Spearman
    #' rho statistic.
    #'
    #' @param col1 A \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 A \link{numeric} vector or matrix to perform the clustering
    #' operation.
    #' @param column.names An optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return A \link{numeric} vector of length 1 or \link{NA} if an error
    #' occurs.
    #'
    #' @importFrom stats cor.test
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (!is.numeric(col1) || !is.numeric(col2)) {
        message("[", class(self)[1], "][WARNING] Columns must be 'numeric' type. ",
                "Returning NA")
        NA
      } else {
        tryCatch(
        stats::cor.test(col1, col2, method = "spearman", exact = FALSE)$p.value,
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "spearman heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)
