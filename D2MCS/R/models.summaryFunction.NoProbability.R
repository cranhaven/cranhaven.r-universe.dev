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

#' @title Compute performance across resamples.
#'
#' @description Computes the performance across resamples when class
#' probabilities cannot be computed.
#'
#' @seealso \code{\link{SummaryFunction}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export NoProbability

NoProbability <- R6::R6Class(
  classname = "NoProbability",
  inherit = SummaryFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description The function defined during runtime the usage of five
    #' measures: 'Kappa', 'Accuracy', 'TCR_9', 'MCC' and 'PPV'.
    #'
    initialize = function() {
      super$initialize(c("Kappa", "Accuracy", "TCR_9", "MCC", "PPV"))
    },
    #'
    #' @description The function computes the performance across resamples using
    #' the previously defined measures.
    #'
    #' @param data A \link{data.frame} containing the data used to
    #' compute the performance.
    #' @param lev An optional value used to define the levels of the target
    #' class.
    #' @param model An optional value used to define the M.L. model used.
    #'
    #' @return A vector of performance estimates.
    #'
    #' @import caret
    #' @importFrom mltools mcc
    #'
    execute = function(data, lev = NULL, model = NULL) {
      lvls <- levels(data$obs)
      if (length(lvls) > 2)
        stop("[", class(self)[1], "][FATAL] Your outcome has ", length(lvls),
             " levels. The 'NoProbability' function is not appropriate. Aborting...")

      if (!all(levels(data[, "pred"]) == lvls))
        stop("[", class(self)[1], "][FATAL] Levels of observed and ",
             "predicted data do not match. Aborting...")

      data$y = as.numeric(data$obs == lvls[2])
      data$z = as.numeric(data$pred == lvls[2])

      confMat <- caret::confusionMatrix(table(data$z, data$y), positive = "1")
      fn_tcr_9 <- (9 * confMat$table[1, 2] + confMat$table[2, 1]) / (9 * (confMat$table[1, 2] + confMat$table[2, 2]) + confMat$table[2, 1] + confMat$table[1, 1])
      mcc <- mltools::mcc(TP = confMat$table[1, 1], FP = confMat$table[1, 2], TN = confMat$table[2, 2], FN = confMat$table[2, 1])
      ppv <- (confMat$table[1, 1] / (confMat$table[1, 1] + confMat$table[1, 2]))
      out <- c(confMat$overall['Kappa'], confMat$overall['Accuracy'], fn_tcr_9, mcc, ppv)
      names(out) <- c("Kappa", "Accuracy", "TCR_9", "MCC", "PPV")
      out
    }
  )
)
