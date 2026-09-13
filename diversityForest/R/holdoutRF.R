# -------------------------------------------------------------------------------
#   This file is part of 'diversityForest'.
#
# 'diversityForest' is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# 'diversityForest' is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with 'diversityForest'. If not, see <http://www.gnu.org/licenses/>.
#
#  NOTE: 'diversityForest' is a fork of the popular R package 'ranger', written by Marvin N. Wright.
#  Most R and C++ code is identical with that of 'ranger'. The package 'diversityForest'
#  was written by taking the original 'ranger' code and making any
#  changes necessary to implement diversity forests.
#
# -------------------------------------------------------------------------------

# Author: Marvin N. Wright
holdoutRF <- function(...) {
  
  ## Get data from arguments
  args <- list(...)
  if ("data" %in% names(args)) {
    data <- args$data
  } else {
    data <- args[[2]]
  }
  
  ## Split data
  if ("gwaa.data" %in% class(data)) {
    n <- nrow(data@phdata) 
  } else {
    n <- nrow(data)
  }
  weights <- rbinom(n, 1, 0.5)
  
  ## Check args
  if ("case.weights" %in% names(args)) {
    stop("Error: Argument 'case.weights' not supported in holdoutRF.")
  }
  if ("holdout" %in% names(args)) {
    stop("Error: Argument 'holdout' not supported in holdoutRF.")
  }
  if ("importance" %in% names(args)) {
    stop("Error: Argument 'importance' not supported in holdoutRF. Always set to 'permutation'.")
  }
  if ("replace" %in% names(args)) {
    stop("Error: Argument 'replace' not supported in holdoutRF.")
  }
  
  ## Grow RFs
  res <- list(
    rf1 = divfor(..., importance = "permutation",  
                 case.weights = weights, replace = FALSE, holdout = TRUE),
    rf2 = divfor(..., importance = "permutation",
                 case.weights = 1-weights, replace = FALSE, holdout = TRUE)
  )
  
  ## Compute importance
  res$variable.importance <- (res$rf1$variable.importance + res$rf2$variable.importance)/2
  res$treetype <- res$rf1$treetype
  res$importance.mode <- res$rf1$importance.mode
  class(res) <- "holdoutRF"
  
  res
}
