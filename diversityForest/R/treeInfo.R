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
treeInfo <- function(object, tree = 1) {
  if (!inherits(object, "divfor") & !inherits(object, "holdoutRF")) {
    stop("Error: Invalid class of input object.")
  } 
  forest <- object$forest
  if (is.null(forest)) {
    stop("Error: No saved forest in divfor object. Please set write.forest to TRUE when calling divfor.")
  }
  if (is.null(forest$dependent.varID) || is.null(forest$num.trees) ||
      is.null(forest$child.nodeIDs) || is.null(forest$split.varIDs) ||
      is.null(forest$split.values) || is.null(forest$independent.variable.names) ||
      is.null(forest$treetype)) {
    stop("Error: Invalid forest object.")
  }
  if (forest$treetype == "Survival" && (is.null(forest$status.varID)  ||
                                        is.null(forest$chf) || is.null(forest$unique.death.times))) {
    stop("Error: Invalid forest object.")
  }
  if (length(forest$child.nodeIDs) != forest$num.trees || length(forest$child.nodeIDs[[1]]) != 2) {
    stop("Error: Invalid forest object. Is the forest grown in divfor version <0.3.9? Try with the same version the forest was grown.")
  }
  if (tree > forest$num.trees) {
    stop("Error: Requesting tree ", tree, ", but forest has only ", forest$num.trees, " trees.")
  }
  
  result <- data.frame(nodeID = 0:(length(forest$split.values[[tree]]) - 1),
                       leftChild = forest$child.nodeIDs[[tree]][[1]], 
                       rightChild = forest$child.nodeIDs[[tree]][[2]], 
                       splitvarID = forest$split.varIDs[[tree]], 
                       splitvarName = "X",
                       splitval = forest$split.values[[tree]], 
                       terminal = FALSE)
  
  result$leftChild[result$leftChild == 0] <- NA
  result$rightChild[result$rightChild == 0] <- NA
  result$terminal[is.na(result$leftChild)] <- TRUE
  result$splitvarID[result$terminal] <- NA
  result$splitvarName[result$terminal] <- NA
  result$splitval[result$terminal] <- NA

  ## Get names of splitting variables 
  # should be -1 for all >= dependent.varID but +1 change for 1-index
  # for survival another -1 if >= status.varID
  independent.varID <- result$splitvarID
  idx <- !is.na(result$splitvarID) & result$splitvarID < forest$dependent.varID
  independent.varID[idx] <- result$splitvarID[idx] + 1
  if (forest$treetype == "Survival") {
    idx <- !is.na(result$splitvarID) & result$splitvarID >= forest$status.varID
    independent.varID[idx] <- independent.varID[idx] - 1
  }
  result$splitvarName <- forest$independent.variable.names[independent.varID]

  ## Unordered splitting
  idx.unordered <- !result$terminal & !forest$is.ordered[result$splitvarID + 1]
  if (any(idx.unordered)) {
    result$splitval[idx.unordered] <- sapply(result$splitval[idx.unordered], function(x) {
      paste(which(as.logical(intToBits(x))), collapse = ",")
    })
  }
  
  ## Prediction
  if (forest$treetype == "Classification") {
    result$prediction <- forest$split.values[[tree]]
    result$prediction[!result$terminal] <- NA
    if (!is.null(forest$levels)) {
      result$prediction <- factor(result$prediction, levels = forest$class.values, labels = forest$levels)
    }
  } else if (forest$treetype == "Regression") {
    result$prediction <- forest$split.values[[tree]]
    result$prediction[!result$terminal] <- NA
  } else if (forest$treetype == "Probability estimation") {
    predictions <- matrix(nrow = nrow(result), ncol = length(forest$levels))
    predictions[result$terminal, ] <- do.call(rbind, forest$terminal.class.counts[[tree]])
    colnames(predictions) <- paste0("pred.", forest$levels)
    result <- data.frame(result, predictions)
  } else if (forest$treetype == "Survival") {
    # No prediction for survival (CHF too large?)
  } else {
    stop("Error: Unknown tree type.")
  }
  
  result
}
