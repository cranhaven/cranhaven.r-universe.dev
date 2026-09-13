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
csrf <- function(formula, training_data, test_data, params1 = list(), params2 = list()) {
  ## Grow a random forest on the training data to obtain weights
  rf.proximity <- do.call(divfor, c(list(formula = formula, data = training_data, 
                                         write.forest = TRUE), params1))
  
  ## Get terminal nodes
  terminal.nodeIDs.train <- predict(rf.proximity, training_data, type = "terminalNodes")$predictions
  terminal.nodeIDs.test <- predict(rf.proximity, test_data, type = "terminalNodes")$predictions
  
  ## Grow weighted RFs for test observations, predict the outcome
  predictions <- sapply(1:nrow(test_data), function(i) {
    ## Compute weights from first RF
    num.same.node <- rowSums(terminal.nodeIDs.test[i, ] == terminal.nodeIDs.train)
    weights <- num.same.node / sum(num.same.node)
    
    ## Grow weighted RF
    rf.prediction <- do.call(divfor, c(list(formula = formula, data = training_data, 
                                            write.forest = TRUE, case.weights = weights), 
                                       params2))
    
    ## Predict outcome
    predict(rf.prediction, test_data[i, ])$predictions
  })
  
  ## Return predictions
  predictions
}



