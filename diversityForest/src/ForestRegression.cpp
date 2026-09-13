/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#include <algorithm>
#include <stdexcept>
#include <string>

#include "utility.h"
#include "ForestRegression.h"
#include "TreeRegression.h"
#include "Data.h"

namespace diversityForest {

void ForestRegression::loadForest(size_t dependent_varID, size_t num_trees,
    std::vector<std::vector<std::vector<size_t>> >& forest_child_nodeIDs,
    std::vector<std::vector<size_t>>& forest_split_varIDs, std::vector<std::vector<double>>& forest_split_values,
	std::vector<std::vector<size_t>>& forest_split_types, std::vector<std::vector<std::vector<size_t>>>& forest_split_multvarIDs, 
	std::vector<std::vector<std::vector<std::vector<bool>>>>& forest_split_directs, std::vector<std::vector<std::vector<std::vector<double>>>>& forest_split_multvalues,
    std::vector<bool>& is_ordered_variable) {

  this->dependent_varID = dependent_varID;
  this->num_trees = num_trees;
  data->setIsOrderedVariable(is_ordered_variable);

  // Create trees
  trees.reserve(num_trees);
  for (size_t i = 0; i < num_trees; ++i) {
    trees.push_back(
        std::make_unique<TreeRegression>(forest_child_nodeIDs[i], forest_split_varIDs[i], forest_split_values[i], forest_split_types[i], forest_split_multvarIDs[i], 
	    forest_split_directs[i], forest_split_multvalues[i]));
  }

  // Create thread ranges
  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
}

void ForestRegression::initInternal(std::string status_variable_name) {

  // If npairs not set, use floored square root of number of independent variables.
  if (npairs == 0) {
    unsigned long temp = (size_t)ceil(sqrt((double) (num_variables - 1)) / 2);
	npairs = temp;
  }

  // If mtry not set, use floored square root of number of independent variables
  if (mtry == 0) {
    unsigned long temp = sqrt((double) (num_variables - 1));
    mtry = std::max((unsigned long) 1, temp);
  }
  
      // If proptry not set, use floored square root of number of independent variables divided by number of independent variables.
  if (proptry == 0.0) {
    double temp = sqrt((double) (num_variables - 1)) / (double) (num_variables - 1);
    proptry = std::min((double) 1, temp);
  }

  // Set minimal node size
  if (min_node_size == 0) {
    min_node_size = DEFAULT_MIN_NODE_SIZE_REGRESSION;
  }

  // Sort data if memory saving mode
  if (!memory_saving_splitting) {
    data->sort();
  }
}

void ForestRegression::growInternal() {
  trees.reserve(num_trees);
  for (size_t i = 0; i < num_trees; ++i) {
    trees.push_back(std::make_unique<TreeRegression>());
  }
}

void ForestRegression::allocatePredictMemory() {
  size_t num_prediction_samples = data->getNumRows();
  if (predict_all || prediction_type == TERMINALNODES) {
    predictions = std::vector<std::vector<std::vector<double>>>(1,
        std::vector<std::vector<double>>(num_prediction_samples, std::vector<double>(num_trees)));
  } else {
    predictions = std::vector<std::vector<std::vector<double>>>(1,
        std::vector<std::vector<double>>(1, std::vector<double>(num_prediction_samples)));
  }
}

void ForestRegression::predictInternal(size_t sample_idx) {
  if (predict_all || prediction_type == TERMINALNODES) {
    // Get all tree predictions
    for (size_t tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
      if (prediction_type == TERMINALNODES) {
        predictions[0][sample_idx][tree_idx] = getTreePredictionTerminalNodeID(tree_idx, sample_idx);
      } else {
		if (divfortype == 1) {
          predictions[0][sample_idx][tree_idx] = getTreePrediction(tree_idx, sample_idx);
		}
		if (divfortype == 2) {
          predictions[0][sample_idx][tree_idx] = getTreePredictionMultivariate(tree_idx, sample_idx);
		}
      }
    }
  } else {
    // Mean over trees
    double prediction_sum = 0;
    for (size_t tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
      if (divfortype == 1) {
        prediction_sum += getTreePrediction(tree_idx, sample_idx);
	  }
	  if (divfortype == 2) {
		prediction_sum += getTreePredictionMultivariate(tree_idx, sample_idx);
	  }
    }
    predictions[0][0][sample_idx] = prediction_sum / num_trees;
  }
}

void ForestRegression::computePredictionErrorInternal() {

// For each sample sum over trees where sample is OOB
  std::vector<size_t> samples_oob_count;
  predictions = std::vector<std::vector<std::vector<double>>>(1,
      std::vector<std::vector<double>>(1, std::vector<double>(num_samples, 0)));
  samples_oob_count.resize(num_samples, 0);
  for (size_t tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
    for (size_t sample_idx = 0; sample_idx < trees[tree_idx]->getNumSamplesOob(); ++sample_idx) {
      size_t sampleID = trees[tree_idx]->getOobSampleIDs()[sample_idx];
      double value;
		  if (divfortype == 1) {
        value = getTreePrediction(tree_idx, sample_idx);
	  }
	  if (divfortype == 2) {
		value = getTreePredictionMultivariate(tree_idx, sample_idx);
	  }
	  
      predictions[0][0][sampleID] += value;
      ++samples_oob_count[sampleID];
    }
  }

// MSE with predictions and true data
  size_t num_predictions = 0;
  overall_prediction_error = 0;
  for (size_t i = 0; i < predictions[0][0].size(); ++i) {
    if (samples_oob_count[i] > 0) {
      ++num_predictions;
      predictions[0][0][i] /= (double) samples_oob_count[i];
      double predicted_value = predictions[0][0][i];
      double real_value = data->get(i, dependent_varID);
      overall_prediction_error += (predicted_value - real_value) * (predicted_value - real_value);
    } else {
      predictions[0][0][i] = NAN;
    }
  }

  overall_prediction_error /= (double) num_predictions;
}

// #nocov start
void ForestRegression::saveToFileInternal(std::ofstream& outfile) {

// Write num_variables
  outfile.write((char*) &num_variables, sizeof(num_variables));

// Write treetype
  TreeType treetype = TREE_REGRESSION;
  outfile.write((char*) &treetype, sizeof(treetype));
}

double ForestRegression::getTreePrediction(size_t tree_idx, size_t sample_idx) const {
  const auto& tree = dynamic_cast<const TreeRegression&>(*trees[tree_idx]);
  return tree.getPrediction(sample_idx);
}

double ForestRegression::getTreePredictionMultivariate(size_t tree_idx, size_t sample_idx) const {
  const auto& tree = dynamic_cast<const TreeRegression&>(*trees[tree_idx]);
  return tree.getPredictionMultivariate(sample_idx);
}

size_t ForestRegression::getTreePredictionTerminalNodeID(size_t tree_idx, size_t sample_idx) const {
  const auto& tree = dynamic_cast<const TreeRegression&>(*trees[tree_idx]);
  return tree.getPredictionTerminalNodeID(sample_idx);
}

// #nocov end

}// namespace diversityForest
