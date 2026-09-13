/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#ifndef TREEREGRESSION_H_
#define TREEREGRESSION_H_

#include <vector>

#include "globals.h"
#include "Tree.h"

namespace diversityForest {

class TreeRegression: public Tree {
public:
  TreeRegression() = default;

  // Create from loaded forest
  TreeRegression(std::vector<std::vector<size_t>>& child_nodeIDs, std::vector<size_t>& split_varIDs,
      std::vector<double>& split_values, std::vector<size_t>& split_types, 
	std::vector<std::vector<size_t>>& split_multvarIDs, std::vector<std::vector<std::vector<bool>>>& split_directs, 
	std::vector<std::vector<std::vector<double>>>& split_multvalues);

  TreeRegression(const TreeRegression&) = delete;
  TreeRegression& operator=(const TreeRegression&) = delete;

  virtual ~TreeRegression() override = default;

  void allocateMemory() override;

  double estimate(size_t nodeID);
  void computePermutationImportanceInternal(std::vector<std::vector<size_t>>* permutations);
  void appendToFileInternal(std::ofstream& file) override;

  double getPrediction(size_t sampleID) const {
    size_t terminal_nodeID = prediction_terminal_nodeIDs[sampleID];
    return (split_values[terminal_nodeID]);
  }
  
  double getPredictionMultivariate(size_t sampleID) const {
    size_t terminal_nodeID = prediction_terminal_nodeIDs[sampleID];
    return (split_multvalues[terminal_nodeID][0][0]);
  }

  size_t getPredictionTerminalNodeID(size_t sampleID) const {
    return prediction_terminal_nodeIDs[sampleID];
  }

private:
  bool splitNodeInternal(size_t nodeID, std::vector<size_t>& possible_split_varIDs) override;
  bool splitNodeUnivariateInternal(size_t nodeID, std::vector<std::pair<size_t, double>> sampled_varIDs_values) override;
  bool splitNodeMultivariateInternal(size_t nodeID, std::vector<size_t> sampled_split_types, std::vector<std::vector<size_t>> sampled_split_multvarIDs, std::vector<std::vector<std::vector<bool>>> sampled_split_directs, std::vector<std::vector<std::vector<double>>> sampled_split_multvalues) override;
  void createEmptyNodeInternal() override;

  double computePredictionAccuracyInternal() override;

  // Called by splitNodeInternal(). Sets split_varIDs and split_values.
  bool findBestSplit(size_t nodeID, std::vector<size_t>& possible_split_varIDs);
  bool findBestSplitUnivariate(size_t nodeID, std::vector<std::pair<size_t, double>> sampled_varIDs_values);
  bool findBestSplitMultivariate(size_t nodeID, std::vector<size_t> sampled_split_types, std::vector<std::vector<size_t>> sampled_split_multvarIDs, std::vector<std::vector<std::vector<bool>>> sampled_split_directs, std::vector<std::vector<std::vector<double>>> sampled_split_multvalues);
  void findBestSplitValueSmallQ(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease);
  void findBestSplitValueSmallQ(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease, std::vector<double> possible_split_values,
      std::vector<double>& sums_right, std::vector<size_t>& n_right);
  void findBestSplitValueLargeQ(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease);
  void findBestSplitValueUnordered(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease);

  bool findBestSplitMaxstat(size_t nodeID, std::vector<size_t>& possible_split_varIDs);

  bool findBestSplitExtraTrees(size_t nodeID, std::vector<size_t>& possible_split_varIDs);
  void findBestSplitValueExtraTrees(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease);
  void findBestSplitValueExtraTrees(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease, std::vector<double> possible_split_values,
      std::vector<double>& sums_right, std::vector<size_t>& n_right);
  void findBestSplitValueExtraTreesUnordered(size_t nodeID, size_t varID, double sum_node, size_t num_samples_node,
      double& best_value, size_t& best_varID, double& best_decrease);

  void addImpurityImportance(size_t nodeID, size_t varID, double decrease);

  double computePredictionMSE();

  void cleanUpInternal() override {
    counter.clear();
    counter.shrink_to_fit();
    sums.clear();
    sums.shrink_to_fit();
  }

  std::vector<size_t> counter;
  std::vector<double> sums;
};

} // namespace diversityForest

#endif /* TREEREGRESSION_H_ */
