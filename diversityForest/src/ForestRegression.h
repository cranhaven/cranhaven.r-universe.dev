/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#ifndef FORESTREGRESSION_H_
#define FORESTREGRESSION_H_

#include <iostream>
#include <vector>

#include "globals.h"
#include "Forest.h"

namespace diversityForest {

class ForestRegression: public Forest {
public:
  ForestRegression() = default;

  ForestRegression(const ForestRegression&) = delete;
  ForestRegression& operator=(const ForestRegression&) = delete;

  virtual ~ForestRegression() override = default;

  void loadForest(size_t dependent_varID, size_t num_trees,
      std::vector<std::vector<std::vector<size_t>> >& forest_child_nodeIDs,
      std::vector<std::vector<size_t>>& forest_split_varIDs, std::vector<std::vector<double>>& forest_split_values,
	  std::vector<std::vector<size_t>>& forest_split_types, std::vector<std::vector<std::vector<size_t>>>& forest_split_multvarIDs, 
	  std::vector<std::vector<std::vector<std::vector<bool>>>>& forest_split_directs, 
	  std::vector<std::vector<std::vector<std::vector<double>>>>& forest_split_multvalues,
      std::vector<bool>& is_ordered_variable);

private:
  void initInternal(std::string status_variable_name) override;
  void growInternal() override;
  void allocatePredictMemory() override;
  void predictInternal(size_t sample_idx) override;
  void computePredictionErrorInternal() override;
  void saveToFileInternal(std::ofstream& outfile) override;

private:
  double getTreePrediction(size_t tree_idx, size_t sample_idx) const;
  double getTreePredictionMultivariate(size_t tree_idx, size_t sample_idx) const;
  size_t getTreePredictionTerminalNodeID(size_t tree_idx, size_t sample_idx) const;
};

} // namespace diversityForest

#endif /* FORESTREGRESSION_H_ */
