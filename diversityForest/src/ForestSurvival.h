/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#ifndef FORESTSURVIVAL_H_
#define FORESTSURVIVAL_H_

#include <iostream>
#include <vector>

#include "globals.h"
#include "Forest.h"
#include "TreeSurvival.h"

namespace diversityForest {

class ForestSurvival: public Forest {
public:
  ForestSurvival() = default;

  ForestSurvival(const ForestSurvival&) = delete;
  ForestSurvival& operator=(const ForestSurvival&) = delete;

  virtual ~ForestSurvival() override = default;

  void loadForest(size_t dependent_varID, size_t num_trees,
      std::vector<std::vector<std::vector<size_t>> >& forest_child_nodeIDs,
      std::vector<std::vector<size_t>>& forest_split_varIDs, std::vector<std::vector<double>>& forest_split_values,
	  std::vector<std::vector<size_t>>& forest_split_types, std::vector<std::vector<std::vector<size_t>>>& forest_split_multvarIDs, 
	  std::vector<std::vector<std::vector<std::vector<bool>>>>& forest_split_directs, 
	  std::vector<std::vector<std::vector<std::vector<double>>>>& forest_split_multvalues,
      size_t status_varID, std::vector<std::vector<std::vector<double>> >& forest_chf,
      std::vector<double>& unique_timepoints, std::vector<bool>& is_ordered_variable);

  std::vector<std::vector<std::vector<double>>> getChf() const;

  size_t getStatusVarId() const {
    return status_varID;
  }
  const std::vector<double>& getUniqueTimepoints() const {
    return unique_timepoints;
  }

private:
  void initInternal(std::string status_variable_name) override;
  void growInternal() override;
  void allocatePredictMemory() override;
  void predictInternal(size_t sample_idx) override;
  void computePredictionErrorInternal() override;
  void saveToFileInternal(std::ofstream& outfile) override;

  size_t status_varID;
  std::vector<double> unique_timepoints;
  std::vector<size_t> response_timepointIDs;

private:
  const std::vector<double>& getTreePrediction(size_t tree_idx, size_t sample_idx) const;
  size_t getTreePredictionTerminalNodeID(size_t tree_idx, size_t sample_idx) const;
};

} // namespace diversityForest

#endif /* FORESTSURVIVAL_H_ */
