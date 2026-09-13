/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#ifndef FORESTCLASSIFICATION_H_
#define FORESTCLASSIFICATION_H_

#include <iostream>
#include <map>
#include <utility>
#include <vector>

#include "globals.h"
#include "Forest.h"
#include "TreeClassification.h"

namespace diversityForest {

class ForestClassification: public Forest {
public:
  ForestClassification() = default;

  ForestClassification(const ForestClassification&) = delete;
  ForestClassification& operator=(const ForestClassification&) = delete;

  virtual ~ForestClassification() override = default;

  void loadForest(size_t dependent_varID, size_t num_trees,
      std::vector<std::vector<std::vector<size_t>>>& forest_child_nodeIDs,
      std::vector<std::vector<size_t>>& forest_split_varIDs, std::vector<std::vector<double>>& forest_split_values,
      std::vector<std::vector<size_t>>& forest_split_types, std::vector<std::vector<std::vector<size_t>>>& forest_split_multvarIDs, 
	  std::vector<std::vector<std::vector<std::vector<bool>>>>& forest_split_directs, 
	  std::vector<std::vector<std::vector<std::vector<double>>>>& forest_split_multvalues,
	  std::vector<std::vector<std::vector<size_t>>>& forest_child_muwnodeIDs,
	  std::vector<std::vector<std::vector<double>>>& forest_split_muwvalues,	  
	  std::vector<double>& class_values, std::vector<bool>& is_ordered_variable);

  const std::vector<double>& getClassValues() const {
    return class_values;
  }

  void setClassWeights(std::vector<double>& class_weights) {
    this->class_weights = class_weights;
  }

  std::vector<std::vector<std::vector<double>>> getSplitMuwValues() {
    std::vector<std::vector<std::vector<double>>> result;
    for (auto &tree : trees) {
      TreeClassification *treeClass = dynamic_cast<TreeClassification *>(tree.get());
      if (treeClass)
      {
        result.push_back(treeClass->getSplitMuwValues());
      }
    }
    return result;
  }

  std::vector<std::vector<std::vector<size_t>>> getChildMuwNodeIDs() {
    std::vector<std::vector<std::vector<size_t>>> result;
    for (auto &tree : trees) {
      TreeClassification *treeClass = dynamic_cast<TreeClassification *>(tree.get());
      if (treeClass)
      {
        result.push_back(treeClass->getChildMuwNodeIDs());
      }
    }
    return result;
  }

  const std::vector<double>& getVariableImportanceMuwMultiway() const {
    return var_imp_classfoc;
  }
  
  const std::vector<double>& getVariableImportanceMuwDiscr() const {
    return var_imp_discr;
  }

protected:
  void initInternal(std::string status_variable_name) override;
  void growInternal() override;
  void allocatePredictMemory() override;
  void predictInternal(size_t sample_idx) override;
  void computePredictionErrorInternal() override;
  void saveToFileInternal(std::ofstream& outfile) override;

  void computeImportanceMuw() override;
  
  void computeTreeImportanceMuwInThread(uint thread_idx, std::vector<double>& importance_classfoc,
     std::vector<double>& importance_discr);

  // Classes of the dependent variable and classIDs for responses
  std::vector<double> class_values;
  std::vector<uint> response_classIDs;
  std::vector<std::vector<size_t>> sampleIDs_per_class;

  // Splitting weights
  std::vector<double> class_weights;

  // Table with classifications and true classes
  std::map<std::pair<double, double>, size_t> classification_table;

  // Variable importance for multiway and binary splits:
  std::vector<double> var_imp_classfoc;
  std::vector<double> var_imp_discr;

private:
  double getTreePrediction(size_t tree_idx, size_t sample_idx) const;
  double getTreePredictionMultivariate(size_t tree_idx, size_t sample_idx) const;
  double getTreePredictionMuw(size_t tree_idx, size_t sample_idx) const;
  size_t getTreePredictionTerminalNodeID(size_t tree_idx, size_t sample_idx) const;
};

} // namespace diversityForest

#endif /* FORESTCLASSIFICATION_H_ */
