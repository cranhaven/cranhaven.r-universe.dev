/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#include <iterator>
#include <cmath>

#include <Rcpp.h>

#include "Tree.h"
#include "utility.h"

namespace diversityForest
{

  Tree::Tree() : dependent_varID(0), mtry(0), nsplits(0), npairs(0), proptry(0.0), num_samples(0), num_samples_oob(0), min_node_size(0), deterministic_varIDs(0), split_select_varIDs(
                                                                                                                                                           0),
                 split_select_weights(0), case_weights(0), manual_inbag(0), oob_sampleIDs(0), promispairs(0), eim_mode(0), divfortype(0), holdout(false), keep_inbag(
                                                                                                                  false),
                 data(0), variable_importance(0), importance_mode(DEFAULT_IMPORTANCE_MODE), sample_with_replacement(
                                                                                                true),
                 sample_fraction(0), memory_saving_splitting(false), splitrule(DEFAULT_SPLITRULE), alpha(DEFAULT_ALPHA), minprop(
                                                                                                                             DEFAULT_MINPROP),
                 num_random_splits(DEFAULT_NUM_RANDOM_SPLITS), max_depth(DEFAULT_MAXDEPTH), depth(0), last_left_nodeID(0)
  { 
  }

  Tree::Tree(std::vector<std::vector<size_t>> &child_nodeIDs, std::vector<size_t> &split_varIDs,
             std::vector<double> &split_values, std::vector<size_t> &split_types, std::vector<std::vector<size_t>> &split_multvarIDs,
             std::vector<std::vector<std::vector<bool>>> &split_directs,
             std::vector<std::vector<std::vector<double>>> &split_multvalues) : dependent_varID(0), mtry(0), nsplits(0), npairs(0), proptry(0.0), num_samples(0), num_samples_oob(0), min_node_size(0), deterministic_varIDs(0), split_select_varIDs(0), split_select_weights(0), case_weights(0), manual_inbag(0), split_varIDs(split_varIDs), split_values(split_values), split_types(split_types), split_multvarIDs(split_multvarIDs), split_directs(split_directs), split_multvalues(split_multvalues), child_nodeIDs(child_nodeIDs), oob_sampleIDs(0), promispairs(0), eim_mode(0), divfortype(0), holdout(false), keep_inbag(false), data(0), variable_importance(0), importance_mode(DEFAULT_IMPORTANCE_MODE), sample_with_replacement(true), sample_fraction(0), memory_saving_splitting(false), splitrule(DEFAULT_SPLITRULE), alpha(DEFAULT_ALPHA), minprop(DEFAULT_MINPROP), num_random_splits(DEFAULT_NUM_RANDOM_SPLITS), max_depth(DEFAULT_MAXDEPTH), depth(0), last_left_nodeID(0)
  {
  }

  void Tree::init(const Data *data, uint mtry, uint nsplits, uint npairs, double proptry, size_t dependent_varID, size_t num_samples, uint seed,
                  std::vector<size_t> *deterministic_varIDs, std::vector<size_t> *split_select_varIDs,
                  std::vector<double> *split_select_weights, ImportanceMode importance_mode, uint min_node_size,
                  bool sample_with_replacement, bool memory_saving_splitting, SplitRule splitrule, std::vector<double> *case_weights,
                  std::vector<size_t> *manual_inbag, bool keep_inbag, std::vector<double> *sample_fraction, double alpha,
                  double minprop, bool holdout, uint num_random_splits, uint max_depth, std::vector<std::vector<size_t>> *promispairs, uint eim_mode, uint divfortype)
  {

    this->data = data;
    this->mtry = mtry;
    this->dependent_varID = dependent_varID;
    this->num_samples = num_samples;
    this->memory_saving_splitting = memory_saving_splitting;
    this->nsplits = nsplits;
	this->npairs = npairs;
    this->proptry = proptry;
    this->divfortype = divfortype;

    // Create root node, assign bootstrap sample and oob samples
    child_nodeIDs.push_back(std::vector<size_t>());
    child_nodeIDs.push_back(std::vector<size_t>());
    if (divfortype == 1)
    {
      createEmptyNode();
    }
    if (divfortype == 2)
    {
      createEmptyNodeMultivariate();
    }
    if (divfortype == 3)
    {
      createEmptyNodeInternal();
    }
	
    // Initialize random number generator and set seed
    random_number_generator.seed(seed);

    this->deterministic_varIDs = deterministic_varIDs;
    this->split_select_varIDs = split_select_varIDs;
    this->split_select_weights = split_select_weights;
    this->importance_mode = importance_mode;
    this->min_node_size = min_node_size;
    this->sample_with_replacement = sample_with_replacement;
    this->splitrule = splitrule;
    this->case_weights = case_weights;
    this->manual_inbag = manual_inbag;
    this->keep_inbag = keep_inbag;
    this->sample_fraction = sample_fraction;
    this->holdout = holdout;
    this->alpha = alpha;
    this->minprop = minprop;
    this->num_random_splits = num_random_splits;
    this->eim_mode = eim_mode;
    this->max_depth = max_depth;
    this->promispairs = promispairs;
  }

  void Tree::grow(std::vector<double> *variable_importance)
  {
    // Allocate memory for tree growing
    allocateMemory();

    this->variable_importance = variable_importance;

    // Bootstrap, dependent if weighted or not and with or without replacement
    if (!case_weights->empty())
    {
      if (sample_with_replacement)
      {
        bootstrapWeighted();
      }
      else
      {
        bootstrapWithoutReplacementWeighted();
      }
    }
    else if (sample_fraction->size() > 1)
    {
      if (sample_with_replacement)
      {
        bootstrapClassWise();
      }
      else
      {
        bootstrapWithoutReplacementClassWise();
      }
    }
    else if (!manual_inbag->empty())
    {
      setManualInbag();
    }
    else
    {
      if (sample_with_replacement)
      {
        bootstrap();
      }
      else
      {
        bootstrapWithoutReplacement();
      }
    }

    // Init start and end positions
    start_pos[0] = 0;
    end_pos[0] = sampleIDs.size();

    // While not all nodes terminal, split next node
    size_t num_open_nodes = 1;
    size_t i = 0;
    depth = 0;
    while (num_open_nodes > 0)
    {
      // Split node
      bool is_terminal_node = splitNode(i);
      if (is_terminal_node)
      {
        --num_open_nodes;
      }
      else
      {
        ++num_open_nodes;
        if (i >= last_left_nodeID)
        {
          // If new level, increase depth
          // (left_node saves left-most node in current level, new level reached if that node is splitted)
          if (divfortype == 1)
          {
            last_left_nodeID = split_varIDs.size() - 2;
          }
          if (divfortype == 2)
          {
            last_left_nodeID = split_multvarIDs.size() - 2;
          }
          ++depth;
        }
      }
      ++i;
    }

    // Delete sampleID vector to save memory
    sampleIDs.clear();
    sampleIDs.shrink_to_fit();
    cleanUpInternal();
  }

  void Tree::predict(const Data *prediction_data, bool oob_prediction)
  {

    size_t num_samples_predict;
    if (oob_prediction)
    {
      num_samples_predict = num_samples_oob;
    }
    else
    {
      num_samples_predict = prediction_data->getNumRows();
    }

    prediction_terminal_nodeIDs.resize(num_samples_predict, 0);

    // For each sample start in root, drop down the tree and return final value
    for (size_t i = 0; i < num_samples_predict; ++i)
    {
      size_t sample_idx;
      if (oob_prediction)
      {
        sample_idx = oob_sampleIDs[i];
      }
      else
      {
        sample_idx = i;
      }
      size_t nodeID = 0;
      while (1)
      {

        // Break if terminal node
        if (child_nodeIDs[0][nodeID] == 0 && child_nodeIDs[1][nodeID] == 0)
        {
          break;
        }

        // Move to child
        size_t split_varID = split_varIDs[nodeID];

        double value = prediction_data->get(sample_idx, split_varID);
        if (prediction_data->isOrderedVariable(split_varID))
        {
          if (value <= split_values[nodeID])
          {
            // Move to left child
            nodeID = child_nodeIDs[0][nodeID];
          }
          else
          {
            // Move to right child
            nodeID = child_nodeIDs[1][nodeID];
          }
        }
        else
        {
          size_t factorID = floor(value) - 1;
          size_t splitID = floor(split_values[nodeID]);

          // Left if 0 found at position factorID
          if (!(splitID & (1 << factorID)))
          {
            // Move to left child
            nodeID = child_nodeIDs[0][nodeID];
          }
          else
          {
            // Move to right child
            nodeID = child_nodeIDs[1][nodeID];
          }
        }
      }

      prediction_terminal_nodeIDs[i] = nodeID;
    }
  }

  void Tree::predictMultivariate(const Data *prediction_data, bool oob_prediction)
  {

    size_t num_samples_predict;
    if (oob_prediction)
    {
      num_samples_predict = num_samples_oob;
    }
    else
    {
      num_samples_predict = prediction_data->getNumRows();
    }

    prediction_terminal_nodeIDs.resize(num_samples_predict, 0);

    // For each sample start in root, drop down the tree and return final value
    for (size_t i = 0; i < num_samples_predict; ++i)
    {
      size_t sample_idx;
      if (oob_prediction)
      {
        sample_idx = oob_sampleIDs[i];
      }
      else
      {
        sample_idx = i;
      }
      size_t nodeID = 0;
      while (1)
      {

        // Break if terminal node
        if (child_nodeIDs[0][nodeID] == 0 && child_nodeIDs[1][nodeID] == 0)
        {
          break;
        }

        // Move to child

        bool inrectangle = IsInRectangle(prediction_data, sample_idx, split_types[nodeID], split_multvarIDs[nodeID], split_directs[nodeID], split_multvalues[nodeID]);
        if (inrectangle)
        {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        }
        else
        {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
        ////}
        ////else
        ////{
        ////  size_t factorID = floor(value) - 1;
        ////  size_t splitID = floor(split_values[nodeID]);
        ////
        ////  // Left if 0 found at position factorID
        ////  if (!(splitID & (1 << factorID)))
        ////  {
        ////    // Move to left child
        ////    nodeID = child_nodeIDs[0][nodeID];
        ////  }
        ////  else
        ////  {
        ////    // Move to right child
        ////    nodeID = child_nodeIDs[1][nodeID];
        ////  }
        ////}
      }

      prediction_terminal_nodeIDs[i] = nodeID;
    }
  }
  
  void Tree::predictMuw(const Data *prediction_data, bool oob_prediction)
  {
	 // Empty on purpose (virtual function only implemented in classification and probability)
  }

  void Tree::computePermutationImportance(std::vector<double> &forest_importance, std::vector<double> &forest_variance)
  {

    size_t num_independent_variables = data->getNumCols() - data->getNoSplitVariables().size();

	// Compute normal prediction accuracy for each tree. Predictions already computed..
    double accuracy_normal = computePredictionAccuracyInternal();

    prediction_terminal_nodeIDs.clear();
    prediction_terminal_nodeIDs.resize(num_samples_oob, 0);

    // Reserve space for permutations, initialize with oob_sampleIDs
    std::vector<size_t> permutations(oob_sampleIDs);

    // Randomly permute for all independent variables
    for (size_t i = 0; i < num_independent_variables; ++i)
    {

      // Skip no split variables
      size_t varID = i;
      for (auto &skip : data->getNoSplitVariables())
      {
        if (varID >= skip)
        {
          ++varID;
        }
      }

      // If variable is not used for splitting, skip it
      double accuracy_difference = 0;
      bool iscontained = false;
      for (size_t j = 0; j < split_varIDs.size(); ++j)
      {
        if (split_varIDs[j] == varID)
        {
          iscontained = true;
          break;
        }
      }
      if (!iscontained)
      {
        forest_importance[i] += 0;
      }
      else
      {
      // Permute and compute prediction accuracy again for this permutation and save difference
	  permuteAndPredictOobSamples(varID, permutations);
      double accuracy_permuted = computePredictionAccuracyInternal();
      accuracy_difference = accuracy_normal - accuracy_permuted;
      forest_importance[i] += accuracy_difference;
      }
		
      // Compute variance
      if (importance_mode == IMP_PERM_BREIMAN)
      {
        forest_variance[i] += accuracy_difference * accuracy_difference;
      }
      else if (importance_mode == IMP_PERM_LIAW)
      {
        forest_variance[i] += accuracy_difference * accuracy_difference * num_samples_oob;
      }
    }
  }

// Interaction Forests: Compute EIM values for the tree:
  void Tree::computePermutationImportanceMultivariate(std::vector<double> &forest_univ, std::vector<double> &forest_bivpooled,
                                                      std::vector<double> &forest_bivqual, std::vector<double> &forest_bivquant_ll,
                                                      std::vector<double> &forest_bivquant_lh, std::vector<double> &forest_bivquant_hl, std::vector<double> &forest_bivquant_hh)
  {

    size_t num_independent_variables = data->getNumCols() - data->getNoSplitVariables().size();

    // Compute normal prediction accuracy for each tree. Predictions already computed..
    double accuracy_normal = computePredictionAccuracyInternal();

    // Compute univariate EIM values:

    prediction_terminal_nodeIDs.clear();
    prediction_terminal_nodeIDs.resize(num_samples_oob, 0);

    std::vector<size_t> permuted_multvarID;
    permuted_multvarID.resize(1);

    for (size_t i = 0; i < num_independent_variables; ++i)
    {

      // Skip no split variables
      size_t varID = i;
      for (auto &skip : data->getNoSplitVariables())
      {
        if (varID >= skip)
        {
          ++varID;
        }
      }

          bool iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_multvarIDs[j].size() == 1)
            {
              if (split_multvarIDs[j][0] == varID)
              {
                iscontained = true;
                break;
              }
            }
          }

          if (!iscontained)
          {
            forest_univ[i] += 0;
          }
          else
          {
      // Drop OOB obervations down the tree, where the decision is randomized, if the
      // split uses permuted_multvarID, and re-calculate accuracy:
      permuted_multvarID[0] = varID;
      randomizedDropDownOobSamples(permuted_multvarID, 1);
      double accuracy_randomized = computePredictionAccuracyInternal();

      double accuracy_difference = accuracy_normal - accuracy_randomized;
      forest_univ[i] += accuracy_difference;
          }

    }

    if (eim_mode != 5)
    {

      // Compute bivariable EIM values:

      prediction_terminal_nodeIDs.clear();
      prediction_terminal_nodeIDs.resize(num_samples_oob, 0);

      permuted_multvarID.resize(2);

      for (size_t i = 0; i < (*promispairs).size(); ++i)
      {

        permuted_multvarID = (*promispairs)[i];

        // Skip variables not to use for splitting:
        for (size_t j = 0; j < 2; ++j)
        {

          for (auto &skip_value : data->getNoSplitVariables())
          {
            if (permuted_multvarID[j] >= skip_value)
            {
              ++permuted_multvarID[j];
            }
          }
        }

        // Drop OOB obervations down the tree, where the decision is randomized, if the
        // split uses permuted_multvarID, and re-calculate accuracy:

        // "pooled":
        if (eim_mode == 1)
        {

          // Check whether at least one split in the tree uses
		  // the variable pair permuted_multvarID:
          bool iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_multvarIDs[j].size() == 2)
            {
              std::vector<size_t> vectemp = split_multvarIDs[j];
              std::sort(vectemp.begin(), vectemp.end());
              if (vectemp[0] == permuted_multvarID[0] && vectemp[1] == permuted_multvarID[1])
              {
                iscontained = true;
                break;
              }
            }
          }

          if (!iscontained)
          {
            // If the variable pair is not used for splitting in the tree, this variable pair does
		    // not change the predictions, which is why the EIM value for that variable pair in
		    // that tree is zero:
            forest_bivpooled[i] += 0;
          }
          else
          {
            randomizedDropDownOobSamples(permuted_multvarID, 2);
            double accuracy_randomized = computePredictionAccuracyInternal();

            double accuracy_difference = accuracy_normal - accuracy_randomized;
            forest_bivpooled[i] += accuracy_difference;
          }
        }
				
        // qualitative EIM values:
        if (eim_mode == 2 || eim_mode == 3)
        {

          bool iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_types[j] == 6)
            {
              std::vector<size_t> vectemp = split_multvarIDs[j];
              std::sort(vectemp.begin(), vectemp.end());
              if (vectemp[0] == permuted_multvarID[0] && vectemp[1] == permuted_multvarID[1])
              {
                iscontained = true;
                break;
              }
            }
          }

          if (!iscontained)
          {
            forest_bivqual[i] += 0;
          }
          else
          {
            randomizedDropDownOobSamples(permuted_multvarID, 3); // permuteAndPredictOobSamples
            double accuracy_randomized = computePredictionAccuracyInternal();

            double accuracy_difference = accuracy_normal - accuracy_randomized;
            forest_bivqual[i] += accuracy_difference;
          }
		  
        }
        // "quantitative":
        if (eim_mode == 2 || eim_mode == 4)
        {

          bool iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_types[j] == 2)
            {
              std::vector<size_t> vectemp = split_multvarIDs[j];
              std::sort(vectemp.begin(), vectemp.end());
              if (vectemp[0] == permuted_multvarID[0] && vectemp[1] == permuted_multvarID[1])
              {
                iscontained = true;
                break;
              }
            }
          }

          if (!iscontained)
          {
            forest_bivquant_ll[i] += 0;
          }
          else
          {
            randomizedDropDownOobSamples(permuted_multvarID, 4); // permuteAndPredictOobSamples
            double accuracy_randomized = computePredictionAccuracyInternal();

            double accuracy_difference = accuracy_normal - accuracy_randomized;
            forest_bivquant_ll[i] += accuracy_difference;
          }

          iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_types[j] == 3 || split_types[j] == 4)
            {

              std::vector<size_t> vectemp = split_multvarIDs[j];
              bool firstsmaller = (vectemp[0] < vectemp[1]);
              std::sort(vectemp.begin(), vectemp.end());
              if (vectemp[0] == permuted_multvarID[0] && vectemp[1] == permuted_multvarID[1])
              {
                if ((split_types[j] == 3 && firstsmaller) || (split_types[j] == 4 && !firstsmaller))
                {
                  iscontained = true;
                  break;
                }
              }
            }
          }

          if (!iscontained)
          {
            forest_bivquant_lh[i] += 0;
          }
          else
          {
            randomizedDropDownOobSamples(permuted_multvarID, 5); // permuteAndPredictOobSamples
            double accuracy_randomized = computePredictionAccuracyInternal();

            double accuracy_difference = accuracy_normal - accuracy_randomized;
            forest_bivquant_lh[i] += accuracy_difference;
          }

          iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_types[j] == 3 || split_types[j] == 4)
            {

              std::vector<size_t> vectemp = split_multvarIDs[j];
              bool firstsmaller = (vectemp[0] < vectemp[1]);
              std::sort(vectemp.begin(), vectemp.end());
              if (vectemp[0] == permuted_multvarID[0] && vectemp[1] == permuted_multvarID[1])
              {
                if ((split_types[j] == 4 && firstsmaller) || (split_types[j] == 3 && !firstsmaller))
                {
                  iscontained = true;
                  break;
                }
              }
            }
          }

          if (!iscontained)
          {
            forest_bivquant_hl[i] += 0;
          }
          else
          {
            randomizedDropDownOobSamples(permuted_multvarID, 6); // permuteAndPredictOobSamples
            double accuracy_randomized = computePredictionAccuracyInternal();

            double accuracy_difference = accuracy_normal - accuracy_randomized;
            forest_bivquant_hl[i] += accuracy_difference;
          }

          iscontained = false;

          for (size_t j = 0; j < split_multvarIDs.size(); ++j)
          {
            if (split_types[j] == 5)
            {
              std::vector<size_t> vectemp = split_multvarIDs[j];
              std::sort(vectemp.begin(), vectemp.end());
              if (vectemp[0] == permuted_multvarID[0] && vectemp[1] == permuted_multvarID[1])
              {
                iscontained = true;
                break;
              }
            }
          }

          if (!iscontained)
          {
            forest_bivquant_hh[i] += 0;
          }
          else
          {
            randomizedDropDownOobSamples(permuted_multvarID, 7); // permuteAndPredictOobSamples
            double accuracy_randomized = computePredictionAccuracyInternal();

            double accuracy_difference = accuracy_normal - accuracy_randomized;
            forest_bivquant_hh[i] += accuracy_difference;
          }
        }
      }
    }
  }

  void Tree::appendToFile(std::ofstream &file)
  {

    // Save general fields
    saveVector2D(child_nodeIDs, file);
    saveVector1D(split_varIDs, file);
    saveVector1D(split_values, file);

    // Call special functions for subclasses to save special fields.
    appendToFileInternal(file);
  }

  void Tree::createPossibleSplitVarSubset(std::vector<size_t> &result)
  {

    size_t num_vars = data->getNumCols();

    // For corrected Gini importance add dummy variables
    if (importance_mode == IMP_GINI_CORRECTED)
    {
      num_vars += data->getNumCols() - data->getNoSplitVariables().size();
    }

    // Randomly add non-deterministic variables (according to weights if needed)
    if (split_select_weights->empty())
    {
      if (deterministic_varIDs->empty())
      {
        drawWithoutReplacementSkip(result, random_number_generator, num_vars, data->getNoSplitVariables(), mtry);
      }
      else
      {
        std::vector<size_t> skip;
        std::copy(data->getNoSplitVariables().begin(), data->getNoSplitVariables().end(),
                  std::inserter(skip, skip.end()));
        std::copy(deterministic_varIDs->begin(), deterministic_varIDs->end(), std::inserter(skip, skip.end()));
        std::sort(skip.begin(), skip.end());
        drawWithoutReplacementSkip(result, random_number_generator, num_vars, skip, mtry);
      }
    }
    else
    {
      drawWithoutReplacementWeighted(result, random_number_generator, *split_select_varIDs, mtry, *split_select_weights);
    }

    // Always use deterministic variables
    std::copy(deterministic_varIDs->begin(), deterministic_varIDs->end(), std::inserter(result, result.end()));
  }

  // New function.
  // This function samples the pairs of variable IDs and splits in these
  // variables.
  void Tree::drawSplitsUnivariate(size_t nodeID, size_t n_triedsplits, std::vector<std::pair<size_t, double>> &sampled_varIDs_values)
  {

    // Get the total number of variables
    size_t num_vars = data->getNumCols();

    // For corrected Gini importance add dummy variables
    if (importance_mode == IMP_GINI_CORRECTED)
    {
      num_vars += data->getNumCols() - data->getNoSplitVariables().size();
    }

    // Determine the indices of the covariates:
    ////////////////

    // REMARK: The covariates are not necessarily all variables
    // different from the target variable, but there may be more
    // variables which should not be used for splitting.
    // For example in the survival case, we have two variables
    // associated with the target variable, the time variable and
    // the censoring indicator. Apart from this, it is also possible
    // for the user to specify variables that should not be used
    // for splitting ("no split variables").
    // Therefore, when determening the indices of the covariates to
    // use, we have to cycle through all variables and skip those
    // variable that should not be used for splitting.

    // Initialize an empty vector of consecutive numbers 0, 1, 2, ...:
    // REMARK: This vector will be modified to exclude the "no split variables":
    std::vector<int> all_varIDsPre(num_vars - data->getNoSplitVariables().size());
    std::iota(all_varIDsPre.begin(), all_varIDsPre.end(), 0);

    // Initialize empty vector, which will contain the indices of
    // the covariates:
    std::vector<int> all_varIDs(num_vars - data->getNoSplitVariables().size());

    // Cycle through "all_varIDsPre" and skip the "no split variables":
    size_t countertemp = 0;
    size_t varIDtemp = 0;

    for (auto &varID : all_varIDsPre)
    {
      varIDtemp = varID;
      // Go through the "no split variables"; if the current variable
      // "varID" is equal to the respective "no split variable",
      // increase index of the current variable:
      for (auto &skip_value : data->getNoSplitVariables())
      {
        if (varIDtemp >= skip_value)
        {
          ++varIDtemp;
        }
      }
      all_varIDs[countertemp] = varIDtemp;
      ++countertemp;
    }

    // Cycle through all variables, count their numbers of split
    // points and add these up:
    ///////////////////////

    size_t n_splitstotal = 0;
    size_t n_triedsplitscandidate;
    for (auto &varID : all_varIDs)
    {

      // Create possible split values for variable 'varID'
      std::vector<double> possible_split_values;
      data->getAllValues(possible_split_values, sampleIDs, varID, start_pos[nodeID], end_pos[nodeID]);

      // Add the number of split values up to the total number of
      // split values:
      n_splitstotal += possible_split_values.size() - 1;

      // Break the loop, if the number n_triedsplitscandidate = proptry * n_splitstotal
      // already exceeds the maximum number of splits to sample:
      n_triedsplitscandidate = (size_t)((double)n_splitstotal * proptry + 0.5);

      if (n_triedsplitscandidate > n_triedsplits)
      {
        break;
      }
    }

    // If the calculated number of splits to sample
    // is larger than the maximum number of splits to sample 'nsplits',
    // use 'nsplits':
    n_triedsplits = std::min(n_triedsplits, n_triedsplitscandidate);

    // Sample the pairs of variable IDs and splits:
    //////////////////////

    // If "n_triedsplits" is zero no splits should be sampled,
    // which will result in findBestSplitUnivariate() returning
    // zero, leading the node splitting to stop:
    if (n_triedsplits > 0)
    {

      // Initialize:
      sampled_varIDs_values.reserve(n_triedsplits);

      // Random number generator for the covariates:
      std::uniform_int_distribution<size_t> unif_distvarID(0, num_vars - 1 - data->getNoSplitVariables().size());

      // Draw the covariate/split pairs by a loop:
      size_t drawnvarID;
      double drawnvalue;

      for (size_t i = 0; i < n_triedsplits; ++i)
      {

        std::pair<size_t, double> drawnpair;
        bool pairnotfound = false;

        // Loop that stops as soon "pairnotfound" becomes FALSE.
        do
        {

          // Draw a covariate, while skipping the "no split variables":
          drawnvarID = unif_distvarID(random_number_generator);
          for (auto &skip_value : data->getNoSplitVariables())
          {
            if (drawnvarID >= skip_value)
            {
              ++drawnvarID;
            }
          }

          // Create possible split values for variable 'varID':
          std::vector<double> possible_split_values;
          data->getAllValues(possible_split_values, sampleIDs, drawnvarID, start_pos[nodeID], end_pos[nodeID]);

          // The pair is declared not found if there is only one
          // or less possible split values in the drawn covariate
          // (and a new variable will be drawn as a consequence)
          // REMINDER: This might be computationally (very) ineffective
          // for higher dimensional data with many dichotome covariates,
          // because here it can happen that there will be no possible
          // splits in a large quantity of covariates after a few splits,
          // which might have the effect that the process of drawing the
          // covariate has to repeated many times before a suitable
          // covariate has been drawn. For this reason it might be
          // better to store the indices of the covariates for which
          // there are no splits left, so that these are not drawn
          // again and again.
          pairnotfound = possible_split_values.size() < 2;

          if (!pairnotfound)
          {

            // Determine the splits in the drawn covariates, which are the mid points
            // between the neighboring covariate values:
            std::vector<double> all_mid_points(possible_split_values.size() - 1);
            for (size_t i = 0; i < possible_split_values.size() - 1; ++i)
            {
              all_mid_points[i] = (possible_split_values[i] + possible_split_values[i + 1]) / 2;
            }

            // Random number generator for the splits:
            std::uniform_int_distribution<size_t> unif_distvalue(0, all_mid_points.size() - 1);

            // Draw a split:
            drawnvalue = all_mid_points[unif_distvalue(random_number_generator)];

            /*
        // Draw values:
        std::vector<double> drawnvalues;
        drawDoublesWithoutReplacement(drawnvalues, random_number_generator, possible_split_values, 2);

        // ...and take their average:
        drawnvalue = (drawnvalues[0] + drawnvalues[1]) / 2;

*/

            // Make the drawn covariate/split pair:
            drawnpair = std::make_pair(drawnvarID, drawnvalue);

            // Check whether this pair is already existent in the drawn pairs.
            // If this is the case, "pairnotfound" will be set to false
            // and the search for a suitable pair continues.
            pairnotfound = std::find(sampled_varIDs_values.begin(), sampled_varIDs_values.end(), drawnpair) != sampled_varIDs_values.end();
          }

        } while (pairnotfound);

        // Add the drawn pair to "sampled_varIDs_values":
        sampled_varIDs_values.push_back(drawnpair);
      }

      // Some console outputs I had used, while developing the function:
      //std::vector<size_t> gezogenevars;
      //std::vector<double> gezogenepunkte;
      //for (size_t i = 0; i < sampled_varIDs_values.size(); ++i) {
      //	gezogenevars.push_back(std::get<0>(sampled_varIDs_values[i]));
      //	gezogenepunkte.push_back(std::get<1>(sampled_varIDs_values[i]));
      //	}

    }
  }

  void Tree::drawSplitsMultivariate(size_t nodeID, size_t n_triedsplits, std::vector<size_t> &sampled_split_types, std::vector<std::vector<size_t>> &sampled_split_multvarIDs, std::vector<std::vector<std::vector<bool>>> &sampled_split_directs, std::vector<std::vector<std::vector<double>>> &sampled_split_multvalues)
  {

    // Number of features:
    size_t num_independent_variables = data->getNumCols() - data->getNoSplitVariables().size();

    // Random number generator for selecting a random number out of {1,2}:
    std::uniform_int_distribution<size_t> getoneortwo(1, 2);

    // Number of variables:
    size_t num_vars = data->getNumCols();

    // Number of promising feature pairs:
    size_t npromispairs = (*promispairs).size();

    // Random number generator for the promising feature pairs:
    std::uniform_int_distribution<size_t> unif_promispairs(0, npromispairs - 1);

    // For corrected Gini importance add dummy variables
    if (importance_mode == IMP_GINI_CORRECTED)
    {
      num_vars += data->getNumCols() - data->getNoSplitVariables().size();
    }
    // Random number generator for the covariates:
    std::uniform_int_distribution<size_t> unif_distvarID(0, num_independent_variables - 1);

    // Reserve space for the split information:

    ///////////////////size_t npairs = std::min((size_t)ceil(sqrt((double)num_independent_variables) / 2), (size_t)10);
    uint numbermaxtotal = ceil(npairs * 1.5);

    sampled_split_types.resize(7 * numbermaxtotal);
    sampled_split_multvarIDs.resize(7 * numbermaxtotal);
    sampled_split_directs.resize(7 * numbermaxtotal);
    sampled_split_multvalues.resize(7 * numbermaxtotal);

    // Initialize arrays that will contain the information on the
    // current sampled split:
    std::vector<size_t> drawn_types;
    std::vector<std::vector<size_t>> drawn_multvarIDs;
    std::vector<std::vector<std::vector<bool>>> drawn_directs;
    std::vector<std::vector<std::vector<double>>> drawn_multvalues;

    drawn_types.resize(7);
    drawn_multvarIDs.resize(7);
    drawn_directs.resize(7);
    drawn_multvalues.resize(7);

    drawn_multvarIDs[0].resize(1);
    drawn_multvarIDs[1].resize(1);
    for (size_t i = 2; i < 7; ++i)
    {
      drawn_multvarIDs[i].resize(2);
    }

    drawn_directs[0].resize(1);
    drawn_directs[0][0].resize(1);
    drawn_directs[1].resize(1);
    drawn_directs[1][0].resize(1);
    for (size_t i = 2; i < 7; ++i)
    {
      drawn_directs[i].resize(1);
      drawn_directs[i][0].resize(2);
    }

    drawn_multvalues[0].resize(1);
    drawn_multvalues[0][0].resize(1);
    drawn_multvalues[1].resize(1);
    drawn_multvalues[1][0].resize(1);
    for (size_t i = 2; i < 7; ++i)
    {
      drawn_multvalues[i].resize(1);
      drawn_multvalues[i][0].resize(2);
    }

    drawn_types = {1, 1, 2, 3, 4, 5, 6};

    drawn_directs[0][0][0] = true;
    drawn_directs[1][0][0] = true;
    drawn_directs[2][0] = {true, true};
    drawn_directs[3][0] = {true, false};
    drawn_directs[4][0] = {false, true};
    drawn_directs[5][0] = {false, false};
    drawn_directs[6][0] = {true, true};

    ///std::vector<std::vector<size_t>> varcombs = { { 0, 1 }, { 0, 2 }, { 1, 2 } };

    // Sample the n_triedsplits splits:

    size_t countit = 0;
    size_t countitall = 0;

    size_t countsplit = 0;

    size_t counttoeight = 0;

    while (countit < npairs && countitall < numbermaxtotal)
    {

      counttoeight = 0;

      //  Randomly select promising pair:
      size_t pairindex = unif_promispairs(random_number_generator);
      std::vector<size_t> drawnvarIDs = (*promispairs)[pairindex];

      // Randomly permute the order in the selected pair:
      size_t permutepair = getoneortwo(random_number_generator);
      if (permutepair == 1)
      {
        size_t firstelement = drawnvarIDs[0];
        drawnvarIDs[0] = drawnvarIDs[1];
        drawnvarIDs[1] = firstelement;
      }

      // Skip variables not to use for splitting:
      for (size_t i = 0; i < 2; ++i)
      {

        for (auto &skip_value : data->getNoSplitVariables())
        {
          if (drawnvarIDs[i] >= skip_value)
          {
            ++drawnvarIDs[i];
          }
        }
      }

      std::vector<double> values_variable1;
      data->getRawValues(values_variable1, sampleIDs, drawnvarIDs[0], start_pos[nodeID], end_pos[nodeID]);

      std::vector<double> values_variable2;
      data->getRawValues(values_variable2, sampleIDs, drawnvarIDs[1], start_pos[nodeID], end_pos[nodeID]);

      std::vector<double> possible_split_values_1 = values_variable1;
      // Sort the values:
      std::sort(possible_split_values_1.begin(), possible_split_values_1.end());
      possible_split_values_1.erase(std::unique(possible_split_values_1.begin(), possible_split_values_1.end()), possible_split_values_1.end());

      std::vector<double> possible_split_values_2 = values_variable2;
      // Sort the values:
      std::sort(possible_split_values_2.begin(), possible_split_values_2.end());
      possible_split_values_2.erase(std::unique(possible_split_values_2.begin(), possible_split_values_2.end()), possible_split_values_2.end());

      // Discard split, if there is no possible split:
      if (possible_split_values_1.size() < 2 || possible_split_values_2.size() < 2)
      {
        countitall++;
        continue;
      }




  std::uniform_int_distribution<size_t> unif_dist(0, possible_split_values_1.size() - 2);
size_t draw = unif_dist(random_number_generator);

        // ...and take their average:
        double xvalueuniv = (possible_split_values_1[draw] + possible_split_values_1[draw+1])/2;


  std::uniform_int_distribution<size_t> unif_dist2(0, possible_split_values_2.size() - 2);
draw = unif_dist2(random_number_generator);

        // ...and take their average:
        double yvalueuniv = (possible_split_values_2[draw] + possible_split_values_2[draw+1])/2;




		        sampled_split_types[countsplit] = drawn_types[counttoeight];
      sampled_split_directs[countsplit].resize(1);
      sampled_split_directs[countsplit][0].resize(1);
      sampled_split_directs[countsplit] = drawn_directs[counttoeight];
      sampled_split_multvarIDs[countsplit].resize(1);
      sampled_split_multvarIDs[countsplit][0] = drawnvarIDs[0];
      sampled_split_multvalues[countsplit].resize(1);
      sampled_split_multvalues[countsplit][0].resize(1);
      sampled_split_multvalues[countsplit][0][0] = xvalueuniv;
      countsplit++;
      counttoeight++;

      sampled_split_types[countsplit] = drawn_types[counttoeight];
      sampled_split_directs[countsplit].resize(1);
      sampled_split_directs[countsplit][0].resize(1);
      sampled_split_directs[countsplit] = drawn_directs[counttoeight];
      sampled_split_multvarIDs[countsplit].resize(1);
      sampled_split_multvarIDs[countsplit][0] = drawnvarIDs[1];
      sampled_split_multvalues[countsplit].resize(1);
      sampled_split_multvalues[countsplit][0].resize(1);
      sampled_split_multvalues[countsplit][0][0] = yvalueuniv;
      countsplit++;
      counttoeight++;






      size_t numbertried = 0;
      bool foundsplit = false;

      double lowerbound;
      double upperbound;
      double drawnxvalue;

      while (numbertried < 20 && !foundsplit)
      {

        // Draw values:
        std::vector<double> drawnvalues;
        drawDoublesWithoutReplacement(drawnvalues, random_number_generator, possible_split_values_1, 2);

        // ...and take their average:
        drawnxvalue = (drawnvalues[0] + drawnvalues[1]) / 2;


        // Get all values from feature 2, for which feature 1 is smaller
        // than the split point in feature 1:

        std::vector<double> values_variable2_smallx = values_variable2;

        size_t last = 0;
        for (size_t j = 0; j < values_variable2_smallx.size(); j++)
        {
          if (values_variable1[j] < drawnxvalue)
          {
            values_variable2_smallx[last] = values_variable2_smallx[j];
            last++;
          }
        }
        values_variable2_smallx.erase(values_variable2_smallx.begin() + last, values_variable2_smallx.end());

        // Get all values from feature 2, for which feature 1 is larger
        // than the split point in feature 1:

        std::vector<double> values_variable2_largex = values_variable2;

        last = 0;
        for (size_t j = 0; j < values_variable2_largex.size(); j++)
        {
          if (values_variable1[j] > drawnxvalue)
          {
            values_variable2_largex[last] = values_variable2_largex[j];
            last++;
          }
        }
        values_variable2_largex.erase(values_variable2_largex.begin() + last, values_variable2_largex.end());

        // Minimum and maximum values of the y values with x values smaller than p^{1, j_1} / larger than p^{2, j_1}:

        double maxyxsmall = *std::max_element(values_variable2_smallx.begin(), values_variable2_smallx.end());
        double maxyxlarge = *std::max_element(values_variable2_largex.begin(), values_variable2_largex.end());

        double minyxsmall = *std::min_element(values_variable2_smallx.begin(), values_variable2_smallx.end());
        double minyxlarge = *std::min_element(values_variable2_largex.begin(), values_variable2_largex.end());

        lowerbound = std::max(minyxsmall, minyxlarge);
        upperbound = std::min(maxyxsmall, maxyxlarge);

        if (lowerbound >= upperbound)
        {
          numbertried++;
          continue;
        }

        foundsplit = true;
      }

      if (!foundsplit)
      {
        countitall++;
        continue;
      }

      // Delete the y values with x values smaller than p^{1, j_1}
      // that are too large to deliver good splits:

      std::vector<double> possible_split_values_2_interval = possible_split_values_2;

      possible_split_values_2_interval.erase(std::remove_if(possible_split_values_2_interval.begin(), possible_split_values_2_interval.end(),
                                                            [lowerbound, upperbound](double n) { return n < lowerbound || n > upperbound; }),
                                             possible_split_values_2_interval.end());

      if (possible_split_values_2_interval.size() < 2)
      {
        countitall++;
        continue;
      }

      // Draw values:
      std::vector<double> drawnvalues;
      drawDoublesWithoutReplacement(drawnvalues, random_number_generator, possible_split_values_2_interval, 2);

      // ...and take their average:
      double drawnyvalue = (drawnvalues[0] + drawnvalues[1]) / 2;

      for (size_t i = 0; i < 5; ++i)
      {
        sampled_split_types[countsplit] = drawn_types[counttoeight];
        sampled_split_directs[countsplit].resize(1);
        sampled_split_directs[countsplit][0].resize(2);
        sampled_split_directs[countsplit] = drawn_directs[counttoeight];
        sampled_split_multvarIDs[countsplit].resize(2);
        sampled_split_multvarIDs[countsplit] = drawnvarIDs;
        sampled_split_multvalues[countsplit].resize(1);
        sampled_split_multvalues[countsplit][0].resize(2);
        sampled_split_multvalues[countsplit][0] = {drawnxvalue, drawnyvalue};
        countsplit++;
        counttoeight++;
      }

      countitall++;
      countit++;
    }

    // Delete empty elements from the vectors:

    ///Rcpp::Rcout << "countitall  " << countitall << std::endl;
    ///Rcpp::Rcout << "countit " << countit << std::endl;
    ///Rcpp::Rcout << "countsplit " << countsplit << std::endl;

    size_t numberkeep = std::min(countsplit, (size_t)7 * npairs);

    sampled_split_multvarIDs.resize(numberkeep);
    sampled_split_types.resize(numberkeep);
    sampled_split_directs.resize(numberkeep);
    sampled_split_multvalues.resize(numberkeep);

    if (sampled_split_multvarIDs.size() > 0)
    {

      std::vector<size_t> randindices(sampled_split_multvarIDs.size());
      std::iota(randindices.begin(), randindices.end(), 0);
      std::shuffle(randindices.begin(), randindices.end(), random_number_generator);

      //for (size_t i = 0; i < randindices.size(); ++i) {
      //  Rcpp::Rcout << "randindices: " << i << "    " << randindices[i] << std::endl;
      //}

      //for (size_t i = 0; i < sampled_split_multvalues.size(); ++i) {
      //  Rcpp::Rcout << "sampled_split_multvalues davor: " << i << "    " << sampled_split_multvalues[i][0][0] << std::endl;
      //}

      sampled_split_multvarIDs = reorder(sampled_split_multvarIDs, randindices);
      sampled_split_types = reorder(sampled_split_types, randindices);
      sampled_split_directs = reorder(sampled_split_directs, randindices);
      sampled_split_multvalues = reorder(sampled_split_multvalues, randindices);

      //for (size_t i = 0; i < sampled_split_multvalues.size(); ++i) {
      //  Rcpp::Rcout << "sampled_split_multvalues danach: " << i << "    " << sampled_split_multvalues[i][0][0] << std::endl;
      //}
    }

    //Rcpp::Rcout << "Check 27" << std::endl;
  }

  bool Tree::IsInRectangle(const Data *data, size_t sampleID, size_t split_type, std::vector<size_t> &split_multvarID, std::vector<std::vector<bool>> &split_direct, std::vector<std::vector<double>> &split_multvalue)
  {

    // For univariate splits, the value of the variable must be smaller than
    // the split point to be considered within the rectangle:
    if (split_type == 1)
    {

      if (data->get(sampleID, split_multvarID[0]) < split_multvalue[0][0])
      {
        return true;
      }
    }
    else if (split_type == 2)
    {

      // Get the x- and y-axis values of sampledID:
      double value1 = data->get(sampleID, split_multvarID[0]);
      double value2 = data->get(sampleID, split_multvarID[1]);

      // Split corresponding to quantiative interaction:

      // If sampleID is contained within the rectangle both with respect to
      // the x- and the y-dimension, it is actually contained
      // in the rectangle:
      if (value1 < split_multvalue[0][0] && value2 < split_multvalue[0][1])
      {
        return true;
      }
    }
    else if (split_type == 3)
    {

      // Get the x- and y-axis values of sampledID:
      double value1 = data->get(sampleID, split_multvarID[0]);
      double value2 = data->get(sampleID, split_multvarID[1]);

      // Split corresponding to quantiative interaction:

      // If sampleID is contained within the rectangle both with respect to
      // the x- and the y-dimension, it is actually contained
      // in the rectangle:
      if (value1 < split_multvalue[0][0] && value2 > split_multvalue[0][1])
      {
        return true;
      }
    }
    else if (split_type == 4)
    {

      // Get the x- and y-axis values of sampledID:
      double value1 = data->get(sampleID, split_multvarID[0]);
      double value2 = data->get(sampleID, split_multvarID[1]);

      // Split corresponding to quantiative interaction:

      // If sampleID is contained within the rectangle both with respect to
      // the x- and the y-dimension, it is actually contained
      // in the rectangle:
      if (value1 > split_multvalue[0][0] && value2 < split_multvalue[0][1])
      {
        return true;
      }
    }
    else if (split_type == 5)
    {

      // Get the x- and y-axis values of sampledID:
      double value1 = data->get(sampleID, split_multvarID[0]);
      double value2 = data->get(sampleID, split_multvarID[1]);

      // Split corresponding to quantiative interaction:

      // If sampleID is contained within the rectangle both with respect to
      // the x- and the y-dimension, it is actually contained
      // in the rectangle:
      if (value1 > split_multvalue[0][0] && value2 > split_multvalue[0][1])
      {
        return true;
      }
    }
    else
    {

      // Get the x- and y-axis values of sampledID:
      double value1 = data->get(sampleID, split_multvarID[0]);
      double value2 = data->get(sampleID, split_multvarID[1]);

      // Return true if sampleID is either contained in the first or in
      // the second rectangle:
      if ((value1 < split_multvalue[0][0] && value2 < split_multvalue[0][1]) || (value1 > split_multvalue[0][0] && value2 > split_multvalue[0][1]))
      {
        return true;
      }
    }

    // If sampleID is not contained in the rectangle(s) return false:
    return false;
  }

  bool Tree::splitNode(size_t nodeID)
  {

    bool stop;

    /// Rcpp::Rcout << "nodeID: " << nodeID << std::endl;

    if (divfortype == 1)
    {

      // Rcpp::Rcout << "Laenge sampled_split_types" << sampled_split_types.size() << std::endl;

      // Draw the variables and the candidate splits - after performing this step,
      // sampled_varIDs_values will contain the variables and candidate splits:
      size_t n_triedsplits = (size_t)nsplits;
      std::vector<std::pair<size_t, double>> sampled_varIDs_values;
      drawSplitsUnivariate(nodeID, n_triedsplits, sampled_varIDs_values);

      // Perform the splitting using the subclass method:
      stop = splitNodeUnivariateInternal(nodeID, sampled_varIDs_values);

      if (stop)
      {
        // Terminal node
        return true;
      }

      size_t split_varID = split_varIDs[nodeID];
      double split_value = split_values[nodeID];

      // Save non-permuted variable for prediction
      split_varIDs[nodeID] = data->getUnpermutedVarID(split_varID);

      // Create child nodes
      size_t left_child_nodeID = split_varIDs.size();
      child_nodeIDs[0][nodeID] = left_child_nodeID;
      createEmptyNode();
      start_pos[left_child_nodeID] = start_pos[nodeID];

      size_t right_child_nodeID = split_varIDs.size();
      child_nodeIDs[1][nodeID] = right_child_nodeID;
      createEmptyNode();
      start_pos[right_child_nodeID] = end_pos[nodeID];

      // For each sample in node, assign to left or right child
      if (data->isOrderedVariable(split_varID))
      {
        // Ordered: left is <= splitval and right is > splitval
        size_t pos = start_pos[nodeID];
        while (pos < start_pos[right_child_nodeID])
        {
          size_t sampleID = sampleIDs[pos];
          if (data->get(sampleID, split_varID) <= split_value)
          {
            // If going to left, do nothing
            ++pos;
          }
          else
          {
            // If going to right, move to right end
            --start_pos[right_child_nodeID];
            std::swap(sampleIDs[pos], sampleIDs[start_pos[right_child_nodeID]]);
          }
        }
      }
      else
      {
        // Unordered: If bit at position is 1 -> right, 0 -> left
        size_t pos = start_pos[nodeID];
        while (pos < start_pos[right_child_nodeID])
        {
          size_t sampleID = sampleIDs[pos];
          double level = data->get(sampleID, split_varID);
          size_t factorID = floor(level) - 1;
          size_t splitID = floor(split_value);

          // Left if 0 found at position factorID
          if (!(splitID & (1 << factorID)))
          {
            // If going to left, do nothing
            ++pos;
          }
          else
          {
            // If going to right, move to right end
            --start_pos[right_child_nodeID];
            std::swap(sampleIDs[pos], sampleIDs[start_pos[right_child_nodeID]]);
          }
        }
      }

      // End position of left child is start position of right child
      end_pos[left_child_nodeID] = start_pos[right_child_nodeID];
      end_pos[right_child_nodeID] = end_pos[nodeID];

      // Rcpp::Rcout << "left_child_nodeID: " << left_child_nodeID << std::endl;

      // No terminal node
      return false;
    }

    if (divfortype == 2)
    {

      size_t n_triedsplits = (size_t)nsplits;
      std::vector<size_t> sampled_split_types;
      std::vector<std::vector<size_t>> sampled_split_multvarIDs;
      std::vector<std::vector<std::vector<bool>>> sampled_split_directs;
      std::vector<std::vector<std::vector<double>>> sampled_split_multvalues;
      drawSplitsMultivariate(nodeID, n_triedsplits, sampled_split_types, sampled_split_multvarIDs, sampled_split_directs, sampled_split_multvalues);

      // Perform the splitting using the subclass method:
      stop = splitNodeMultivariateInternal(nodeID, sampled_split_types, sampled_split_multvarIDs, sampled_split_directs, sampled_split_multvalues);

      if (stop)
      {
        // Terminal node
        return true;
      }

      size_t split_type = split_types[nodeID];
      std::vector<size_t> split_multvarID = split_multvarIDs[nodeID];
      std::vector<std::vector<bool>> split_direct = split_directs[nodeID];
      std::vector<std::vector<double>> split_multvalue = split_multvalues[nodeID];

      // Save non-permuted variable for prediction
      for (size_t i = 0; i < split_multvarIDs[nodeID].size(); ++i)
      {
        split_multvarIDs[nodeID][i] = data->getUnpermutedVarID(split_multvarIDs[nodeID][i]);
      }

      // Create child nodes
      size_t left_child_nodeID = split_multvarIDs.size();
      child_nodeIDs[0][nodeID] = left_child_nodeID;
      createEmptyNodeMultivariate();
      start_pos[left_child_nodeID] = start_pos[nodeID];

      size_t right_child_nodeID = split_multvarIDs.size();
      child_nodeIDs[1][nodeID] = right_child_nodeID;
      createEmptyNodeMultivariate();
      start_pos[right_child_nodeID] = end_pos[nodeID];

      // For each sample in node, assign to left or right child
      /////if (data->isOrderedVariable(split_varID))  Remark: Currently only ordered variables
      /////{
      // Ordered: left is <= splitval and right is > splitval
      bool inrectangle;
      size_t pos = start_pos[nodeID];
      while (pos < start_pos[right_child_nodeID])
      {
        size_t sampleID = sampleIDs[pos];

        //// MAKE NEW FUNCTION IsInRectangle TO CHECK WHETHER IN RECTANGLE

        /// 1. Schritt: IsInRectangle in Tree.cpp definieren

        inrectangle = IsInRectangle(data, sampleID, split_type, split_multvarID, split_direct, split_multvalue);
        if (inrectangle)
        {
          // If going to left, do nothing
          ++pos;
        }
        else
        {
          // If going to right, move to right end
          --start_pos[right_child_nodeID];
          std::swap(sampleIDs[pos], sampleIDs[start_pos[right_child_nodeID]]);
        }
      }

      // End position of left child is start position of right child
      end_pos[left_child_nodeID] = start_pos[right_child_nodeID];
      end_pos[right_child_nodeID] = end_pos[nodeID];

      // Rcpp::Rcout << "left_child_nodeID: " << left_child_nodeID << std::endl;

      // No terminal node
      return false;
    }

    // To satisfy the compiler:
    return false;
  }

  void Tree::createEmptyNode()
  {
    split_varIDs.push_back(0);
    split_values.push_back(0);
    child_nodeIDs[0].push_back(0);
    child_nodeIDs[1].push_back(0);
    start_pos.push_back(0);
    end_pos.push_back(0);

    createEmptyNodeInternal();
  }

  void Tree::createEmptyNodeMultivariate()
  {

    split_types.push_back(0);
    split_multvarIDs.push_back(std::vector<size_t>());
    split_directs.push_back(std::vector<std::vector<bool>>());
    split_multvalues.push_back(std::vector<std::vector<double>>());
    child_nodeIDs[0].push_back(0);
    child_nodeIDs[1].push_back(0);
    start_pos.push_back(0);
    end_pos.push_back(0);

    createEmptyNodeInternal();
  }

  size_t Tree::dropDownSamplePermuted(size_t permuted_varID, size_t sampleID, size_t permuted_sampleID)
  {

    // Start in root and drop down
    size_t nodeID = 0;
    while (child_nodeIDs[0][nodeID] != 0 || child_nodeIDs[1][nodeID] != 0)
    {

      // Permute if variable is permutation variable
      size_t split_varID = split_varIDs[nodeID];
      size_t sampleID_final = sampleID;
      if (split_varID == permuted_varID)
      {
        sampleID_final = permuted_sampleID;
      }

      // Move to child
      double value = data->get(sampleID_final, split_varID);
      if (data->isOrderedVariable(split_varID))
      {
        if (value <= split_values[nodeID])
        {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        }
        else
        {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
      }
      else
      {
        size_t factorID = floor(value) - 1;
        size_t splitID = floor(split_values[nodeID]);

        // Left if 0 found at position factorID
        if (!(splitID & (1 << factorID)))
        {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        }
        else
        {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
      }
    }
    return nodeID;
  }

  // Interaction forest: Drop OOB sample sampleID down the tree and whenever a split uses
  // permuted_multvarID as split variable make random assignment to child node
  size_t Tree::randomizedDropDownSample(std::vector<size_t> permuted_multvarID, size_t sampleID, size_t effect_type)
  {

    // Start in root and drop down
    size_t nodeID = 0;
    while (child_nodeIDs[0][nodeID] != 0 || child_nodeIDs[1][nodeID] != 0)
    {

      // If the split uses permuted_multvarID for splitting make random
      // assignment
      bool isequal;
      if (effect_type == 1)
      {
        isequal = (split_multvarIDs[nodeID].size() == 1) && (permuted_multvarID[0] == split_multvarIDs[nodeID][0]);
      }
      else if (effect_type == 2)
      {
        std::vector<size_t> multvarIDstemp = split_multvarIDs[nodeID];
        std::sort(multvarIDstemp.begin(), multvarIDstemp.end());
        isequal = areArraysEqual(permuted_multvarID, multvarIDstemp);
      }
      else if (effect_type == 3)
      {
        std::vector<size_t> multvarIDstemp = split_multvarIDs[nodeID];
        std::sort(multvarIDstemp.begin(), multvarIDstemp.end());
        bool variablesequal = areArraysEqual(permuted_multvarID, multvarIDstemp);
        isequal = variablesequal && split_types[nodeID] == 6;
      }
      else if (effect_type == 4)
      {
        std::vector<size_t> multvarIDstemp = split_multvarIDs[nodeID];
        std::sort(multvarIDstemp.begin(), multvarIDstemp.end());
        bool variablesequal = areArraysEqual(permuted_multvarID, multvarIDstemp);
        isequal = variablesequal && split_types[nodeID] == 2;
      }
      else if (effect_type == 5)
      {
        std::vector<size_t> multvarIDstemp = split_multvarIDs[nodeID];
        if (multvarIDstemp.size() == 1)
        {
          isequal = false;
        }
        else
        {
          bool firstsmaller = (multvarIDstemp[0] < multvarIDstemp[1]);
          std::sort(multvarIDstemp.begin(), multvarIDstemp.end());
          bool variablesequal = areArraysEqual(permuted_multvarID, multvarIDstemp);
          if (firstsmaller)
          {
            isequal = variablesequal && split_types[nodeID] == 3;
          }
          else
          {
            isequal = variablesequal && split_types[nodeID] == 4;
          }
        }
      }
      else if (effect_type == 6)
      {
        std::vector<size_t> multvarIDstemp = split_multvarIDs[nodeID];
        if (multvarIDstemp.size() == 1)
        {
          isequal = false;
        }
        else
        {
          bool firstsmaller = (multvarIDstemp[0] < multvarIDstemp[1]);
          std::sort(multvarIDstemp.begin(), multvarIDstemp.end());
          bool variablesequal = areArraysEqual(permuted_multvarID, multvarIDstemp);
          if (firstsmaller)
          {
            isequal = variablesequal && split_types[nodeID] == 4;
          }
          else
          {
            isequal = variablesequal && split_types[nodeID] == 3;
          }
        }
      }
      else
      {
        std::vector<size_t> multvarIDstemp = split_multvarIDs[nodeID];
        std::sort(multvarIDstemp.begin(), multvarIDstemp.end());
        bool variablesequal = areArraysEqual(permuted_multvarID, multvarIDstemp);
        isequal = variablesequal && split_types[nodeID] == 5;
      }
      if (isequal)
      {

        // Randomly decide to which child node the observation
        // is assigned, where the probabilities for the two child
        // nodes are proportional to their sizes:
        bool toleft = randomAssignLeftChildNode(nodeID);
        if (toleft)
        {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        }
        else
        {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
      }
      else
      {

        // If the split does not use permuted_multvarID for splitting,
        // just assign observation to child node according to the
        // regular split point:

        bool inrectangle = IsInRectangle(data, sampleID, split_types[nodeID], split_multvarIDs[nodeID], split_directs[nodeID], split_multvalues[nodeID]);
        if (inrectangle)
        {
          // Move to left child
          nodeID = child_nodeIDs[0][nodeID];
        }
        else
        {
          // Move to right child
          nodeID = child_nodeIDs[1][nodeID];
        }
      }
    }
    return nodeID;
  }

  // Interaction Forest: Used for randomly assigning OOB observations to child nodes:
  // Calculates the sizes of the two child nodes and randomly assigns observation
  // to one of them, where the probabilities for assigning to either of these
  // are proportional to the child node sizes:
  bool Tree::randomAssignLeftChildNode(size_t nodeID)
  {

    // Sizes of the child nodes:
    size_t num_samples_leftnode = end_pos[child_nodeIDs[0][nodeID]] - start_pos[child_nodeIDs[0][nodeID]];
    size_t num_samples_rightnode = end_pos[child_nodeIDs[1][nodeID]] - start_pos[child_nodeIDs[1][nodeID]];

    // Left child node size divided by node size:
    double prob_left = (double)num_samples_leftnode / ((double)(num_samples_leftnode + num_samples_rightnode));

    // Return true with probability prob_left and false with probability 1 - prob_left:

    // Draw [0,1] uniformly distributed number:
    std::uniform_real_distribution<double> distribution(0.0, 1.0);
    double rand_num = distribution(random_number_generator);
    if (rand_num <= prob_left)
      return true;

    return false;
  }

  void Tree::permuteAndPredictOobSamples(size_t permuted_varID, std::vector<size_t> &permutations)
  {

    // Permute OOB sample
    //std::vector<size_t> permutations(oob_sampleIDs);
    std::shuffle(permutations.begin(), permutations.end(), random_number_generator);

    // For each sample, drop down the tree and add prediction
    for (size_t i = 0; i < num_samples_oob; ++i)
    {
      size_t nodeID = dropDownSamplePermuted(permuted_varID, oob_sampleIDs[i], permutations[i]);
      prediction_terminal_nodeIDs[i] = nodeID;
    }
  }

  // Interaction forest: Drop all OOB samples down the tree and whenever a split uses
  // permuted_multvarID as split variable make random assignment to child node
  void Tree::randomizedDropDownOobSamples(std::vector<size_t> permuted_multvarID, size_t effect_type)
  {

    // For each sample, drop down the tree and add prediction
    for (size_t i = 0; i < num_samples_oob; ++i)
    {
      size_t nodeID = randomizedDropDownSample(permuted_multvarID, oob_sampleIDs[i], effect_type);
      prediction_terminal_nodeIDs[i] = nodeID;
    }
  }

  void Tree::bootstrap()
  {

    // Use fraction (default 63.21%) of the samples
    size_t num_samples_inbag = (size_t)num_samples * (*sample_fraction)[0];

    // Reserve space, reserve a little more to be save)
    sampleIDs.reserve(num_samples_inbag);
    oob_sampleIDs.reserve(num_samples * (exp(-(*sample_fraction)[0]) + 0.1));

    std::uniform_int_distribution<size_t> unif_dist(0, num_samples - 1);

    // Start with all samples OOB
    inbag_counts.resize(num_samples, 0);

    // Draw num_samples samples with replacement (num_samples_inbag out of n) as inbag and mark as not OOB
    for (size_t s = 0; s < num_samples_inbag; ++s)
    {
      size_t draw = unif_dist(random_number_generator);
      sampleIDs.push_back(draw);
      ++inbag_counts[draw];
    }

    // Save OOB samples
    for (size_t s = 0; s < inbag_counts.size(); ++s)
    {
      if (inbag_counts[s] == 0)
      {
        oob_sampleIDs.push_back(s);
      }
    }
    num_samples_oob = oob_sampleIDs.size();

    if (!keep_inbag)
    {
      inbag_counts.clear();
      inbag_counts.shrink_to_fit();
    }
  }

  void Tree::bootstrapWeighted()
  {

    // Use fraction (default 63.21%) of the samples
    size_t num_samples_inbag = (size_t)num_samples * (*sample_fraction)[0];

    // Reserve space, reserve a little more to be save)
    sampleIDs.reserve(num_samples_inbag);
    oob_sampleIDs.reserve(num_samples * (exp(-(*sample_fraction)[0]) + 0.1));

    std::discrete_distribution<> weighted_dist(case_weights->begin(), case_weights->end());

    // Start with all samples OOB
    inbag_counts.resize(num_samples, 0);

    // Draw num_samples samples with replacement (n out of n) as inbag and mark as not OOB
    for (size_t s = 0; s < num_samples_inbag; ++s)
    {
      size_t draw = weighted_dist(random_number_generator);
      sampleIDs.push_back(draw);
      ++inbag_counts[draw];
    }

    // Save OOB samples. In holdout mode these are the cases with 0 weight.
    if (holdout)
    {
      for (size_t s = 0; s < (*case_weights).size(); ++s)
      {
        if ((*case_weights)[s] == 0)
        {
          oob_sampleIDs.push_back(s);
        }
      }
    }
    else
    {
      for (size_t s = 0; s < inbag_counts.size(); ++s)
      {
        if (inbag_counts[s] == 0)
        {
          oob_sampleIDs.push_back(s);
        }
      }
    }
    num_samples_oob = oob_sampleIDs.size();

    if (!keep_inbag)
    {
      inbag_counts.clear();
      inbag_counts.shrink_to_fit();
    }
  }

  void Tree::bootstrapWithoutReplacement()
  {

    // Use fraction (default 63.21%) of the samples
    size_t num_samples_inbag = (size_t)num_samples * (*sample_fraction)[0];
    shuffleAndSplit(sampleIDs, oob_sampleIDs, num_samples, num_samples_inbag, random_number_generator);
    num_samples_oob = oob_sampleIDs.size();

    if (keep_inbag)
    {
      // All observation are 0 or 1 times inbag
      inbag_counts.resize(num_samples, 1);
      for (size_t i = 0; i < oob_sampleIDs.size(); i++)
      {
        inbag_counts[oob_sampleIDs[i]] = 0;
      }
    }
  }

  void Tree::bootstrapWithoutReplacementWeighted()
  {

    // Use fraction (default 63.21%) of the samples
    size_t num_samples_inbag = (size_t)num_samples * (*sample_fraction)[0];
    drawWithoutReplacementWeighted(sampleIDs, random_number_generator, num_samples - 1, num_samples_inbag, *case_weights);

    // All observation are 0 or 1 times inbag
    inbag_counts.resize(num_samples, 0);
    for (auto &sampleID : sampleIDs)
    {
      inbag_counts[sampleID] = 1;
    }

    // Save OOB samples. In holdout mode these are the cases with 0 weight.
    if (holdout)
    {
      for (size_t s = 0; s < (*case_weights).size(); ++s)
      {
        if ((*case_weights)[s] == 0)
        {
          oob_sampleIDs.push_back(s);
        }
      }
    }
    else
    {
      for (size_t s = 0; s < inbag_counts.size(); ++s)
      {
        if (inbag_counts[s] == 0)
        {
          oob_sampleIDs.push_back(s);
        }
      }
    }
    num_samples_oob = oob_sampleIDs.size();

    if (!keep_inbag)
    {
      inbag_counts.clear();
      inbag_counts.shrink_to_fit();
    }
  }

  void Tree::bootstrapClassWise()
  {
    // Empty on purpose (virtual function only implemented in classification and probability)
  }

  void Tree::bootstrapWithoutReplacementClassWise()
  {
    // Empty on purpose (virtual function only implemented in classification and probability)
  }

  void Tree::setManualInbag()
  {
    // Select observation as specified in manual_inbag vector
    sampleIDs.reserve(manual_inbag->size());
    inbag_counts.resize(num_samples, 0);
    for (size_t i = 0; i < manual_inbag->size(); ++i)
    {
      size_t inbag_count = (*manual_inbag)[i];
      if ((*manual_inbag)[i] > 0)
      {
        for (size_t j = 0; j < inbag_count; ++j)
        {
          sampleIDs.push_back(i);
        }
        inbag_counts[i] = inbag_count;
      }
      else
      {
        oob_sampleIDs.push_back(i);
      }
    }
    num_samples_oob = oob_sampleIDs.size();

    // Shuffle samples
    std::shuffle(sampleIDs.begin(), sampleIDs.end(), random_number_generator);

    if (!keep_inbag)
    {
      inbag_counts.clear();
      inbag_counts.shrink_to_fit();
    }
  }

} // namespace diversityForest
