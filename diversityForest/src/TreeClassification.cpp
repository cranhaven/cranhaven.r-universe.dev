/*-------------------------------------------------------------------------------
 This file is part of diversityForest.
 
 Copyright (c) [2014-2018] [Marvin N. Wright]
 
 This software may be modified and distributed under the terms of the MIT license.
 
 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
#-------------------------------------------------------------------------------*/
 
#include <Rcpp.h>
 
#include <unordered_map>
#include <random>
#include <algorithm>
#include <iostream>
#include <iterator>
 
#include "TreeClassification.h"
#include "utility.h"
#include "Data.h"
 
#include "Hungarian.h"
 
 //#include <chrono>
 
 namespace diversityForest {
 
 TreeClassification::TreeClassification(std::vector<double>* class_values, std::vector<uint>* response_classIDs,
                                        std::vector<std::vector<size_t>>* sampleIDs_per_class, std::vector<double>* class_weights) :
 class_values(class_values), response_classIDs(response_classIDs), sampleIDs_per_class(sampleIDs_per_class), class_weights(
     class_weights), counter(0), counter_per_class(0) {
 }
 
 TreeClassification::TreeClassification(std::vector<std::vector<size_t>>& child_nodeIDs,
                                        std::vector<size_t>& split_varIDs, std::vector<double>& split_values, std::vector<size_t>& split_types, 
                                        std::vector<std::vector<size_t>>& split_multvarIDs, std::vector<std::vector<std::vector<bool>>>& split_directs, 
                                        std::vector<std::vector<std::vector<double>>>& split_multvalues, std::vector<std::vector<size_t>>& child_muwnodeIDs, std::vector<size_t>& split_muwvarIDs, std::vector<std::vector<double>>& split_muwvalues, std::vector<double>* class_values,
                                        std::vector<uint>* response_classIDs) :
 Tree(child_nodeIDs, split_varIDs, split_values, split_types, split_multvarIDs, split_directs, split_multvalues), child_muwnodeIDs(child_muwnodeIDs), split_muwvarIDs(split_muwvarIDs), split_muwvalues(split_muwvalues), class_values(class_values), response_classIDs(response_classIDs), sampleIDs_per_class(
     0), class_weights(0), counter { }, counter_per_class { } {
     }
 
 void TreeClassification::allocateMemory() {
   // Init counters if not in memory efficient mode
   if (!memory_saving_splitting) {
     size_t num_classes = class_values->size();
     size_t max_num_splits = data->getMaxNumUniqueValues();
     
     // Use number of random splits for extratrees
     if (splitrule == EXTRATREES && num_random_splits > max_num_splits) {
       max_num_splits = num_random_splits;
     }
     
     counter.resize(max_num_splits);
     counter_per_class.resize(num_classes * max_num_splits);
   }
 }
 
 double TreeClassification::estimate(size_t nodeID) {
   
   // Count classes over samples in node and return class with maximum count
   std::vector<double> class_count = std::vector<double>(class_values->size(), 0.0);
   
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     size_t value = (*response_classIDs)[sampleID];
     class_count[value] += (*class_weights)[value];
   }
   
   if (end_pos[nodeID] > start_pos[nodeID]) {
     size_t result_classID = mostFrequentClass(class_count, random_number_generator);
     return ((*class_values)[result_classID]);
   } else {
     throw std::runtime_error("Error: Empty node.");
   }
   
 }
 
 void TreeClassification::appendToFileInternal(std::ofstream& file) { // #nocov start
   // Empty on purpose
 } // #nocov end
 
 void TreeClassification::grow(std::vector<double> *variable_importance)
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
       
       if (divfortype == 1 || divfortype == 2 || divfortype == 3)
       {			
         ++num_open_nodes;
       }
       if (i >= last_left_nodeID)
       {
         // If new level, increase depth
         // (left_node saves left-most node in current level, new level reached if that node is splitted)
         if (divfortype == 1 || divfortype == 3)
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
 
 bool TreeClassification::splitNode(size_t nodeID)
 {
   
   bool stop;
   
   if (divfortype == 1)
   {
     
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
     
     bool inrectangle;
     size_t pos = start_pos[nodeID];
     while (pos < start_pos[right_child_nodeID])
     {
       size_t sampleID = sampleIDs[pos];
       
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
   
   if (divfortype == 3)
   {
     
     // Perform the splitting using the subclass method:
     std::vector<size_t> varIDs_rel;
     stop = checkWhetherFinal(nodeID, varIDs_rel);
     
     if (stop)
     {
       // Terminal node
       return true;
     }
     
     // Draw the variables considered for splitting.
     // Draw nsplits variables without replacement from the set of covariates varIDs_rel
     // if varIDs_rel.size() > nsplits, otherwise draw all covariates from varIDs_rel:
     std::vector<size_t> varIDs_sel = varIDs_rel;
     if (varIDs_sel.size() > nsplits)
     {
       std::shuffle(varIDs_sel.begin(), varIDs_sel.end(), random_number_generator);
       varIDs_sel.resize(nsplits);
     }
     
     // Perform the split selection and the splitting:
     splitNodeMuwUnivInternal(nodeID, varIDs_sel);
     
     if (importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_BOTH) {
       // Draw the multi-way partitions and select the optimal one:
       std::vector<std::vector<double>> split_muwvalues_temp;
       std::vector<size_t> varIDs_temp;
       drawPartitionsMuw(nodeID, split_muwvalues_temp, varIDs_temp, varIDs_sel);
       partitionNodeInternal(nodeID, split_muwvalues_temp, varIDs_temp);
       size_t split_muwvarID = split_muwvarIDs[nodeID];		
       split_muwvarIDs[nodeID] = data->getUnpermutedVarID(split_muwvarID);
     }
     
     size_t split_varID = split_varIDs[nodeID];
     
     double split_value = split_values[nodeID];
     
     // Save non-permuted variable for prediction
     split_varIDs[nodeID] = data->getUnpermutedVarID(split_varID);
     
     // Create child nodes
     size_t left_child_nodeID = split_varIDs.size();
     child_nodeIDs[0][nodeID] = left_child_nodeID;
     createEmptyNodeInternal();
     start_pos[left_child_nodeID] = start_pos[nodeID];
     
     size_t right_child_nodeID = split_varIDs.size();
     child_nodeIDs[1][nodeID] = right_child_nodeID;
     createEmptyNodeInternal();
     start_pos[right_child_nodeID] = end_pos[nodeID];
     
     // For each sample in node, assign to left or right child
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
     
     // End position of left child is start position of right child
     end_pos[left_child_nodeID] = start_pos[right_child_nodeID];
     end_pos[right_child_nodeID] = end_pos[nodeID];
     
     // No terminal node
     return false;
     
   }
   
   // To satisfy the compiler:
   return false;
 }
 
 bool TreeClassification::splitNodeInternal(size_t nodeID, std::vector<size_t>& possible_split_varIDs) {
   
   // Stop if maximum node size or depth reached
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   if (num_samples_node <= min_node_size || (nodeID >= last_left_nodeID && max_depth > 0 && depth >= max_depth)) {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   // Check if node is pure and set split_value to estimate and stop if pure
   bool pure = true;
   double pure_value = 0;
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     double value = data->get(sampleID, dependent_varID);
     if (pos != start_pos[nodeID] && value != pure_value) {
       pure = false;
       break;
     }
     pure_value = value;
   }
   if (pure) {
     split_values[nodeID] = pure_value;
     return true;
   }
   
   // Find best split, stop if no decrease of impurity
   bool stop;
   if (splitrule == EXTRATREES) {
     stop = findBestSplitExtraTrees(nodeID, possible_split_varIDs);
   } else {
     stop = findBestSplit(nodeID, possible_split_varIDs);
   }
   
   if (stop) {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   return false;
 }
 
 // Diversity Forests: Split node using univariable, binary splitting:
 bool TreeClassification::splitNodeUnivariateInternal(size_t nodeID, std::vector<std::pair<size_t, double>> sampled_varIDs_values) {
   
   // Stop if maximum node size or depth reached
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   if (num_samples_node <= min_node_size || (nodeID >= last_left_nodeID && max_depth > 0 && depth >= max_depth)) {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   // Check if node is pure and set split_value to estimate and stop if pure
   bool pure = true;
   double pure_value = 0;
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     double value = data->get(sampleID, dependent_varID);
     if (pos != start_pos[nodeID] && value != pure_value) {
       pure = false;
       break;
     }
     pure_value = value;
   }
   if (pure) {
     split_values[nodeID] = pure_value;
     return true;
   }
   
   // Find best split, stop if no decrease of impurity
   bool stop = findBestSplitUnivariate(nodeID, sampled_varIDs_values);
   
   if (stop) {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   return false;
 }
 
 // Class-focused/discriminatory VIM: Check whether the current not is final.
 bool TreeClassification::checkWhetherFinal(size_t nodeID, std::vector<size_t>& varIDs_rel) {
   
   // Stop if maximum node size or depth reached
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   if (num_samples_node <= min_node_size || (nodeID >= last_left_nodeID && max_depth > 0 && depth >= max_depth)) {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   // Check if node is pure and set split_value to estimate and stop if pure
   bool pure = true;
   double pure_value = 0;
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     double value = data->get(sampleID, dependent_varID);
     if (pos != start_pos[nodeID] && value != pure_value) {
       pure = false;
       break;
     }
     pure_value = value;
   }
   if (pure) {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   // Determine the set of covariates varIDs_rel that have more than one unique value in the current node "nodeID":
   size_t varIDtemp = 0;
   
   for (size_t i = 0; i < data->getNumCols() - data->getNoSplitVariables().size(); ++i) {
     varIDtemp = i;
     for (auto &skip_value : data->getNoSplitVariables())
     {
       if (varIDtemp >= skip_value)
       {
         ++varIDtemp;
       }
     }
     
     // Determine the values of the current covariate from all observations in the current node "nodeID":
     std::vector<double> all_values_nodeID;
     all_values_nodeID.reserve(end_pos[nodeID] - start_pos[nodeID]);
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
       size_t sampleID = sampleIDs[pos];
       double value = data->get(sampleID, varIDtemp);
       all_values_nodeID.push_back(value);
     }
     
     // Determine the corresponding unique values (no sorting needed):
     std::vector<double> all_unique_values_nodeID;
     all_unique_values_nodeID.reserve(data->getNumUniqueDataValues(varIDtemp));
     std::copy(all_values_nodeID.begin(), all_values_nodeID.end(), std::back_inserter(all_unique_values_nodeID));
     auto last = std::unique(all_unique_values_nodeID.begin(), all_unique_values_nodeID.end());
     all_unique_values_nodeID.erase(last, all_unique_values_nodeID.end());
     
     if (all_unique_values_nodeID.size() > 1) {
       varIDs_rel.push_back(varIDtemp);
     }
   }
   
   // Stop if no suitable split was found:
   if (varIDs_rel.size() == 0)
   {
     split_values[nodeID] = estimate(nodeID);
     return true;
   }
   
   return false;
   
 }
 
 // Interaction Forests: Split node:
 bool TreeClassification::splitNodeMultivariateInternal(size_t nodeID, std::vector<size_t> sampled_split_types, std::vector<std::vector<size_t>> sampled_split_multvarIDs, std::vector<std::vector<std::vector<bool>>> sampled_split_directs, std::vector<std::vector<std::vector<double>>> sampled_split_multvalues) {
   
   // Stop, if no suitable split was found:
   if (sampled_split_types.size() == 0)
   {
     split_multvalues[nodeID].resize(1);
     split_multvalues[nodeID][0].resize(1);
     split_multvalues[nodeID][0][0] = estimate(nodeID);
     return true;
   }
   
   // Stop if maximum node size or depth reached
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   if (num_samples_node <= min_node_size || (nodeID >= last_left_nodeID && max_depth > 0 && depth >= max_depth)) {
     split_multvalues[nodeID].resize(1);
     split_multvalues[nodeID][0].resize(1);
     split_multvalues[nodeID][0][0] = estimate(nodeID);
     return true;
   }
   
   // Check if node is pure and set split_value to estimate and stop if pure
   bool pure = true;
   double pure_value = 0;
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     double value = data->get(sampleID, dependent_varID);
     if (pos != start_pos[nodeID] && value != pure_value) {
       pure = false;
       break;
     }
     pure_value = value;
   }
   if (pure) {
     split_multvalues[nodeID].resize(1);
     split_multvalues[nodeID][0].resize(1);
     split_multvalues[nodeID][0][0] = pure_value;
     return true;
   }
   
   // Find best split, stop if no decrease of impurity
   bool stop = findBestSplitMultivariate(nodeID, sampled_split_types, sampled_split_multvarIDs, sampled_split_directs, sampled_split_multvalues);
   
   if (stop) {
     split_multvalues[nodeID].resize(1);
     split_multvalues[nodeID][0].resize(1);
     split_multvalues[nodeID][0][0] = estimate(nodeID);
     return true;
   }
   
   return false;
 }
 
 // Class-focused/discriminatory VIM: Draw the multi-way partitions.
 void TreeClassification::drawPartitionsMuw(size_t nodeID, std::vector<std::vector<double>> &split_muwvalues_temp, std::vector<size_t> &varIDs_temp, std::vector<size_t> varIDs_sel)
 {
   
   // Identify the set of the C_l classes featured in the current node "nodeID":
   std::vector<size_t> class_vector;
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
   {
     size_t sampleID = sampleIDs[pos];
     size_t value = (*response_classIDs)[sampleID];
     if (std::find(class_vector.begin(), class_vector.end(), value) == class_vector.end())
     {
       class_vector.push_back(value);
     }
   }
   
   // Determine the number of classes in the current node "nodeID":
   size_t n_classes = class_vector.size();
   
   // Make a counter for the number of splits:
   size_t num_splits = 0;
   
   // Draw the splits by a loop:
   for (size_t i = 0; i < varIDs_sel.size(); ++i)
   {
     
     size_t varID = varIDs_sel[i];
     
     // This next step will produce the M-1 split points (split_muwvalues_temp[i]).
     
     // Denote with all_values_nodeID the values of the current covariate from all observations in the current node "nodeID" and with all_unique_values_nodeID the corresponding sorted unique values. Additionally, let class_vec_nodeID denote the classes corresponding to each of the observations in "nodeID".
     
     // Determine the values of the current covariate from all observations in the current node "nodeID":
     std::vector<double> all_values_nodeID;
     all_values_nodeID.reserve(end_pos[nodeID] - start_pos[nodeID]);
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
     {
       size_t sampleID = sampleIDs[pos];
       double value = data->get(sampleID, varID);
       all_values_nodeID.push_back(value);
     }
     
     // Determine the corresponding sorted unique values:
     std::vector<double> all_unique_values_nodeID;
     all_unique_values_nodeID.reserve(data->getNumUniqueDataValues(varID));
     std::copy(all_values_nodeID.begin(), all_values_nodeID.end(), std::back_inserter(all_unique_values_nodeID));
     std::sort(all_unique_values_nodeID.begin(), all_unique_values_nodeID.end());
     auto last = std::unique(all_unique_values_nodeID.begin(), all_unique_values_nodeID.end());
     all_unique_values_nodeID.erase(last, all_unique_values_nodeID.end());
     
     // If the number of unique values is smaller or equal to the number of classes,
     // a split point between each unique value is used:
     if (all_unique_values_nodeID.size() <= n_classes)
     {
       
       // Set the split points to the mid points between the neighboring unique values:
       split_muwvalues_temp.emplace_back();
       for (size_t j = 0; j < all_unique_values_nodeID.size() - 1; ++j)
       {
         split_muwvalues_temp[num_splits].push_back((all_unique_values_nodeID[j] + all_unique_values_nodeID[j + 1]) / 2);
       }
       varIDs_temp.push_back(varID);
       ++num_splits;
       
     }
     else
     {
       
       // Draw the split points randomly:
       
       /*
        size_t min_num_obs = floor(all_unique_values_nodeID.size() / (n_classes * 2));
        
        // If min_num_obs is smaller than 1, set it to 1:
        if (min_num_obs < 1)
        {
        min_num_obs = 1;
        }
        */
       size_t min_num_obs = 1;
       
       
       // Make a vector "min_starts" with elements min_num_obs-1, 2*min_num_obs-1, ..., (n_classes-1)*min_num_obs-1:
       std::vector<size_t> min_starts;
       min_starts.reserve(n_classes - 1);
       for (size_t j = 1; j < n_classes; ++j)
       {
         min_starts.push_back(j * min_num_obs - 1);
       }
       
       // For k = 0 to num_random_splits-1:
       for (size_t k = 0; k < num_random_splits; ++k)
       {
         
         // Make a vector "temp_indices" of length min_starts.size(), where each element is a random integer between 0 and
         // all_unique_values_nodeID.size()-(n_classes-1)*min_num_obs-1:
         std::vector<size_t> temp_indices;
         temp_indices.reserve(min_starts.size());
         //std::uniform_int_distribution<size_t> unif_dist(0, all_unique_values_nodeID.size() - ((n_classes - 1) * min_num_obs) - 1);
         std::uniform_int_distribution<size_t> unif_dist(0, all_unique_values_nodeID.size() - n_classes * min_num_obs);
         for (size_t j = 0; j < min_starts.size(); ++j)
         {
           temp_indices.push_back(unif_dist(random_number_generator));
         }
         // Sort temp_indices:
         std::sort(temp_indices.begin(), temp_indices.end());
         
         // Add the elements of temp_indices to the elements of min_starts:
         std::vector<size_t> temp_indices2;
         temp_indices2.reserve(temp_indices.size());
         for (size_t j = 0; j < temp_indices.size(); ++j)
         {
           temp_indices2.push_back(min_starts[j] + temp_indices[j]);
         }
         
         // Set the split points to (all_unique_values_nodeID[temp_indices2[j]] + all_unique_values_nodeID[temp_indices2[j+1]])/2
         // for j in 0, ..., temp_indices2.size()-1:
         split_muwvalues_temp.emplace_back();
         for (size_t j = 0; j < temp_indices2.size(); ++j)
         {
           split_muwvalues_temp[num_splits].push_back((all_unique_values_nodeID[temp_indices2[j]] + all_unique_values_nodeID[temp_indices2[j] + 1]) / 2);
         }
         varIDs_temp.push_back(varID);
         ++num_splits;
       }
     }
   }
 }
 
 // Class-focused/discriminatory VIM: Determine the best split for multi-ways partitions:
 void TreeClassification::partitionNodeInternal(size_t nodeID, std::vector<std::vector<double>> split_muwvalues_temp, std::vector<size_t> varIDs_temp)
 {
   
   // Compute the number of samples in the current node "nodeID":
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   
   // Identify the set of the C_l classes featured in the current node "nodeID":
   std::vector<size_t> class_vector;
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
   {
     size_t sampleID = sampleIDs[pos];
     size_t value = (*response_classIDs)[sampleID];
     if (std::find(class_vector.begin(), class_vector.end(), value) == class_vector.end())
     {
       class_vector.push_back(value);
     }
   }
   
   // Determine the number of classes in the current node "nodeID":
   size_t n_classes = class_vector.size();
   
   
   // Make a loop to calculate the split criterion values for the sampled partitions "split_muwvalues_temp":
   
   size_t n_triedsplits = split_muwvalues_temp.size();
   
   // Initialize the vectors of the assigned classes (each vector will be a vector of length equal
   // to the number of classes, where the j-th element will contain the index of the child node to
   // which class j is assigned):
   std::vector<std::vector<size_t>> assigned_class_vectors(n_triedsplits);
   
   // Initialize a vector that will contain the values of the split criterion
   // and reserve memory for it:
   std::vector<double> split_criterion_values(n_triedsplits);
   
   // Draw the covariate/split pairs by a loop:
   for (size_t i = 0; i < n_triedsplits; ++i)
   {
     
     // Consider the i-th candidate covariate:
     size_t varID = varIDs_temp[i];
     
     // Determine the values of the current covariate from all observations in the current node "nodeID":
     std::vector<double> all_values_nodeID;
     all_values_nodeID.reserve(end_pos[nodeID] - start_pos[nodeID]);
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
     {
       size_t sampleID = sampleIDs[pos];
       double value = data->get(sampleID, varID);
       all_values_nodeID.push_back(value);
     }
     
     // Determine the corresponding sorted unique values:
     std::vector<double> all_unique_values_nodeID;
     all_unique_values_nodeID.reserve(data->getNumUniqueDataValues(varID));
     std::copy(all_values_nodeID.begin(), all_values_nodeID.end(), std::back_inserter(all_unique_values_nodeID));
     std::sort(all_unique_values_nodeID.begin(), all_unique_values_nodeID.end());
     auto last = std::unique(all_unique_values_nodeID.begin(), all_unique_values_nodeID.end());
     all_unique_values_nodeID.erase(last, all_unique_values_nodeID.end());
     
     // Initialize a vector "assigned_class_vector" that will contain the indices of the child nodes
     // to which the classes are assigned:
     std::vector<size_t> assigned_class_vector;
     assigned_class_vector.reserve(n_classes);
     
     // Initialize the object for the hungarian algortihm:
     HungarianAlgorithm HungAlgo;
     
     // Initialize the split criterion value:
     double split_criterion_value = 0;
     
     // If the number of unique values is smaller or equal to the number of classes,
     // each class is assigned to the child node that features the largest squared proportion
     // of the class:
     if (all_unique_values_nodeID.size() < n_classes)
     {
       
       // Loop through the class vector class_vector and in each case assign to assigned_class_vector[j] the index
       // of the child node for which class_vector[j] has the largest squared proportion of observations across all child nodes
       // of the current node "nodeID" (ties are broken randomly):
       
       // Calculate the numbers of observations in each child node of the current node "nodeID"
       // (Note: The child nodes are defined by the split points in all_unique_values_nodeID as follows:
       // Observations with values smaller than or equal to sampled_split_muwvalues[i][0] are assigned to the first child node,
       // observations with values larger than sampled_split_muwvalues[i][0] and smaller than or equal to sampled_split_muwvalues[i][1]
       // are assigned to the second child node, and so on):
       std::vector<size_t> child_node_sizes(split_muwvalues_temp[i].size() + 1, 0);
       for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
       {
         size_t sampleID = sampleIDs[pos];
         double value = data->get(sampleID, varID);
         for (size_t k = 0; k < split_muwvalues_temp[i].size() + 1; ++k)
         {
           if (k == 0 && value <= split_muwvalues_temp[i][k])
           {
             ++child_node_sizes[k];
             break;
           }
           else if (k == split_muwvalues_temp[i].size() && value > split_muwvalues_temp[i][k - 1])
           {
             ++child_node_sizes[k];
             break;
           }
           else if (value > split_muwvalues_temp[i][k - 1] && value <= split_muwvalues_temp[i][k])
           {
             ++child_node_sizes[k];
             break;
           }
         }
       }
       
       // Loop through the class vector class_vector:
       for (size_t j = 0; j < n_classes; ++j)
       {
         // Count the number of observations of class_vector[j] in each child node of the current node "nodeID":
         std::vector<size_t> class_counts(split_muwvalues_temp[i].size() + 1, 0);
         for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
         {
           size_t sampleID = sampleIDs[pos];
           double value = data->get(sampleID, varID);
           size_t value_classID = (*response_classIDs)[sampleID];
           if (value_classID == class_vector[j])
           {
             for (size_t k = 0; k < split_muwvalues_temp[i].size() + 1; ++k)
             {
               if (k == 0 && value <= split_muwvalues_temp[i][k])
               {
                 ++class_counts[k];
                 break;
               }
               else if (k == split_muwvalues_temp[i].size() && value > split_muwvalues_temp[i][k - 1])
               {
                 ++class_counts[k];
                 break;
               }
               else if (value > split_muwvalues_temp[i][k - 1] && value <= split_muwvalues_temp[i][k])
               {
                 ++class_counts[k];
                 break;
               }
             }
           }
         }
         
         // Calculate the squared proportions of observations of class_vector[j] in each child node of the current node "nodeID":
         std::vector<double> class_proportions(split_muwvalues_temp[i].size() + 1, 0);
         for (size_t k = 0; k < split_muwvalues_temp[i].size() + 1; ++k)
         {
           class_proportions[k] = ((double)class_counts[k] / (double)child_node_sizes[k]) * ((double)class_counts[k] / (double)child_node_sizes[k]);
         }
         
         // Assign the index of the child node for which class_vector[j] has the largest squared proportion of observations across all child nodes.
         // If there are ties, break them randomly:
         
         // Find the maximum value in class_proportions:
         double max_value = *std::max_element(class_proportions.begin(), class_proportions.end());
         
         // Find the indices of the maximum value in class_proportions:
         std::vector<size_t> max_indices;
         for (size_t k = 0; k < class_proportions.size(); ++k)
         {
           if (class_proportions[k] == max_value)
           {
             max_indices.push_back(k);
           }
         }
         
         // Draw randomly one of the indices in max_indices:
         std::random_device rd;
         std::mt19937 gen(rd());
         std::uniform_int_distribution<> distrib(0, max_indices.size() - 1);
         
         size_t random_index = max_indices[distrib(gen)];
         
         // Assign the index of the child node for which class_vector[j] has the largest squared proportion of observations across all child nodes:
         assigned_class_vector.push_back(random_index);
         
         // Weight class_proportions[random_index] by child_node_sizes[random_index] and divide by the size of the current node "nodeID":
         class_proportions[random_index] = class_proportions[random_index] * (double)child_node_sizes[random_index] / (double)num_samples_node;
         
         // Add the squared proportion of the observations of class_vector[j] in the child node it was assigned to to the split criterion value:
         split_criterion_value += class_proportions[random_index];
         
       }
     }
     else
     {
       
       // If the number of observations in the current node is larger than the number of classes,
       // assign the classes to the child nodes in such a way that the sum of the squared proportions
       // of the observations from the classes in their assigned child nodes is maximized.
       
       // Calculate the numbers of observations in each child node of the current node "nodeID"
       // (Note: The child nodes are defined by the split points in all_unique_values_nodeID as follows:
       // Observations with values smaller than or equal to split_muwvalues_temp[i][0] are assigned to the first child node,
       // observations with values larger than split_muwvalues_temp[i][0] and smaller than or equal to split_muwvalues_temp[i][1]
       // are assigned to the second child node, and so on):
       std::vector<size_t> child_node_sizes(split_muwvalues_temp[i].size() + 1, 0);
       for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
       {
         size_t sampleID = sampleIDs[pos];
         double value = data->get(sampleID, varID);
         for (size_t k = 0; k < split_muwvalues_temp[i].size() + 1; ++k)
         {
           if (k == 0 && value <= split_muwvalues_temp[i][k])
           {
             ++child_node_sizes[k];
             break;
           }
           else if (k == split_muwvalues_temp[i].size() && value > split_muwvalues_temp[i][k - 1])
           {
             ++child_node_sizes[k];
             break;
           }
           else if (value > split_muwvalues_temp[i][k - 1] && value <= split_muwvalues_temp[i][k])
           {
             ++child_node_sizes[k];
             break;
           }
         }
       }
       
       
       // Calculate the squared proportions of observations of each class in each child node of the current node "nodeID":
       
       // Initialize, with zeros, the vector class_counts and class_proportions that will contain the numbers and squared proportions, respectively, of observations of each class
       // in each child node of the current node "nodeID":
       std::vector<std::vector<size_t>> class_counts(n_classes, std::vector<size_t>(split_muwvalues_temp[i].size() + 1, 0));
       std::vector<std::vector<double>> class_proportions(n_classes, std::vector<double>(split_muwvalues_temp[i].size() + 1, 0));
       for (size_t j = 0; j < n_classes; ++j)
       {
         
         // Count the number of observations of class_vector[j] in each child node of the current node "nodeID"
         // and save the counts in class_counts[j]:
         for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos)
         {
           size_t sampleID = sampleIDs[pos];
           double value = data->get(sampleID, varID);
           size_t value_classID = (*response_classIDs)[sampleID];
           if (value_classID == class_vector[j])
           {
             for (size_t k = 0; k < split_muwvalues_temp[i].size() + 1; ++k)
             {
               if (k == 0 && value <= split_muwvalues_temp[i][k])
               {
                 ++class_counts[j][k];
                 break;
               }
               else if (k == split_muwvalues_temp[i].size() && value > split_muwvalues_temp[i][k - 1])
               {
                 ++class_counts[j][k];
                 break;
               }
               else if (value > split_muwvalues_temp[i][k - 1] && value <= split_muwvalues_temp[i][k])
               {
                 ++class_counts[j][k];
                 break;
               }
             }
           }
         }
         
         // Calculate the proportions of observations of class_vector[j] in each child node of the current node "nodeID":
         for (size_t k = 0; k < split_muwvalues_temp[i].size() + 1; ++k)
         {
           class_proportions[j][k] = ((double)class_counts[j][k] / (double)child_node_sizes[k]) * ((double)class_counts[j][k] / (double)child_node_sizes[k]);
         }
         
       }
       
       
       // Use the Hungarian algorithm to assign the classes to the child nodes in a way that maximizes the sum of the 
       // squared proportions of the observations from each class in the child node to which it is assigned:
       
       vector<int> assignment;
       double split_criterion_unweighted = HungAlgo.Solve(class_proportions, assignment);
       
       // Assign the indices of the child nodes to assigned_class_vector:
       for (size_t j = 0; j < n_classes; ++j)
       {
         assigned_class_vector.push_back(static_cast<size_t>(assignment[j]));
       }
       
       // Calculate the split criterion value as the sum of the proportions class_proportions[j][assigned_class_vector[j]], where
       // the latter are weighted with child_node_sizes[assigned_class_vector[j]] and divided by the size of the current node "nodeID":
       for (size_t j = 0; j < n_classes; ++j)
       {
         split_criterion_value += class_proportions[j][assigned_class_vector[j]] * (double)child_node_sizes[assigned_class_vector[j]] / (double)num_samples_node;
       }
       
     }
     
     // Add the split criterion value to "split_criterion_values":
     split_criterion_values[i] = split_criterion_value;
     
     // Add the vector assigned_class_vector to "assigned_class_vectors":
     assigned_class_vectors[i] = assigned_class_vector;
   }
   
   // Find the index of the split with the largest split criterion value:
   std::vector<double>::iterator it = std::max_element(split_criterion_values.begin(), split_criterion_values.end());
   size_t index = std::distance(split_criterion_values.begin(), it);
   
   // Set the split value set to the split value set with the largest split criterion value:
   split_muwvalues[nodeID] = split_muwvalues_temp[index];
   
   // Set assigned_classes[nodeID] to the assigned class vector with the largest split criterion value:
   assigned_classes[nodeID] = assigned_class_vectors[index];
   
   // Set classes_at_nodes[nodeID] to the set of classes that are present in the current node "nodeID":
   classes_at_nodes[nodeID] = class_vector;
   
   // Set the split variable to the split variable with the largest split criterion value:
   split_muwvarIDs[nodeID] = varIDs_temp[index];
 }
 
 // Class-focused/discriminatory VIM: Determine the best split for binary splits:
 void TreeClassification::splitNodeMuwUnivInternal(size_t nodeID, std::vector<size_t> varIDs_sel)
 {
   
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   size_t num_classes = class_values->size();
   double best_decrease = -1;
   size_t best_varID = 0;
   double best_value = 0;
   
   std::vector<size_t> class_counts(num_classes);
   // Compute overall class counts
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     uint sample_classID = (*response_classIDs)[sampleID];
     ++class_counts[sample_classID];
   }
   
   // For all possible split variables
   for (auto& varID : varIDs_sel) {
     // Find best split value:
     // Use memory saving method if option set
     if (memory_saving_splitting) {
       findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                best_decrease);
     } else {
       // Use faster method for both cases
       double q = (double) num_samples_node / (double) data->getNumUniqueDataValues(varID);
       if (q < Q_THRESHOLD) {
         findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                  best_decrease);
       } else {
         findBestSplitValueLargeQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                  best_decrease);
       }
     }
   }
   
   // Save best values
   split_varIDs[nodeID] = best_varID;
   split_values[nodeID] = best_value;
   
 }
 
 void TreeClassification::createEmptyNodeInternal() {
   if (divfortype == 3)
   {
     split_varIDs.push_back(0);
     split_muwvarIDs.push_back(0);
     split_values.push_back(0);
     std::vector<double> emptyMuwValues;
     split_muwvalues.push_back(emptyMuwValues);
     child_nodeIDs[0].push_back(0);
     child_nodeIDs[1].push_back(0);
     std::vector<size_t> emptyChildNodeIDs;
     child_muwnodeIDs.push_back(emptyChildNodeIDs); // EVTL. WEG
     std::vector<size_t> emptyAssignedClasses;
     assigned_classes.push_back(emptyAssignedClasses);
     std::vector<size_t> emptyClassesAtNodes;
     classes_at_nodes.push_back(emptyClassesAtNodes);
     start_pos.push_back(0);
     end_pos.push_back(0);	
   }
 }
 
 // Class-focused/discriminatory VIM: The function predictMuw computes the terminal node IDs for the (OOB) samples:
 void TreeClassification::predictMuw(const Data *prediction_data, bool oob_prediction)
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
       
       // Determine the value of the split variable for the current node:
       double value = prediction_data->get(sample_idx, split_varID);
       
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
     
     prediction_terminal_nodeIDs[i] = nodeID;
     
     // Print the node ID of the terminal node:
     ////std::cout << "Sample " << i << " is in terminal node " << prediction_terminal_nodeIDs[i] << std::endl;
     
   }
   
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceMuw computes the importance for the current tree:
 void TreeClassification::computeImportanceMuw(std::vector<double> &forest_classfoc, std::vector<double> &forest_discr)
 {
   
   size_t num_independent_variables = data->getNumCols() - data->getNoSplitVariables().size(); // EVTL. WEG
   
   // Drop the OOB observations down the tree and for each node, determine the
   // OOB observations that pass through the node:
   std::unordered_set<size_t> visited_varIDs;
   std::vector<std::vector<size_t>> oob_sampleIDs_nodeID_classfoc(split_varIDs.size());
   std::vector<std::vector<size_t>> oob_sampleIDs_nodeID_discr(split_varIDs.size());
   for (size_t sampleID : oob_sampleIDs)
   {
     
     visited_varIDs.clear();
     size_t nodeID = 0;
     while (1)
     {
       
       // Break if terminal node
       if (child_nodeIDs[0][nodeID] == 0 && child_nodeIDs[1][nodeID] == 0)
       {
         break;
       }
       
       size_t split_varID = split_varIDs[nodeID];
       
       if (visited_varIDs.find(split_varID) == visited_varIDs.end())
       {
         oob_sampleIDs_nodeID_classfoc[nodeID].push_back(sampleID);
       }
       visited_varIDs.insert(split_varID);     
       
       oob_sampleIDs_nodeID_discr[nodeID].push_back(sampleID);
       
       // Move to child
       
       double value = data->get(sampleID, split_varID);
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
     
   }
   
   
   // Compute importance values for each variable:
   if (importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_BOTH)
   {
     
     // Make a vector 0, 1, 2, ..., split_varIDs.size()-1:
     std::vector<size_t> nodeIDs_vec_muw(split_varIDs.size());
     std::iota(nodeIDs_vec_muw.begin(), nodeIDs_vec_muw.end(), 0);
     
     // Remove empty elements from oob_sampleIDs_nodeID_classfoc and the corresponding elements from nodeIDs_vec_muw:
     std::vector<std::vector<size_t>> oob_sampleIDs_nodeID_classfoc_nonempty;
     std::vector<size_t> nodeIDs_vec_muw_nonempty;
     for (size_t i = 0; i < oob_sampleIDs_nodeID_classfoc.size(); ++i)
     {
       if (oob_sampleIDs_nodeID_classfoc[i].size() > 0)
       {
         oob_sampleIDs_nodeID_classfoc_nonempty.push_back(oob_sampleIDs_nodeID_classfoc[i]);
         nodeIDs_vec_muw_nonempty.push_back(nodeIDs_vec_muw[i]);
       }
     }
     nodeIDs_vec_muw = nodeIDs_vec_muw_nonempty;
     oob_sampleIDs_nodeID_classfoc = oob_sampleIDs_nodeID_classfoc_nonempty;
     
     // Loop through best_nodeIDs and compute the importance of the variables:
     for (size_t i = 0; i < nodeIDs_vec_muw.size(); ++i)
     {
       // Calculate the importance of the variable for the node using OOB observations:
       forest_classfoc[split_muwvarIDs[nodeIDs_vec_muw[i]]] += computeImportanceDifference(nodeIDs_vec_muw[i], oob_sampleIDs_nodeID_classfoc[i], 1);
     }
     
   }
   
   
   // Compute discriminatory importance values for each variable:
   if (importance_mode == MUWIMP_DISCR || importance_mode == MUWIMP_BOTH)
   {
     
     // Make a vector 0, 1, 2, ..., split_varIDs.size()-1:
     std::vector<size_t> nodeIDs_vec_discr(split_varIDs.size());
     std::iota(nodeIDs_vec_discr.begin(), nodeIDs_vec_discr.end(), 0);
     
     // Remove empty elements from oob_sampleIDs_nodeID_discr and the corresponding elements from nodeIDs_vec_discr:
     std::vector<std::vector<size_t>> oob_sampleIDs_nodeID_discr_nonempty;
     std::vector<size_t> nodeIDs_vec_discr_nonempty;
     for (size_t i = 0; i < oob_sampleIDs_nodeID_discr.size(); ++i)
     {
       if (oob_sampleIDs_nodeID_discr[i].size() > 0)
       {
         oob_sampleIDs_nodeID_discr_nonempty.push_back(oob_sampleIDs_nodeID_discr[i]);
         nodeIDs_vec_discr_nonempty.push_back(nodeIDs_vec_discr[i]);
       }
     }
     nodeIDs_vec_discr = nodeIDs_vec_discr_nonempty;
     oob_sampleIDs_nodeID_discr = oob_sampleIDs_nodeID_discr_nonempty;
     
     // Loop through best_nodeIDs and compute the importance of the variables:
     for (size_t i = 0; i < nodeIDs_vec_discr.size(); ++i)
     {
       
       // Calculate the importance of the variable for the node using OOB observations:
       forest_discr[split_varIDs[nodeIDs_vec_discr[i]]] += computeImportanceDifference(nodeIDs_vec_discr[i], oob_sampleIDs_nodeID_discr[i], 0);
     }
     
   }
   
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceDifference computes the importance difference for the current variable varID:
 double TreeClassification::computeImportanceDifference(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID, size_t classfoc_ind)
 {
   
   // Compute the importance for oob_sampleIDs_subset_subset and first_visited_nodeID_oob_subset:
   double importance_node = computeImportanceNode(nodeID, oob_sampleIDs_thisnodeID, classfoc_ind);
   
   // Permute the values of oob_sampleIDs_subset_subset and save the permuted values in oob_sampleIDs_thisnodeID_permuted:
   std::vector<size_t> oob_sampleIDs_thisnodeID_permuted = oob_sampleIDs_thisnodeID;
   std::shuffle(oob_sampleIDs_thisnodeID_permuted.begin(), oob_sampleIDs_thisnodeID_permuted.end(), random_number_generator);
   
   // Compute the importance after permuting the values of split_varIDs[nodeID] (the response variable is not permuted):
   double importance_node_permuted = computeImportanceNodePermuted(nodeID, oob_sampleIDs_thisnodeID, oob_sampleIDs_thisnodeID_permuted, classfoc_ind);
   
   // Add the importance difference to importance_difference, weighted by the size of the node:
   double importance_difference = (importance_node - importance_node_permuted) * (end_pos[nodeID] - start_pos[nodeID]);
   
   return importance_difference;
   
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceNode computes the importance for the current variable varID:
 double TreeClassification::computeImportanceNode(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID, size_t classfoc_ind)
 {
   
   double split_criterion_value;
   
   if (classfoc_ind == 1)
   {
     // Compute the importance for the current variable varID for multi-way partitions:
     split_criterion_value = computeImportanceNodeClassfoc(nodeID, oob_sampleIDs_thisnodeID);
   }
   
   if (classfoc_ind == 0)
   {
     // Compute the importance for the current variable varID for binary splits:
     split_criterion_value = computeImportanceNodeDiscr(nodeID, oob_sampleIDs_thisnodeID);
   }
   
   return split_criterion_value;
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceNodePermuted computes the importance for the current variable varID
 // for multi-way partitions:
 double TreeClassification::computeImportanceNodeClassfoc(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID)
 {
   
   // Compute the numbers of the observations in oob_sampleIDs_thisnodeID that are assigned to each
   // child node of the current node "nodeID" as well as the numbers of the observations from each class
   // that land in the child nodes to which they were assigned:
   std::vector<size_t> child_node_sizes(split_muwvalues[nodeID].size() + 1, 0);
   std::vector<size_t> class_counts(classes_at_nodes[nodeID].size(), 0);
   for (size_t i = 0; i < oob_sampleIDs_thisnodeID.size(); ++i)
   {
     size_t sampleID = oob_sampleIDs_thisnodeID[i];
     double value = data->get(sampleID, split_muwvarIDs[nodeID]);
     for (size_t j = 0; j < split_muwvalues[nodeID].size() + 1; ++j)
     {
       if (j == 0 && value <= split_muwvalues[nodeID][j])
       {
         ++child_node_sizes[j];
         size_t classID = (*response_classIDs)[sampleID];
         for (size_t k = 0; k < classes_at_nodes[nodeID].size(); ++k)
         {
           if (classID == classes_at_nodes[nodeID][k] && assigned_classes[nodeID][k] == j)
           {
             ++class_counts[k];
             break;
           }
         }
         break;
       }
       else if (j == split_muwvalues[nodeID].size() && value > split_muwvalues[nodeID][j - 1])
       {
         ++child_node_sizes[j];
         size_t classID = (*response_classIDs)[sampleID];
         for (size_t k = 0; k < classes_at_nodes[nodeID].size(); ++k)
         {
           if (classID == classes_at_nodes[nodeID][k] && assigned_classes[nodeID][k] == j)
           {
             ++class_counts[k];
             break;
           }
         }
         break;
       }
       else if (value > split_muwvalues[nodeID][j - 1] && value <= split_muwvalues[nodeID][j])
       {
         ++child_node_sizes[j];
         size_t classID = (*response_classIDs)[sampleID];
         for (size_t k = 0; k < classes_at_nodes[nodeID].size(); ++k)
         {
           if (classID == classes_at_nodes[nodeID][k] && assigned_classes[nodeID][k] == j)
           {
             ++class_counts[k];
             break;
           }
         }
         break;
       }
     }
   }
   
   // For i = 0, ..., classes_at_nodes[nodeID].size() - 1, compute the squared proportion of the observations from oob_sampleIDs_thisnodeID
   // in the child nodes that are from the classes assigned to them:
   std::vector<double> class_proportions;
   for (size_t i = 0; i < classes_at_nodes[nodeID].size(); ++i)
   {
     if (child_node_sizes[assigned_classes[nodeID][i]] == 0)
     {
       class_proportions.push_back(0);
     }
     else
     {
       class_proportions.push_back(((double)class_counts[i] / (double)child_node_sizes[assigned_classes[nodeID][i]]) * ((double)class_counts[i] / (double)child_node_sizes[assigned_classes[nodeID][i]]));
     }
   }
   
   // Weight the class proportions by the size of the child nodes and compute the OOB split criterion value:
   double split_criterion_value = 0;
   for (size_t i = 0; i < classes_at_nodes[nodeID].size(); ++i)
   {
     split_criterion_value += class_proportions[i] * (double)child_node_sizes[assigned_classes[nodeID][i]] / (double)oob_sampleIDs_thisnodeID.size();
   }
   
   return split_criterion_value;
   
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceNodeDiscr computes the importance for the current variable varID
 // for binary splits:
 double TreeClassification::computeImportanceNodeDiscr(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID)
 {
   
   // Compute the Gini impurity of the parent node:
   //////double gini_parent = computeGiniImpurity(oob_sampleIDs_thisnodeID);
   
   // Determine the OOB observations that are assigned to the left and right child nodes:
   std::vector<size_t> oob_sampleIDs_left_child;
   oob_sampleIDs_left_child.reserve(oob_sampleIDs_thisnodeID.size());
   std::vector<size_t> oob_sampleIDs_right_child;
   oob_sampleIDs_right_child.reserve(oob_sampleIDs_thisnodeID.size());
   
   for (size_t i = 0; i < oob_sampleIDs_thisnodeID.size(); ++i)
   {
     size_t sampleID = oob_sampleIDs_thisnodeID[i];
     double value = data->get(sampleID, split_varIDs[nodeID]);
     if (value <= split_values[nodeID])
     {
       oob_sampleIDs_left_child.push_back(sampleID);
     }
     else
     {
       oob_sampleIDs_right_child.push_back(sampleID);
     }
   }
   
   // If oob_sampleIDs_left_child is empty, set the Gini impurity of the left child node to 0:
   double gini_left = 0;
   if (oob_sampleIDs_left_child.size() > 0)
   {
     gini_left = computeGiniImpurity(oob_sampleIDs_left_child);
   }
   
   // Compute the Gini impurity of the right child node:
   double gini_right = 0;
   if (oob_sampleIDs_right_child.size() > 0)
   {
     gini_right = computeGiniImpurity(oob_sampleIDs_right_child);
   }
   
   // Compute the Gini impurity reduction:
   double gini_reduction = - ((double)oob_sampleIDs_left_child.size() / (double)oob_sampleIDs_thisnodeID.size()) * gini_left - ((double)oob_sampleIDs_right_child.size() / (double)oob_sampleIDs_thisnodeID.size()) * gini_right;
   
   return gini_reduction;
   
 }
 
 // Class-focused/discriminatory VIM:
 double TreeClassification::computeGiniImpurity(std::vector<size_t> oob_sampleIDs_thisnodeID)
 {
   // Compute the number of samples in the current node:
   size_t num_samples_node = oob_sampleIDs_thisnodeID.size();
   
   // Compute the number of classes:
   size_t num_classes = class_values->size();
   
   // Compute the class counts:
   std::vector<size_t> class_counts(num_classes, 0);
   for (size_t i = 0; i < num_samples_node; ++i)
   {
     size_t sampleID = oob_sampleIDs_thisnodeID[i];
     uint sample_classID = (*response_classIDs)[sampleID];
     ++class_counts[sample_classID];
   }
   
   // Compute the Gini impurity:
   double gini = 1.0;
   for (size_t i = 0; i < num_classes; ++i)
   {
     double proportion = (double)class_counts[i] / (double)num_samples_node;
     gini -= (*class_weights)[i] * proportion * proportion;
   }
   
   return gini;
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceNodePermuted computes the importance for the current variable varID after permuting it (the response variable is not permuted):
 double TreeClassification::computeImportanceNodePermuted(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID, std::vector<size_t> oob_sampleIDs_thisnodeID_permuted, size_t classfoc_ind)
 {
   
   double split_criterion_value;
   
   if (classfoc_ind == 1)
   {
     // Compute the importance for the current variable varID after permuting it for multi-way partitions:
     split_criterion_value = computeImportanceNodePermutedClassfoc(nodeID, oob_sampleIDs_thisnodeID, oob_sampleIDs_thisnodeID_permuted);
   }
   
   if (classfoc_ind == 0)
   {
     // Compute the importance for the current variable varID after permuting it for binary splits:
     split_criterion_value = computeImportanceNodePermutedDiscr(nodeID, oob_sampleIDs_thisnodeID, oob_sampleIDs_thisnodeID_permuted);
   }
   
   return split_criterion_value;
   
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceNodePermutedMuw computes the importance for the current variable varID after permuting it for multi-way partitions:
 double TreeClassification::computeImportanceNodePermutedClassfoc(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID, std::vector<size_t> oob_sampleIDs_thisnodeID_permuted)
 {
   
   // Compute the numbers of the observations in oob_sampleIDs_thisnodeID_permuted that are assigned to each
   // child node of the current node "nodeID" as well as the numbers of the observations from each class
   // that land in the child nodes to which they were assigned
   // (Note: For the values of variable split_muwvarIDs[nodeID] the permuted values oob_sampleIDs_thisnodeID_permuted
   // are used, while for the response variable the original values oob_sampleIDs_thisnodeID are used):
   std::vector<size_t> child_node_sizes(split_muwvalues[nodeID].size() + 1, 0);
   std::vector<size_t> class_counts(classes_at_nodes[nodeID].size(), 0);
   for (size_t i = 0; i < oob_sampleIDs_thisnodeID_permuted.size(); ++i)
   {
     size_t sampleID = oob_sampleIDs_thisnodeID_permuted[i];
     double value = data->get(sampleID, split_muwvarIDs[nodeID]);
     for (size_t j = 0; j < split_muwvalues[nodeID].size() + 1; ++j)
     {
       if (j == 0 && value <= split_muwvalues[nodeID][j])
       {
         ++child_node_sizes[j];
         size_t classID = (*response_classIDs)[oob_sampleIDs_thisnodeID[i]];
         for (size_t k = 0; k < classes_at_nodes[nodeID].size(); ++k)
         {
           if (classID == classes_at_nodes[nodeID][k] && assigned_classes[nodeID][k] == j)
           {
             ++class_counts[k];
             break;
           }
         }
         break;
       }
       else if (j == split_muwvalues[nodeID].size() && value > split_muwvalues[nodeID][j - 1])
       {
         ++child_node_sizes[j];
         size_t classID = (*response_classIDs)[oob_sampleIDs_thisnodeID[i]];
         for (size_t k = 0; k < classes_at_nodes[nodeID].size(); ++k)
         {
           if (classID == classes_at_nodes[nodeID][k] && assigned_classes[nodeID][k] == j)
           {
             ++class_counts[k];
             break;
           }
         }
         break;
       }
       else if (value > split_muwvalues[nodeID][j - 1] && value <= split_muwvalues[nodeID][j])
       {
         ++child_node_sizes[j];
         size_t classID = (*response_classIDs)[oob_sampleIDs_thisnodeID[i]];
         for (size_t k = 0; k < classes_at_nodes[nodeID].size(); ++k)
         {
           if (classID == classes_at_nodes[nodeID][k] && assigned_classes[nodeID][k] == j)
           {
             ++class_counts[k];
             break;
           }
         }
         break;
       }
     }
   }
   
   // For i = 0, ..., classes_at_nodes[nodeID].size() - 1, compute the proportion of the observations from oob_sampleIDs_thisnodeID
   // in the child nodes that are from the classes assigned to them:
   std::vector<double> class_proportions;
   for (size_t i = 0; i < classes_at_nodes[nodeID].size(); ++i)
   {
     if (child_node_sizes[assigned_classes[nodeID][i]] == 0)
     {
       class_proportions.push_back(0);
     }
     else
     {
       class_proportions.push_back(((double)class_counts[i] / (double)child_node_sizes[assigned_classes[nodeID][i]]) * ((double)class_counts[i] / (double)child_node_sizes[assigned_classes[nodeID][i]]));
     }
   }
   
   // Weight the class proportions by the size of the child nodes and compute the OOB split criterion value:
   double split_criterion_value = 0;
   for (size_t i = 0; i < classes_at_nodes[nodeID].size(); ++i)
   {
     split_criterion_value += class_proportions[i] * (double)child_node_sizes[assigned_classes[nodeID][i]] / (double)oob_sampleIDs_thisnodeID.size();
   }
   
   return split_criterion_value;
 }
 
 // Class-focused/discriminatory VIM: The function computeImportanceNodePermutedUniv computes the importance for the current variable varID after permuting it for binary splits:
 double TreeClassification::computeImportanceNodePermutedDiscr(size_t nodeID, std::vector<size_t> oob_sampleIDs_thisnodeID, std::vector<size_t> oob_sampleIDs_thisnodeID_permuted)
 {
   
   // Compute the Gini impurity of the parent node:
   //////double gini_parent = computeGiniImpurity(oob_sampleIDs_thisnodeID);
   
   // Determine the OOB observations that are assigned to the left and right child nodes:
   
   std::vector<uint> sample_classID_left_child;
   sample_classID_left_child.reserve(oob_sampleIDs_thisnodeID.size());
   std::vector<uint> sample_classID_right_child;
   sample_classID_right_child.reserve(oob_sampleIDs_thisnodeID.size());
   
   for (size_t i = 0; i < oob_sampleIDs_thisnodeID_permuted.size(); ++i)
   {
     size_t sampleID = oob_sampleIDs_thisnodeID_permuted[i];
     double value = data->get(sampleID, split_varIDs[nodeID]);
     
     uint sample_classID = (*response_classIDs)[oob_sampleIDs_thisnodeID[i]];
     
     if (value <= split_values[nodeID])
     {
       sample_classID_left_child.push_back(sample_classID);
     }
     else
     {
       sample_classID_right_child.push_back(sample_classID);
     }
   }
   
   // If sample_classID_left_child is empty, set the Gini impurity of the left child node to 0:
   double gini_left = 1;
   if (sample_classID_left_child.size() > 0)
   {
     
     // Compute the number of samples in the right child node:
     size_t num_samples_node = sample_classID_left_child.size();
     
     // Compute the number of classes:
     size_t num_classes = class_values->size();
     
     // Compute the class counts:
     std::vector<size_t> class_counts(num_classes, 0);
     for (size_t i = 0; i < num_samples_node; ++i)
     {
       ++class_counts[sample_classID_left_child[i]];
     }
     
     // Compute the Gini impurity:
     for (size_t i = 0; i < num_classes; ++i)
     {
       double proportion = (double)class_counts[i] / (double)num_samples_node;
       gini_left -= (*class_weights)[i] * proportion * proportion;
     }
   }
   
   // Compute the Gini impurity of the right child node:
   double gini_right = 1;
   if (sample_classID_right_child.size() > 0)
   {
     
     // Compute the number of samples in the right child node:
     size_t num_samples_node = sample_classID_right_child.size();
     
     // Compute the number of classes:
     size_t num_classes = class_values->size();
     
     // Compute the class counts:
     std::vector<size_t> class_counts(num_classes, 0);
     for (size_t i = 0; i < num_samples_node; ++i)
     {
       ++class_counts[sample_classID_right_child[i]];
     }
     
     // Compute the Gini impurity:
     for (size_t i = 0; i < num_classes; ++i)
     {
       double proportion = (double)class_counts[i] / (double)num_samples_node;
       gini_right -= (*class_weights)[i] * proportion * proportion;
     }
   }
   
   // Compute the Gini impurity reduction:
   double gini_reduction = -((double)sample_classID_left_child.size() / (double)oob_sampleIDs_thisnodeID.size()) * gini_left - ((double)sample_classID_right_child.size() / (double)oob_sampleIDs_thisnodeID.size()) * gini_right;
   
   return gini_reduction;
 }
 
 double TreeClassification::computePredictionAccuracyInternal() {
   
   size_t num_predictions = prediction_terminal_nodeIDs.size();
   
   size_t num_missclassifications = 0;
   for (size_t i = 0; i < num_predictions; ++i) {
     size_t terminal_nodeID = prediction_terminal_nodeIDs[i];
     double predicted_value;
     if (divfortype == 1) {
       predicted_value = split_values[terminal_nodeID];
     }
     if (divfortype == 2) {
       predicted_value = split_multvalues[terminal_nodeID][0][0];
     }
     double real_value = data->get(oob_sampleIDs[i], dependent_varID);
     if (predicted_value != real_value) {
       ++num_missclassifications;
     }
   }
   
   return (1.0 - (double) num_missclassifications / (double) num_predictions);
   
 }
 
 bool TreeClassification::findBestSplit(size_t nodeID, std::vector<size_t>& possible_split_varIDs) {
   
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   size_t num_classes = class_values->size();
   double best_decrease = -1;
   size_t best_varID = 0;
   double best_value = 0;
   
   std::vector<size_t> class_counts(num_classes);
   // Compute overall class counts
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     uint sample_classID = (*response_classIDs)[sampleID];
     ++class_counts[sample_classID];
   }
   
   // For all possible split variables
   for (auto& varID : possible_split_varIDs) {
     // Find best split value, if ordered consider all values as split values, else all 2-partitions
     if (data->isOrderedVariable(varID)) {
       
       // Use memory saving method if option set
       if (memory_saving_splitting) {
         findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                  best_decrease);
       } else {
         // Use faster method for both cases
         double q = (double) num_samples_node / (double) data->getNumUniqueDataValues(varID);
         if (q < Q_THRESHOLD) {
           findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                    best_decrease);
         } else {
           findBestSplitValueLargeQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                    best_decrease);
         }
       }
     } else {
       findBestSplitValueUnordered(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                   best_decrease);
     }
   }
   
   // Stop if no good split found
   if (best_decrease < 0) {
     return true;
   }
   
   // Save best values
   split_varIDs[nodeID] = best_varID;
   split_values[nodeID] = best_value;
   
   // Compute gini index for this node and to variable importance if needed
   if (importance_mode == IMP_GINI || importance_mode == IMP_GINI_CORRECTED) {
     addGiniImportance(nodeID, best_varID, best_decrease);
   }
   return false;
 }
 
 // Diversity Forests: Find best split for univariable, binary splitting:
 bool TreeClassification::findBestSplitUnivariate(size_t nodeID, std::vector<std::pair<size_t, double>> sampled_varIDs_values) {
   
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   size_t num_classes = class_values->size();
   double best_decrease = -1;
   size_t best_varID = 0;
   double best_value = 0;
   
   // Only split if there is at least one sampled covariate/split pair:
   if(sampled_varIDs_values.size() > 0) {
     
     std::vector<size_t> class_counts(num_classes);
     // Compute overall class counts
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
       size_t sampleID = sampleIDs[pos];
       uint sample_classID = (*response_classIDs)[sampleID];
       ++class_counts[sample_classID];
     }
     
     // Cycle through the covariate/split pairs and
     // determine the best split out of these:
     /////////////////
     
     size_t varIDtemp; 
     double valuetemp;
     
     for (size_t i = 0; i < sampled_varIDs_values.size(); ++i) {
       
       // Get current covariate ID and split:
       varIDtemp = std::get<0>(sampled_varIDs_values[i]);
       valuetemp = std::get<1>(sampled_varIDs_values[i]);
       
       std::vector<size_t> class_counts_right(num_classes);
       size_t n_right = 0;
       
       // Count samples in right child per class and possible split
       for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
         size_t sampleID = sampleIDs[pos];
         double value = data->get(sampleID, varIDtemp);
         uint sample_classID = (*response_classIDs)[sampleID];
         
         if (value > valuetemp) {
           ++n_right;
           ++class_counts_right[sample_classID];
         }
       }
       
       // Number of samples in left child:
       size_t n_left = num_samples_node - n_right;
       //if (n_left == 0 || n_right == 0) {
       // continue;
       //}
       
       // Sum of squares
       double sum_left = 0;
       double sum_right = 0;
       for (size_t j = 0; j < num_classes; ++j) {
         size_t class_count_right = class_counts_right[j];
         size_t class_count_left = class_counts[j] - class_count_right;
         
         sum_right += (*class_weights)[j] * class_count_right * class_count_right;
         sum_left += (*class_weights)[j] * class_count_left * class_count_left;
       }
       
       // Decrease of impurity
       double decrease = sum_left / (double) n_left + sum_right / (double) n_right;
       
       // If better than before, use this
       if (decrease > best_decrease) {
         best_value = valuetemp;
         best_varID = varIDtemp;
         best_decrease = decrease;
       }
     }
     
   }
   
   // Stop if no good split found
   if (best_decrease < 0) {
     return true;
   }
   
   // Save best values
   split_varIDs[nodeID] = best_varID;
   split_values[nodeID] = best_value;
   
   return false;
   
 }
 
 // Interaction Forests: Find candidate split:
 bool TreeClassification::findBestSplitMultivariate(size_t nodeID, std::vector<size_t> sampled_split_types, std::vector<std::vector<size_t>> sampled_split_multvarIDs, std::vector<std::vector<std::vector<bool>>> sampled_split_directs, std::vector<std::vector<std::vector<double>>> sampled_split_multvalues) {
   
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   size_t num_classes = class_values->size();
   double best_decrease = -1;
   size_t best_split_type;
   std::vector<size_t> best_split_multvarID;
   std::vector<std::vector<bool>> best_split_direct;
   std::vector<std::vector<double>> best_split_multvalue;
   
   
   std::vector<size_t> class_counts(num_classes);
   // Compute overall class counts
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     uint sample_classID = (*response_classIDs)[sampleID];
     ++class_counts[sample_classID];
   }
   
   
   // Cycle through the splits and determine the best split
   // out of these:
   
   for (size_t i = 0; i < sampled_split_types.size(); ++i) {
     
     std::vector<size_t> class_counts_right(num_classes);
     size_t n_right = 0;
     
     // Count samples in right child per class and possible split
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
       size_t sampleID = sampleIDs[pos];
       uint sample_classID = (*response_classIDs)[sampleID];
       bool inrectangle = IsInRectangle(data, sampleID, sampled_split_types[i], sampled_split_multvarIDs[i], sampled_split_directs[i], sampled_split_multvalues[i]);
       if (!inrectangle) {
         ++n_right;
         ++class_counts_right[sample_classID];
       }
     }
     
     // Number of samples in left child:
     size_t n_left = num_samples_node - n_right;
     
     // Sum of squares
     double sum_left = 0;
     double sum_right = 0;
     for (size_t j = 0; j < num_classes; ++j) {
       size_t class_count_right = class_counts_right[j];
       size_t class_count_left = class_counts[j] - class_count_right;
       
       sum_right += (*class_weights)[j] * class_count_right * class_count_right;
       sum_left += (*class_weights)[j] * class_count_left * class_count_left;
     }
     
     // Decrease of impurity
     double decrease = sum_left / (double) n_left + sum_right / (double) n_right;
     
     // If better than before, use this
     if (decrease > best_decrease) {
       
       size_t nvars = sampled_split_multvarIDs[i].size();
       best_split_multvarID.resize(nvars);
       size_t nrects = sampled_split_directs[i].size();
       best_split_direct.resize(nrects);
       best_split_multvalue.resize(nrects);
       
       for (size_t j = 0; j < nrects; j++) {
         best_split_direct[j].resize(nvars);
         best_split_multvalue[j].resize(nvars);
       }
       
       best_split_type = sampled_split_types[i];
       best_split_multvarID = sampled_split_multvarIDs[i];
       best_split_direct = sampled_split_directs[i];
       best_split_multvalue = sampled_split_multvalues[i];
       best_decrease = decrease;
     }
     
   }
   
   // Stop if no good split found
   if (best_decrease < 0) {
     return true;
   }
   
   split_types[nodeID] = best_split_type;
   
   split_multvarIDs[nodeID].resize(best_split_multvarID.size());
   split_multvarIDs[nodeID] = best_split_multvarID;
   
   
   size_t sizeouter = best_split_direct.size();
   
   split_directs[nodeID].resize(sizeouter);
   for (size_t i = 0; i < sizeouter; ++i) {
     split_directs[nodeID][i].resize(best_split_direct[i].size());
   }
   split_directs[nodeID] = best_split_direct;
   
   split_multvalues[nodeID].resize(sizeouter);
   for (size_t i = 0; i < sizeouter; ++i) {
     split_multvalues[nodeID][i].resize(best_split_multvalue[i].size());
   }
   split_multvalues[nodeID] = best_split_multvalue;
   
   return false;
   
 }
 
 
 
 void TreeClassification::findBestSplitValueSmallQ(size_t nodeID, size_t varID, size_t num_classes,
                                                   const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                   double& best_decrease) {
   
   // Create possible split values
   std::vector<double> possible_split_values;
   data->getAllValues(possible_split_values, sampleIDs, varID, start_pos[nodeID], end_pos[nodeID]);
   
   // Try next variable if all equal for this
   if (possible_split_values.size() < 2) {
     return;
   }
   
   // -1 because no split possible at largest value
   const size_t num_splits = possible_split_values.size() - 1;
   if (memory_saving_splitting) {
     std::vector<size_t> class_counts_right(num_splits * num_classes), n_right(num_splits);
     findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                              best_decrease, possible_split_values, class_counts_right, n_right);
   } else {
     std::fill_n(counter_per_class.begin(), num_splits * num_classes, 0);
     std::fill_n(counter.begin(), num_splits, 0);
     findBestSplitValueSmallQ(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                              best_decrease, possible_split_values, counter_per_class, counter);
   }
 }
 
 void TreeClassification::findBestSplitValueSmallQ(size_t nodeID, size_t varID, size_t num_classes,
                                                   const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                   double& best_decrease, const std::vector<double>& possible_split_values, std::vector<size_t>& class_counts_right,
                                                   std::vector<size_t>& n_right) {
   const size_t num_splits = possible_split_values.size() - 1;
   
   // Count samples in right child per class and possbile split
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     double value = data->get(sampleID, varID);
     uint sample_classID = (*response_classIDs)[sampleID];
     
     // Count samples until split_value reached
     for (size_t i = 0; i < num_splits; ++i) {
       if (value > possible_split_values[i]) {
         ++n_right[i];
         ++class_counts_right[i * num_classes + sample_classID];
       } else {
         break;
       }
     }
   }
   
   // Compute decrease of impurity for each possible split
   for (size_t i = 0; i < num_splits; ++i) {
     
     // Stop if one child empty
     size_t n_left = num_samples_node - n_right[i];
     if (n_left == 0 || n_right[i] == 0) {
       continue;
     }
     
     // Sum of squares
     double sum_left = 0;
     double sum_right = 0;
     for (size_t j = 0; j < num_classes; ++j) {
       size_t class_count_right = class_counts_right[i * num_classes + j];
       size_t class_count_left = class_counts[j] - class_count_right;
       
       sum_right += (*class_weights)[j] * class_count_right * class_count_right;
       sum_left += (*class_weights)[j] * class_count_left * class_count_left;
     }
     
     // Decrease of impurity
     double decrease = sum_left / (double) n_left + sum_right / (double) n_right[i];
     
     // If better than before, use this
     if (decrease > best_decrease) {
       best_value = (possible_split_values[i] + possible_split_values[i + 1]) / 2;
       best_varID = varID;
       best_decrease = decrease;
       
       // Use smaller value if average is numerically the same as the larger value
       if (best_value == possible_split_values[i + 1]) {
         best_value = possible_split_values[i];
       }
     }
   }
 }
 
 void TreeClassification::findBestSplitValueLargeQ(size_t nodeID, size_t varID, size_t num_classes,
                                                   const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                   double& best_decrease) {
   
   // Set counters to 0
   size_t num_unique = data->getNumUniqueDataValues(varID);
   std::fill_n(counter_per_class.begin(), num_unique * num_classes, 0);
   std::fill_n(counter.begin(), num_unique, 0);
   
   // Count values
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     size_t index = data->getIndex(sampleID, varID);
     size_t classID = (*response_classIDs)[sampleID];
     
     ++counter[index];
     ++counter_per_class[index * num_classes + classID];
   }
   
   size_t n_left = 0;
   std::vector<size_t> class_counts_left(num_classes);
   
   // Compute decrease of impurity for each split
   for (size_t i = 0; i < num_unique - 1; ++i) {
     
     // Stop if nothing here
     if (counter[i] == 0) {
       continue;
     }
     
     n_left += counter[i];
     
     // Stop if right child empty
     size_t n_right = num_samples_node - n_left;
     if (n_right == 0) {
       break;
     }
     
     // Sum of squares
     double sum_left = 0;
     double sum_right = 0;
     for (size_t j = 0; j < num_classes; ++j) {
       class_counts_left[j] += counter_per_class[i * num_classes + j];
       size_t class_count_right = class_counts[j] - class_counts_left[j];
       
       sum_left += (*class_weights)[j] * class_counts_left[j] * class_counts_left[j];
       sum_right += (*class_weights)[j] * class_count_right * class_count_right;
     }
     
     // Decrease of impurity
     double decrease = sum_right / (double) n_right + sum_left / (double) n_left;
     
     // If better than before, use this
     if (decrease > best_decrease) {
       // Find next value in this node
       size_t j = i + 1;
       while (j < num_unique && counter[j] == 0) {
         ++j;
       }
       
       // Use mid-point split
       best_value = (data->getUniqueDataValue(varID, i) + data->getUniqueDataValue(varID, j)) / 2;
       best_varID = varID;
       best_decrease = decrease;
       
       // Use smaller value if average is numerically the same as the larger value
       if (best_value == data->getUniqueDataValue(varID, j)) {
         best_value = data->getUniqueDataValue(varID, i);
       }
     }
   }
 }
 
 void TreeClassification::findBestSplitValueUnordered(size_t nodeID, size_t varID, size_t num_classes,
                                                      const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                      double& best_decrease) {
   
   // Create possible split values
   std::vector<double> factor_levels;
   data->getAllValues(factor_levels, sampleIDs, varID, start_pos[nodeID], end_pos[nodeID]);
   
   // Try next variable if all equal for this
   if (factor_levels.size() < 2) {
     return;
   }
   
   // Number of possible splits is 2^num_levels
   size_t num_splits = (1 << factor_levels.size());
   
   // Compute decrease of impurity for each possible split
   // Split where all left (0) or all right (1) are excluded
   // The second half of numbers is just left/right switched the first half -> Exclude second half
   for (size_t local_splitID = 1; local_splitID < num_splits / 2; ++local_splitID) {
     
     // Compute overall splitID by shifting local factorIDs to global positions
     size_t splitID = 0;
     for (size_t j = 0; j < factor_levels.size(); ++j) {
       if ((local_splitID & (1 << j))) {
         double level = factor_levels[j];
         size_t factorID = floor(level) - 1;
         splitID = splitID | (1 << factorID);
       }
     }
     
     // Initialize
     std::vector<size_t> class_counts_right(num_classes);
     size_t n_right = 0;
     
     // Count classes in left and right child
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
       size_t sampleID = sampleIDs[pos];
       uint sample_classID = (*response_classIDs)[sampleID];
       double value = data->get(sampleID, varID);
       size_t factorID = floor(value) - 1;
       
       // If in right child, count
       // In right child, if bitwise splitID at position factorID is 1
       if ((splitID & (1 << factorID))) {
         ++n_right;
         ++class_counts_right[sample_classID];
       }
     }
     size_t n_left = num_samples_node - n_right;
     
     // Sum of squares
     double sum_left = 0;
     double sum_right = 0;
     for (size_t j = 0; j < num_classes; ++j) {
       size_t class_count_right = class_counts_right[j];
       size_t class_count_left = class_counts[j] - class_count_right;
       
       sum_right += (*class_weights)[j] * class_count_right * class_count_right;
       sum_left += (*class_weights)[j] * class_count_left * class_count_left;
     }
     
     // Decrease of impurity
     double decrease = sum_left / (double) n_left + sum_right / (double) n_right;
     
     // If better than before, use this
     if (decrease > best_decrease) {
       best_value = splitID;
       best_varID = varID;
       best_decrease = decrease;
     }
   }
 }
 
 bool TreeClassification::findBestSplitExtraTrees(size_t nodeID, std::vector<size_t>& possible_split_varIDs) {
   
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   size_t num_classes = class_values->size();
   double best_decrease = -1;
   size_t best_varID = 0;
   double best_value = 0;
   
   std::vector<size_t> class_counts(num_classes);
   // Compute overall class counts
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     uint sample_classID = (*response_classIDs)[sampleID];
     ++class_counts[sample_classID];
   }
   
   // For all possible split variables
   for (auto& varID : possible_split_varIDs) {
     // Find best split value, if ordered consider all values as split values, else all 2-partitions
     if (data->isOrderedVariable(varID)) {
       findBestSplitValueExtraTrees(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                    best_decrease);
     } else {
       findBestSplitValueExtraTreesUnordered(nodeID, varID, num_classes, class_counts, num_samples_node, best_value,
                                             best_varID, best_decrease);
     }
   }
   
   // Stop if no good split found
   if (best_decrease < 0) {
     return true;
   }
   
   // Save best values
   split_varIDs[nodeID] = best_varID;
   split_values[nodeID] = best_value;
   
   // Compute gini index for this node and to variable importance if needed
   if (importance_mode == IMP_GINI || importance_mode == IMP_GINI_CORRECTED) {
     addGiniImportance(nodeID, best_varID, best_decrease);
   }
   return false;
 }
 
 void TreeClassification::findBestSplitValueExtraTrees(size_t nodeID, size_t varID, size_t num_classes,
                                                       const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                       double& best_decrease) {
   
   // Get min/max values of covariate in node
   double min;
   double max;
   data->getMinMaxValues(min, max, sampleIDs, varID, start_pos[nodeID], end_pos[nodeID]);
   
   // Try next variable if all equal for this
   if (min == max) {
     return;
   }
   
   // Create possible split values: Draw randomly between min and max
   std::vector<double> possible_split_values;
   std::uniform_real_distribution<double> udist(min, max);
   possible_split_values.reserve(num_random_splits);
   for (size_t i = 0; i < num_random_splits; ++i) {
     possible_split_values.push_back(udist(random_number_generator));
   }
   
   const size_t num_splits = possible_split_values.size();
   if (memory_saving_splitting) {
     std::vector<size_t> class_counts_right(num_splits * num_classes), n_right(num_splits);
     findBestSplitValueExtraTrees(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                  best_decrease, possible_split_values, class_counts_right, n_right);
   } else {
     std::fill_n(counter_per_class.begin(), num_splits * num_classes, 0);
     std::fill_n(counter.begin(), num_splits, 0);
     findBestSplitValueExtraTrees(nodeID, varID, num_classes, class_counts, num_samples_node, best_value, best_varID,
                                  best_decrease, possible_split_values, counter_per_class, counter);
   }
 }
 
 void TreeClassification::findBestSplitValueExtraTrees(size_t nodeID, size_t varID, size_t num_classes,
                                                       const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                       double& best_decrease, const std::vector<double>& possible_split_values, std::vector<size_t>& class_counts_right,
                                                       std::vector<size_t>& n_right) {
   const size_t num_splits = possible_split_values.size();
   
   // Count samples in right child per class and possbile split
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     double value = data->get(sampleID, varID);
     uint sample_classID = (*response_classIDs)[sampleID];
     
     // Count samples until split_value reached
     for (size_t i = 0; i < num_splits; ++i) {
       if (value > possible_split_values[i]) {
         ++n_right[i];
         ++class_counts_right[i * num_classes + sample_classID];
       } else {
         break;
       }
     }
   }
   
   // Compute decrease of impurity for each possible split
   for (size_t i = 0; i < num_splits; ++i) {
     
     // Stop if one child empty
     size_t n_left = num_samples_node - n_right[i];
     if (n_left == 0 || n_right[i] == 0) {
       continue;
     }
     
     // Sum of squares
     double sum_left = 0;
     double sum_right = 0;
     for (size_t j = 0; j < num_classes; ++j) {
       size_t class_count_right = class_counts_right[i * num_classes + j];
       size_t class_count_left = class_counts[j] - class_count_right;
       
       sum_right += (*class_weights)[j] * class_count_right * class_count_right;
       sum_left += (*class_weights)[j] * class_count_left * class_count_left;
     }
     
     // Decrease of impurity
     double decrease = sum_left / (double) n_left + sum_right / (double) n_right[i];
     
     // If better than before, use this
     if (decrease > best_decrease) {
       best_value = possible_split_values[i];
       best_varID = varID;
       best_decrease = decrease;
     }
   }
 }
 
 void TreeClassification::findBestSplitValueExtraTreesUnordered(size_t nodeID, size_t varID, size_t num_classes,
                                                                const std::vector<size_t>& class_counts, size_t num_samples_node, double& best_value, size_t& best_varID,
                                                                double& best_decrease) {
   
   size_t num_unique_values = data->getNumUniqueDataValues(varID);
   
   // Get all factor indices in node
   std::vector<bool> factor_in_node(num_unique_values, false);
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     size_t index = data->getIndex(sampleID, varID);
     factor_in_node[index] = true;
   }
   
   // Vector of indices in and out of node
   std::vector<size_t> indices_in_node;
   std::vector<size_t> indices_out_node;
   indices_in_node.reserve(num_unique_values);
   indices_out_node.reserve(num_unique_values);
   for (size_t i = 0; i < num_unique_values; ++i) {
     if (factor_in_node[i]) {
       indices_in_node.push_back(i);
     } else {
       indices_out_node.push_back(i);
     }
   }
   
   // Generate num_random_splits splits
   for (size_t i = 0; i < num_random_splits; ++i) {
     std::vector<size_t> split_subset;
     split_subset.reserve(num_unique_values);
     
     // Draw random subsets, sample all partitions with equal probability
     if (indices_in_node.size() > 1) {
       size_t num_partitions = (2 << (indices_in_node.size() - 1)) - 2; // 2^n-2 (don't allow full or empty)
       std::uniform_int_distribution<size_t> udist(1, num_partitions);
       size_t splitID_in_node = udist(random_number_generator);
       for (size_t j = 0; j < indices_in_node.size(); ++j) {
         if ((splitID_in_node & (1 << j)) > 0) {
           split_subset.push_back(indices_in_node[j]);
         }
       }
     }
     if (indices_out_node.size() > 1) {
       size_t num_partitions = (2 << (indices_out_node.size() - 1)) - 1; // 2^n-1 (allow full or empty)
       std::uniform_int_distribution<size_t> udist(0, num_partitions);
       size_t splitID_out_node = udist(random_number_generator);
       for (size_t j = 0; j < indices_out_node.size(); ++j) {
         if ((splitID_out_node & (1 << j)) > 0) {
           split_subset.push_back(indices_out_node[j]);
         }
       }
     }
     
     // Assign union of the two subsets to right child
     size_t splitID = 0;
     for (auto& idx : split_subset) {
       splitID |= 1 << idx;
     }
     
     // Initialize
     std::vector<size_t> class_counts_right(num_classes);
     size_t n_right = 0;
     
     // Count classes in left and right child
     for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
       size_t sampleID = sampleIDs[pos];
       uint sample_classID = (*response_classIDs)[sampleID];
       double value = data->get(sampleID, varID);
       size_t factorID = floor(value) - 1;
       
       // If in right child, count
       // In right child, if bitwise splitID at position factorID is 1
       if ((splitID & (1 << factorID))) {
         ++n_right;
         ++class_counts_right[sample_classID];
       }
     }
     size_t n_left = num_samples_node - n_right;
     
     // Sum of squares
     double sum_left = 0;
     double sum_right = 0;
     for (size_t j = 0; j < num_classes; ++j) {
       size_t class_count_right = class_counts_right[j];
       size_t class_count_left = class_counts[j] - class_count_right;
       
       sum_right += (*class_weights)[j] * class_count_right * class_count_right;
       sum_left += (*class_weights)[j] * class_count_left * class_count_left;
     }
     
     // Decrease of impurity
     double decrease = sum_left / (double) n_left + sum_right / (double) n_right;
     
     // If better than before, use this
     if (decrease > best_decrease) {
       best_value = splitID;
       best_varID = varID;
       best_decrease = decrease;
     }
   }
 }
 
 void TreeClassification::addGiniImportance(size_t nodeID, size_t varID, double decrease) {
   
   size_t num_samples_node = end_pos[nodeID] - start_pos[nodeID];
   std::vector<size_t> class_counts;
   class_counts.resize(class_values->size(), 0);
   
   for (size_t pos = start_pos[nodeID]; pos < end_pos[nodeID]; ++pos) {
     size_t sampleID = sampleIDs[pos];
     uint sample_classID = (*response_classIDs)[sampleID];
     class_counts[sample_classID]++;
   }
   double sum_node = 0;
   for (auto& class_count : class_counts) {
     sum_node += class_count * class_count;
   }
   double best_gini = decrease - sum_node / (double) num_samples_node;
   
   // No variable importance for no split variables
   size_t tempvarID = data->getUnpermutedVarID(varID);
   for (auto& skip : data->getNoSplitVariables()) {
     if (tempvarID >= skip) {
       --tempvarID;
     }
   }
   
   // Subtract if corrected importance and permuted variable, else add
   if (importance_mode == IMP_GINI_CORRECTED && varID >= data->getNumCols()) {
     (*variable_importance)[tempvarID] -= best_gini;
   } else {
     (*variable_importance)[tempvarID] += best_gini;
   }
 }
 
 void TreeClassification::bootstrapClassWise() {
   // Number of samples is sum of sample fraction * number of samples
   size_t num_samples_inbag = 0;
   double sum_sample_fraction = 0;
   for (auto& s : *sample_fraction) {
     num_samples_inbag += (size_t) num_samples * s;
     sum_sample_fraction += s;
   }
   
   // Reserve space, reserve a little more to be save)
   sampleIDs.reserve(num_samples_inbag);
   oob_sampleIDs.reserve(num_samples * (exp(-sum_sample_fraction) + 0.1));
   
   // Start with all samples OOB
   inbag_counts.resize(num_samples, 0);
   
   // Draw samples for each class
   for (size_t i = 0; i < sample_fraction->size(); ++i) {
     // Draw samples of class with replacement as inbag and mark as not OOB
     size_t num_samples_class = (*sampleIDs_per_class)[i].size();
     size_t num_samples_inbag_class = round(num_samples * (*sample_fraction)[i]);
     std::uniform_int_distribution<size_t> unif_dist(0, num_samples_class - 1);
     for (size_t s = 0; s < num_samples_inbag_class; ++s) {
       size_t draw = (*sampleIDs_per_class)[i][unif_dist(random_number_generator)];
       sampleIDs.push_back(draw);
       ++inbag_counts[draw];
     }
   }
   
   // Save OOB samples
   for (size_t s = 0; s < inbag_counts.size(); ++s) {
     if (inbag_counts[s] == 0) {
       oob_sampleIDs.push_back(s);
     }
   }
   num_samples_oob = oob_sampleIDs.size();
   
   if (!keep_inbag) {
     inbag_counts.clear();
     inbag_counts.shrink_to_fit();
   }
 }
 
 void TreeClassification::bootstrapWithoutReplacementClassWise() {
   // Draw samples for each class
   for (size_t i = 0; i < sample_fraction->size(); ++i) {
     size_t num_samples_class = (*sampleIDs_per_class)[i].size();
     size_t num_samples_inbag_class = round(num_samples * (*sample_fraction)[i]);
     
     shuffleAndSplitAppend(sampleIDs, oob_sampleIDs, num_samples_class, num_samples_inbag_class,
                           (*sampleIDs_per_class)[i], random_number_generator);
   }
   
   if (keep_inbag) {
     // All observation are 0 or 1 times inbag
     inbag_counts.resize(num_samples, 1);
     for (size_t i = 0; i < oob_sampleIDs.size(); i++) {
       inbag_counts[oob_sampleIDs[i]] = 0;
     }
   }
 }
 
 } // namespace ranger
 
