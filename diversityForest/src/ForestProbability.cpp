/*-------------------------------------------------------------------------------
 This file is part of diversityForest.
 
 Copyright (c) [2014-2018] [Marvin N. Wright]
 
 This software may be modified and distributed under the terms of the MIT license.
 
 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
#-------------------------------------------------------------------------------*/
 
#include <Rcpp.h>
#include <chrono>   // Required for std::chrono
#include <thread>   // Required for std::this_thread
 
#include <stdexcept>
 
#include "utility.h"
#include "ForestProbability.h"
#include "TreeProbability.h"
#include "Data.h"
 
 namespace diversityForest {
 
 void ForestProbability::loadForest(size_t dependent_varID, size_t num_trees,
                                    std::vector<std::vector<std::vector<size_t>> >& forest_child_nodeIDs,
                                    std::vector<std::vector<size_t>>& forest_split_varIDs, std::vector<std::vector<double>>& forest_split_values,
                                    std::vector<std::vector<size_t>>& forest_split_types, std::vector<std::vector<std::vector<size_t>>>& forest_split_multvarIDs, 
                                    std::vector<std::vector<std::vector<std::vector<bool>>>>& forest_split_directs, std::vector<std::vector<std::vector<std::vector<double>>>>& forest_split_multvalues,
                                    std::vector<std::vector<std::vector<size_t>> >& forest_child_muwnodeIDs,
                                    std::vector<std::vector<std::vector<double>>>& forest_split_muwvalues,
                                    std::vector<double>& class_values, std::vector<std::vector<std::vector<double>>>& forest_terminal_class_counts,
                                    std::vector<bool>& is_ordered_variable) {
   
   this->dependent_varID = dependent_varID;
   this->num_trees = num_trees;
   this->class_values = class_values;
   data->setIsOrderedVariable(is_ordered_variable);
   
   std::vector<size_t> empty_split_muwvarIDs; 
   std::vector<std::vector<double>> empty_split_muwvalues;
   
   // Create trees
   trees.reserve(num_trees);
   for (size_t i = 0; i < num_trees; ++i) {
     trees.push_back(
       std::make_unique<TreeProbability>(forest_child_nodeIDs[i], forest_split_varIDs[i], forest_split_values[i], forest_split_types[i], forest_split_multvarIDs[i], 
                                         forest_split_directs[i], forest_split_multvalues[i], forest_child_muwnodeIDs[i], empty_split_muwvarIDs, empty_split_muwvalues, &this->class_values, &response_classIDs, forest_terminal_class_counts[i]));
   }
   
   // Create thread ranges
   equalSplit(thread_ranges, 0, num_trees - 1, num_threads);
 }
 
 std::vector<std::vector<std::vector<double>>> ForestProbability::getTerminalClassCounts() const {
   std::vector<std::vector<std::vector<double>>> result;
   result.reserve(num_trees);
   for (const auto& tree : trees) {
     const auto& temp = dynamic_cast<const TreeProbability&>(*tree);
     result.push_back(temp.getTerminalClassCounts());
   }
   return result;
 }
 
 void ForestProbability::initInternal(std::string status_variable_name) {
   
   // If npairs not set, use floored square root of number of independent variables.
   if (npairs == 0) {
     unsigned long temp = (size_t)ceil(sqrt((double) (num_variables - 1)) / 2);
     npairs = temp;
   }
   
   // If mtry not set, use floored square root of number of independent variables.
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
     min_node_size = DEFAULT_MIN_NODE_SIZE_PROBABILITY;
   }
   
   // Create class_values and response_classIDs
   if (!prediction_mode) {
     for (size_t i = 0; i < num_samples; ++i) {
       double value = data->get(i, dependent_varID);
       
       // If classID is already in class_values, use ID. Else create a new one.
       uint classID = find(class_values.begin(), class_values.end(), value) - class_values.begin();
       if (classID == class_values.size()) {
         class_values.push_back(value);
       }
       response_classIDs.push_back(classID);
     }
   }
   
   // Create sampleIDs_per_class if required
   if (sample_fraction.size() > 1) {
     sampleIDs_per_class.resize(sample_fraction.size());
     for (auto& v : sampleIDs_per_class) {
       v.reserve(num_samples);
     }
     for (size_t i = 0; i < num_samples; ++i) {
       size_t classID = response_classIDs[i];
       sampleIDs_per_class[classID].push_back(i);
     }
   }
   
   // Set class weights all to 1
   class_weights = std::vector<double>(class_values.size(), 1.0);
   
   // Sort data if memory saving mode
   if (!memory_saving_splitting) {
     data->sort();
   }
 }
 
 void ForestProbability::growInternal() {
   trees.reserve(num_trees);
   for (size_t i = 0; i < num_trees; ++i) {
     trees.push_back(
       std::make_unique<TreeProbability>(&class_values, &response_classIDs, &sampleIDs_per_class, &class_weights));
   }
 }
 
 void ForestProbability::allocatePredictMemory() {
   size_t num_prediction_samples = data->getNumRows();
   if (predict_all) {
     predictions = std::vector<std::vector<std::vector<double>>>(num_prediction_samples,
                                                                 std::vector<std::vector<double>>(class_values.size(), std::vector<double>(num_trees, 0)));
   } else if (prediction_type == TERMINALNODES) {
     predictions = std::vector<std::vector<std::vector<double>>>(1,
                                                                 std::vector<std::vector<double>>(num_prediction_samples, std::vector<double>(num_trees, 0)));
   } else {
     predictions = std::vector<std::vector<std::vector<double>>>(1,
                                                                 std::vector<std::vector<double>>(num_prediction_samples, std::vector<double>(class_values.size(), 0)));
   }
 }
 
 void ForestProbability::predictInternal(size_t sample_idx) {
   // For each sample compute proportions in each tree
   for (size_t tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
     if (predict_all) {
       std::vector<double> counts = getTreePrediction(tree_idx, sample_idx);
       
       for (size_t class_idx = 0; class_idx < counts.size(); ++class_idx) {
         predictions[sample_idx][class_idx][tree_idx] += counts[class_idx];
       }
     } else if (prediction_type == TERMINALNODES) {
       predictions[0][sample_idx][tree_idx] = getTreePredictionTerminalNodeID(tree_idx, sample_idx);
     } else {
       std::vector<double> counts = getTreePrediction(tree_idx, sample_idx);
       
       for (size_t class_idx = 0; class_idx < counts.size(); ++class_idx) {
         predictions[0][sample_idx][class_idx] += counts[class_idx];
       }
     }
   }
   
   // Average over trees
   if (!predict_all && prediction_type != TERMINALNODES) {
     for (size_t class_idx = 0; class_idx < predictions[0][sample_idx].size(); ++class_idx) {
       predictions[0][sample_idx][class_idx] /= num_trees;
     }
   }
 }
 
 void ForestProbability::computePredictionErrorInternal() {
   
   // For each sample sum over trees where sample is OOB
   std::vector<size_t> samples_oob_count;
   samples_oob_count.resize(num_samples, 0);
   predictions = std::vector<std::vector<std::vector<double>>>(1,
                                                               std::vector<std::vector<double>>(num_samples, std::vector<double>(class_values.size(), 0)));
   
   for (size_t tree_idx = 0; tree_idx < num_trees; ++tree_idx) {
     for (size_t sample_idx = 0; sample_idx < trees[tree_idx]->getNumSamplesOob(); ++sample_idx) {
       size_t sampleID = trees[tree_idx]->getOobSampleIDs()[sample_idx];
       std::vector<double> counts = getTreePrediction(tree_idx, sample_idx);
       
       for (size_t class_idx = 0; class_idx < counts.size(); ++class_idx) {
         predictions[0][sampleID][class_idx] += counts[class_idx];
       }
       ++samples_oob_count[sampleID];
     }
   }
   
   // MSE with predicted probability and true data
   size_t num_predictions = 0;
   overall_prediction_error = 0;
   for (size_t i = 0; i < predictions[0].size(); ++i) {
     if (samples_oob_count[i] > 0) {
       ++num_predictions;
       for (size_t j = 0; j < predictions[0][i].size(); ++j) {
         predictions[0][i][j] /= (double) samples_oob_count[i];
       }
       size_t real_classID = response_classIDs[i];
       double predicted_value = predictions[0][i][real_classID];
       overall_prediction_error += (1 - predicted_value) * (1 - predicted_value);
     } else {
       for (size_t j = 0; j < predictions[0][i].size(); ++j) {
         predictions[0][i][j] = NAN;
       }
     }
   }
   
   overall_prediction_error /= (double) num_predictions;
 }
 
 // #nocov start
 void ForestProbability::saveToFileInternal(std::ofstream& outfile) {
   
   // Write num_variables
   outfile.write((char*) &num_variables, sizeof(num_variables));
   
   // Write treetype
   TreeType treetype = TREE_PROBABILITY;
   outfile.write((char*) &treetype, sizeof(treetype));
   
   // Write class_values
   saveVector1D(class_values, outfile);
 }
 
 const std::vector<double>& ForestProbability::getTreePrediction(size_t tree_idx, size_t sample_idx) const {
   const auto& tree = dynamic_cast<const TreeProbability&>(*trees[tree_idx]);
   return tree.getPrediction(sample_idx);
 }
 
 size_t ForestProbability::getTreePredictionTerminalNodeID(size_t tree_idx, size_t sample_idx) const {
   const auto& tree = dynamic_cast<const TreeProbability&>(*trees[tree_idx]);
   return tree.getPredictionTerminalNodeID(sample_idx);
 }
 
 // Class-focused/discriminatory VIM: Compute variable importance.
 void ForestProbability::computeImportanceMuw() {
   
   // Compute variable importance in multiple threads
   progress = 0;
#ifdef R_BUILD
   aborted = false;
   aborted_threads = 0;
#endif
   
   std::vector<std::thread> threads;
   threads.reserve(num_threads);
   
   // Initialize importance
   std::vector<std::vector<double>> var_imp_classfoc_threads(num_threads);
   std::vector<std::vector<double>> var_imp_discr_threads(num_threads);
   
   // Compute importance
   for (uint i = 0; i < num_threads; ++i)
   {    
     if (importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_BOTH)
     {
       var_imp_classfoc_threads[i].resize(num_variables, 0);
     }
     if (importance_mode == MUWIMP_DISCR || importance_mode == MUWIMP_BOTH)
     {
       var_imp_discr_threads[i].resize(num_variables, 0);
     }
     threads.emplace_back(&ForestProbability::computeTreeImportanceMuwInThread, this, i,
                          std::ref(var_imp_classfoc_threads[i]), std::ref(var_imp_discr_threads[i]));
   }
   showProgress("Computing variable importance..", num_trees);
   for (auto &thread : threads)
   {
     thread.join();
   }
   
#ifdef R_BUILD
   if (aborted_threads > 0)
   {
     throw std::runtime_error("User interrupt.");
   }
#endif
   
   // Sum thread importances
   if (importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_BOTH)
   {
     var_imp_classfoc.resize(num_variables, 0);
     for (size_t i = 0; i < num_variables; ++i)
     {
       for (uint j = 0; j < num_threads; ++j)
       {
         var_imp_classfoc[i] += var_imp_classfoc_threads[j][i];
       }
     }  
   }
   
   if (importance_mode == MUWIMP_DISCR || importance_mode == MUWIMP_BOTH)
   {
     var_imp_discr.resize(num_variables, 0);
     for (size_t i = 0; i < num_variables; ++i)
     {
       for (uint j = 0; j < num_threads; ++j)
       {
         var_imp_discr[i] += var_imp_discr_threads[j][i];
       }
     }  
   }
   
   var_imp_classfoc_threads.clear();
   var_imp_discr_threads.clear();
   
   
   // Determine the indices of the independent variables:
   std::vector<size_t> all_vars;
   for (size_t i = 0; i < data->getNumCols(); ++i)
   {
     // If the variable is not in data->getNoSplitVariables(), add it to all_vars:
     if (std::find(data->getNoSplitVariables().begin(), data->getNoSplitVariables().end(), i) == data->getNoSplitVariables().end())
     {
       all_vars.push_back(i);
     }
   }
   
   // Keep only those elements of variable_importance that have in all_vars:
   
   if (importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_BOTH)
   {
     std::vector<double> var_imp_classfoc_indep(num_independent_variables);
     for (size_t i = 0; i < num_independent_variables; ++i)
     {
       var_imp_classfoc_indep[i] = var_imp_classfoc[all_vars[i]];
     }
     var_imp_classfoc = var_imp_classfoc_indep;
   }
   
   if (importance_mode == MUWIMP_DISCR || importance_mode == MUWIMP_BOTH)
   {
     std::vector<double> var_imp_discr_indep(num_independent_variables);
     for (size_t i = 0; i < num_independent_variables; ++i)
     {
       var_imp_discr_indep[i] = var_imp_discr[all_vars[i]];
     }
     var_imp_discr = var_imp_discr_indep;
   }
   
   
   if (importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_BOTH)
   {
     for (size_t i = 0; i < var_imp_classfoc.size(); ++i)
     {
       var_imp_classfoc[i] /= num_trees;
     }
   }
   
   if (importance_mode == MUWIMP_DISCR || importance_mode == MUWIMP_BOTH)
   {
     for (size_t i = 0; i < var_imp_discr.size(); ++i)
     {
       var_imp_discr[i] /= num_trees;
     }
   }
   
 }
 
 // Class-focused/discriminatory VIM: Compute variable importance in one thread.
 void ForestProbability::computeTreeImportanceMuwInThread(uint thread_idx, std::vector<double>& importance_classfoc,
                                                          std::vector<double>& importance_discr) {
   if (thread_ranges.size() > thread_idx + 1) {
     for (size_t i = thread_ranges[thread_idx]; i < thread_ranges[thread_idx + 1]; ++i) {
       if (auto treeClassPtr = dynamic_cast<TreeProbability*>(trees[i].get())) {
         treeClassPtr->computeImportanceMuw(importance_classfoc, importance_discr);
       }
       
       // Check for user interrupt
#ifdef R_BUILD
       if (aborted) {
         std::unique_lock<std::mutex> lock(mutex);
         ++aborted_threads;
         condition_variable.notify_one();
         return;
       }
#endif
       
       // Increase progress by 1 tree
       std::unique_lock<std::mutex> lock(mutex);
       ++progress;
       condition_variable.notify_one();
     }
   }
 }
 
 // #nocov end
 
 }// namespace diversityForest
 
