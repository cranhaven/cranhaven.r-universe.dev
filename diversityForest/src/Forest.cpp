/*-------------------------------------------------------------------------------
 This file is part of diversityForest.

 Copyright (c) [2014-2018] [Marvin N. Wright]

 This software may be modified and distributed under the terms of the MIT license.

 Please note that the C++ core of divfor is distributed under MIT license and the
 R package "diversityForest" under GPL3 license.
 #-------------------------------------------------------------------------------*/

#include <Rcpp.h>
#include <math.h>
#include <algorithm>
#include <stdexcept>
#include <string>
#include <ctime>
#include <functional>
#include <thread>
#include <chrono>

#include "utility.h"
#include "Forest.h"
#include "DataChar.h"
#include "DataDouble.h"
#include "DataFloat.h"

namespace diversityForest {

Forest::Forest() :
    verbose_out(0), num_trees(DEFAULT_NUM_TREE), mtry(0), min_node_size(0), num_variables(0), num_independent_variables(
        0), seed(0), dependent_varID(0), num_samples(0), prediction_mode(false), memory_mode(MEM_DOUBLE), sample_with_replacement(
        true), memory_saving_splitting(false), splitrule(DEFAULT_SPLITRULE), predict_all(false), keep_inbag(false), sample_fraction(
        { 1 }), holdout(false), prediction_type(DEFAULT_PREDICTIONTYPE), num_random_splits(DEFAULT_NUM_RANDOM_SPLITS), max_depth(
        DEFAULT_MAXDEPTH), nsplits(0), npairs(0), proptry(0.0), alpha(DEFAULT_ALPHA), minprop(DEFAULT_MINPROP), num_threads(DEFAULT_NUM_THREADS), data { }, overall_prediction_error(
    NAN), importance_mode(DEFAULT_IMPORTANCE_MODE), progress(0) { 
}

void Forest::initR(std::string dependent_variable_name, std::unique_ptr<Data> input_data, uint mtry, uint num_trees,
    std::ostream* verbose_out, uint seed, uint num_threads, ImportanceMode importance_mode, uint min_node_size,
    std::vector<std::vector<double>>& split_select_weights, const std::vector<std::string>& always_split_variable_names,
    std::string status_variable_name, bool prediction_mode, bool sample_with_replacement,
    const std::vector<std::string>& unordered_variable_names, bool memory_saving_splitting, SplitRule splitrule,
    std::vector<double>& case_weights, std::vector<std::vector<size_t>>& manual_inbag, bool predict_all,
    bool keep_inbag, std::vector<double>& sample_fraction, uint nsplits, uint npairs, double proptry, double alpha, double minprop, bool holdout,
    PredictionType prediction_type, uint num_random_splits, bool order_snps, uint max_depth, std::vector<std::vector<size_t>>& promispairs, uint& eim_mode, uint& divfortype) {

  this->verbose_out = verbose_out;

  // Call other init function
  init(dependent_variable_name, MEM_DOUBLE, std::move(input_data), mtry, "", num_trees, seed, num_threads,
      importance_mode, min_node_size, status_variable_name, prediction_mode, sample_with_replacement,
      unordered_variable_names, memory_saving_splitting, splitrule, predict_all, sample_fraction, nsplits, npairs, proptry, alpha, minprop,
      holdout, prediction_type, num_random_splits, order_snps, max_depth, promispairs, eim_mode, divfortype);

  // Set variables to be always considered for splitting
  if (!always_split_variable_names.empty()) {
    setAlwaysSplitVariables(always_split_variable_names);
  }

  // Set split select weights
  if (!split_select_weights.empty()) {
    setSplitWeightVector(split_select_weights);
  }

  // Set case weights
  if (!case_weights.empty()) {
    if (case_weights.size() != num_samples) {
      throw std::runtime_error("Number of case weights not equal to number of samples.");
    }
    this->case_weights = case_weights;
  }

  // Set manual inbag
  if (!manual_inbag.empty()) {
    this->manual_inbag = manual_inbag;
  }

  // Keep inbag counts
  this->keep_inbag = keep_inbag;
}

void Forest::init(std::string dependent_variable_name, MemoryMode memory_mode, std::unique_ptr<Data> input_data,
    uint mtry, std::string output_prefix, uint num_trees, uint seed, uint num_threads, ImportanceMode importance_mode,
    uint min_node_size, std::string status_variable_name, bool prediction_mode, bool sample_with_replacement,
    const std::vector<std::string>& unordered_variable_names, bool memory_saving_splitting, SplitRule splitrule,
    bool predict_all, std::vector<double>& sample_fraction, uint nsplits, uint npairs, double proptry, double alpha, double minprop, bool holdout,
    PredictionType prediction_type, uint num_random_splits, bool order_snps, uint max_depth, std::vector<std::vector<size_t>>& promispairs, uint eim_mode, uint divfortype) {

  // Initialize data with memmode
  this->data = std::move(input_data);

  // Initialize random number generator and set seed
  if (seed == 0) {
    std::random_device random_device;
    random_number_generator.seed(random_device());
  } else {
    random_number_generator.seed(seed);
  }

  // Set number of threads
  if (num_threads == DEFAULT_NUM_THREADS) {
    this->num_threads = std::thread::hardware_concurrency();
  } else {
    this->num_threads = num_threads;
  }

  // Set member variables
  this->num_trees = num_trees;
  this->mtry = mtry;
  this->seed = seed;
  this->output_prefix = output_prefix;
  this->importance_mode = importance_mode;
  this->min_node_size = min_node_size;
  this->memory_mode = memory_mode;
  this->prediction_mode = prediction_mode;
  this->sample_with_replacement = sample_with_replacement;
  this->memory_saving_splitting = memory_saving_splitting;
  this->splitrule = splitrule;
  this->predict_all = predict_all;
  this->sample_fraction = sample_fraction;
  this->holdout = holdout;
  this->alpha = alpha;
  this->minprop = minprop;
  this->prediction_type = prediction_type;
  this->num_random_splits = num_random_splits;
  this->max_depth = max_depth;
  this->nsplits = nsplits;
  this->npairs = npairs;
  this->proptry = proptry;
  this->promispairs = promispairs;
  this->eim_mode = eim_mode;
  this->divfortype = divfortype;
  
  // Set number of samples and variables
  num_samples = data->getNumRows();
  num_variables = data->getNumCols();

  // Convert dependent variable name to ID
  if (!prediction_mode && !dependent_variable_name.empty()) {
    dependent_varID = data->getVariableID(dependent_variable_name);
  }

  // Set unordered factor variables
  if (!prediction_mode) {
    data->setIsOrderedVariable(unordered_variable_names);
  }

  data->addNoSplitVariable(dependent_varID);

  initInternal(status_variable_name);

  num_independent_variables = num_variables - data->getNoSplitVariables().size();

  // Init split select weights
  split_select_weights.push_back(std::vector<double>());

  // Init manual inbag
  manual_inbag.push_back(std::vector<size_t>());

  // Check if mtry is in valid range
  if (this->mtry > num_variables - 1) {
    throw std::runtime_error("mtry can not be larger than number of variables in data.");
  }

  // Check if any observations samples
  if ((size_t) num_samples * sample_fraction[0] < 1) {
    throw std::runtime_error("sample_fraction too small, no observations sampled.");
  }

  // Permute samples for corrected Gini importance
  if (importance_mode == IMP_GINI_CORRECTED) {
    data->permuteSampleIDs(random_number_generator);
  }

  // Order SNP levels if in "order" splitting
  if (!prediction_mode && order_snps) {
    data->orderSnpLevels(dependent_variable_name, (importance_mode == IMP_GINI_CORRECTED));
  }
}

void Forest::run(bool verbose, bool compute_oob_error) {

  if (prediction_mode) {
    if (verbose && verbose_out) {
      *verbose_out << "Predicting .." << std::endl;
    }
    predict();
  } else {
    if (verbose && verbose_out) {
      *verbose_out << "Growing trees .." << std::endl;
    }

    grow();

    if (verbose && verbose_out) {
      *verbose_out << "Computing prediction error .." << std::endl;
    }

    if (compute_oob_error) {
      computePredictionError();
    }

    if (importance_mode == IMP_PERM_BREIMAN || importance_mode == IMP_PERM_LIAW || importance_mode == IMP_PERM_RAW || importance_mode == MUWIMP_BOTH || importance_mode == MUWIMP_CLASSFOC || importance_mode == MUWIMP_DISCR) {
      if (verbose && verbose_out) {
        *verbose_out << "Computing permutation variable importance .." << std::endl;
      }
	  if (divfortype == 1) {
        computePermutationImportance();
	  }
	  if (divfortype == 2) {
        computePermutationImportanceMultivariate();
	  }
	  if (divfortype == 3) {
        computeImportanceMuw();
	  }
    }
  }
}

// #nocov start
void Forest::saveToFile() {

  // Open file for writing
  std::string filename = output_prefix + ".forest";
  std::ofstream outfile;
  outfile.open(filename, std::ios::binary);
  if (!outfile.good()) {
    throw std::runtime_error("Could not write to output file: " + filename + ".");
  }

  // Write dependent_varID
  outfile.write((char*) &dependent_varID, sizeof(dependent_varID));

  // Write num_trees
  outfile.write((char*) &num_trees, sizeof(num_trees));

  // Write is_ordered_variable
  saveVector1D(data->getIsOrderedVariable(), outfile);

  saveToFileInternal(outfile);

  // Write tree data for each tree
  for (auto& tree : trees) {
    tree->appendToFile(outfile);
  }

  // Close file
  outfile.close();
  if (verbose_out)
    *verbose_out << "Saved forest to file " << filename << "." << std::endl;
}
// #nocov end

void Forest::grow() {

  // Create thread ranges
  equalSplit(thread_ranges, 0, num_trees - 1, num_threads);

  // Call special grow functions of subclasses. There trees must be created.
  growInternal();

  // Init trees, create a seed for each tree, based on main seed
  std::uniform_int_distribution<uint> udist;
  for (size_t i = 0; i < num_trees; ++i) {
    uint tree_seed;
    if (seed == 0) {
      tree_seed = udist(random_number_generator);
    } else {
      tree_seed = (i + 1) * seed;
    }

    // Get split select weights for tree
    std::vector<double>* tree_split_select_weights;
    if (split_select_weights.size() > 1) {
      tree_split_select_weights = &split_select_weights[i];
    } else {
      tree_split_select_weights = &split_select_weights[0];
    }

    // Get inbag counts for tree
    std::vector<size_t>* tree_manual_inbag;
    if (manual_inbag.size() > 1) {
      tree_manual_inbag = &manual_inbag[i];
    } else {
      tree_manual_inbag = &manual_inbag[0];
    }

    trees[i]->init(data.get(), mtry, nsplits, npairs, proptry, dependent_varID, num_samples, tree_seed, &deterministic_varIDs,
        &split_select_varIDs, tree_split_select_weights, importance_mode, min_node_size, sample_with_replacement,
        memory_saving_splitting, splitrule, &case_weights, tree_manual_inbag, keep_inbag, &sample_fraction, alpha,
        minprop, holdout, num_random_splits, max_depth, &promispairs, eim_mode, divfortype);
  }

// Init variable importance
  variable_importance.resize(num_independent_variables, 0);

// Grow trees in multiple threads
  progress = 0;
#ifdef R_BUILD
  aborted = false;
  aborted_threads = 0;
#endif

  std::vector<std::thread> threads;
  threads.reserve(num_threads);

// Initialize importance per thread
  std::vector<std::vector<double>> variable_importance_threads(num_threads);

  for (uint i = 0; i < num_threads; ++i) {
    if (importance_mode == IMP_GINI || importance_mode == IMP_GINI_CORRECTED) {
      variable_importance_threads[i].resize(num_independent_variables, 0);
    }
    threads.emplace_back(&Forest::growTreesInThread, this, i, &(variable_importance_threads[i]));
  }
  showProgress("Growing trees..", num_trees);
  for (auto &thread : threads) {
    thread.join();
  }

#ifdef R_BUILD
  if (aborted_threads > 0) {
    throw std::runtime_error("User interrupt.");
  }
#endif

  // Sum thread importances
  if (importance_mode == IMP_GINI || importance_mode == IMP_GINI_CORRECTED) {
    variable_importance.resize(num_independent_variables, 0);
    for (size_t i = 0; i < num_independent_variables; ++i) {
      for (uint j = 0; j < num_threads; ++j) {
        variable_importance[i] += variable_importance_threads[j][i];
      }
    }
    variable_importance_threads.clear();
  }

// Divide importance by number of trees
  if (importance_mode == IMP_GINI || importance_mode == IMP_GINI_CORRECTED) {
    for (auto& v : variable_importance) {
      v /= num_trees;
    }
  }
}

void Forest::predict() {

// Predict trees in multiple threads and join the threads with the main thread
  progress = 0;
#ifdef R_BUILD
  aborted = false;
  aborted_threads = 0;
#endif

  // Predict
  std::vector<std::thread> threads;
  threads.reserve(num_threads);
  for (uint i = 0; i < num_threads; ++i) {
    threads.emplace_back(&Forest::predictTreesInThread, this, i, data.get(), false);
  }
  showProgress("Predicting..", num_trees);
  for (auto &thread : threads) {
    thread.join();
  }

  // Aggregate predictions
  allocatePredictMemory();
  threads.clear();
  threads.reserve(num_threads);
  progress = 0;
  for (uint i = 0; i < num_threads; ++i) {
    threads.emplace_back(&Forest::predictInternalInThread, this, i);
  }
  showProgress("Aggregating predictions..", num_samples);
  for (auto &thread : threads) {
    thread.join();
  }

#ifdef R_BUILD
  if (aborted_threads > 0) {
    throw std::runtime_error("User interrupt.");
  }
#endif
}

void Forest::computePredictionError() {

// Predict trees in multiple threads
  std::vector<std::thread> threads;
  threads.reserve(num_threads);
  progress = 0;
  for (uint i = 0; i < num_threads; ++i) {
    threads.emplace_back(&Forest::predictTreesInThread, this, i, data.get(), true);
  }
  showProgress("Computing prediction error..", num_trees);
  for (auto &thread : threads) {
    thread.join();
  }

#ifdef R_BUILD
  if (aborted_threads > 0) {
    throw std::runtime_error("User interrupt.");
  }
#endif

  // Call special function for subclasses
  computePredictionErrorInternal();
}

void Forest::computePermutationImportance() {

// Compute tree permutation importance in multiple threads
  progress = 0;
#ifdef R_BUILD
  aborted = false;
  aborted_threads = 0;
#endif

  std::vector<std::thread> threads;
  threads.reserve(num_threads);

// Initailize importance and variance
  std::vector<std::vector<double>> variable_importance_threads(num_threads);
  std::vector<std::vector<double>> variance_threads(num_threads);

// Compute importance
  for (uint i = 0; i < num_threads; ++i) {
    variable_importance_threads[i].resize(num_independent_variables, 0);
    if (importance_mode == IMP_PERM_BREIMAN || importance_mode == IMP_PERM_LIAW) {
      variance_threads[i].resize(num_independent_variables, 0);
    }
    threads.emplace_back(&Forest::computeTreePermutationImportanceInThread, this, i,
        std::ref(variable_importance_threads[i]), std::ref(variance_threads[i]));
  }
  showProgress("Computing permutation importance..", num_trees);
  for (auto &thread : threads) {
    thread.join();
  }
  
#ifdef R_BUILD
  if (aborted_threads > 0) {
    throw std::runtime_error("User interrupt.");
  }
#endif

// Sum thread importances
  variable_importance.resize(num_independent_variables, 0);
  for (size_t i = 0; i < num_independent_variables; ++i) {
    for (uint j = 0; j < num_threads; ++j) {
      variable_importance[i] += variable_importance_threads[j][i];
    }
  }
  variable_importance_threads.clear();

// Sum thread variances
  std::vector<double> variance(num_independent_variables, 0);
  if (importance_mode == IMP_PERM_BREIMAN || importance_mode == IMP_PERM_LIAW) {
    for (size_t i = 0; i < num_independent_variables; ++i) {
      for (uint j = 0; j < num_threads; ++j) {
        variance[i] += variance_threads[j][i];
      }
    }
    variance_threads.clear();
  }
  
  for (size_t i = 0; i < variable_importance.size(); ++i) {
    variable_importance[i] /= num_trees;

    // Normalize by variance for scaled permutation importance
    if (importance_mode == IMP_PERM_BREIMAN || importance_mode == IMP_PERM_LIAW) {
      if (variance[i] != 0) {
        variance[i] = variance[i] / num_trees - variable_importance[i] * variable_importance[i];
        variable_importance[i] /= sqrt(variance[i] / num_trees);
      }
    }
  }
  
}

void Forest::computePermutationImportanceMultivariate()
{

  // Compute EIM values in multiple threads
  progress = 0;
#ifdef R_BUILD
  aborted = false;
  aborted_threads = 0;
#endif

  std::vector<std::thread> threads;
  threads.reserve(num_threads);

  // Initialize importance
  std::vector<std::vector<double>> eim_univ_threads(num_threads);
  std::vector<std::vector<double>> eim_bivpooled_threads(num_threads);
  std::vector<std::vector<double>> eim_bivqual_threads(num_threads);
  std::vector<std::vector<double>> eim_bivquant_ll_threads(num_threads);
  std::vector<std::vector<double>> eim_bivquant_lh_threads(num_threads);
  std::vector<std::vector<double>> eim_bivquant_hl_threads(num_threads);
  std::vector<std::vector<double>> eim_bivquant_hh_threads(num_threads);

  // Compute importance
  for (uint i = 0; i < num_threads; ++i)
  {
    eim_univ_threads[i].resize(num_independent_variables, 0);

    if (eim_mode != 5)
    {
      if (eim_mode == 1)
      {
        eim_bivpooled_threads[i].resize(promispairs.size(), 0);
      }
      if (eim_mode == 2 || eim_mode == 3)
      {
        eim_bivqual_threads[i].resize(promispairs.size(), 0);
      }
      if (eim_mode == 2 || eim_mode == 4)
      {
        eim_bivquant_ll_threads[i].resize(promispairs.size(), 0);
        eim_bivquant_lh_threads[i].resize(promispairs.size(), 0);
        eim_bivquant_hl_threads[i].resize(promispairs.size(), 0);
        eim_bivquant_hh_threads[i].resize(promispairs.size(), 0);
      }
    }
    threads.emplace_back(&Forest::computeTreePermutationImportanceMultivariateInThread, this, i,
                         std::ref(eim_univ_threads[i]), std::ref(eim_bivpooled_threads[i]),
                         std::ref(eim_bivqual_threads[i]),
                         std::ref(eim_bivquant_ll_threads[i]), std::ref(eim_bivquant_lh_threads[i]),
                         std::ref(eim_bivquant_hl_threads[i]), std::ref(eim_bivquant_hh_threads[i]));
  }
  showProgress("Computing EIM values..", num_trees);
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
  eim_univ.resize(num_independent_variables, 0);
  for (size_t i = 0; i < num_independent_variables; ++i)
  {
    for (uint j = 0; j < num_threads; ++j)
    {
      eim_univ[i] += eim_univ_threads[j][i];
    }
  }

  if (eim_mode != 5)
  {
    if (eim_mode == 1)
    {
      eim_bivpooled.resize(promispairs.size(), 0);
      for (size_t i = 0; i < promispairs.size(); ++i)
      {
        for (uint j = 0; j < num_threads; ++j)
        {
          eim_bivpooled[i] += eim_bivpooled_threads[j][i];
        }
      }
    }
    if (eim_mode == 2 || eim_mode == 3)
    {
      eim_bivqual.resize(promispairs.size(), 0);
      for (size_t i = 0; i < promispairs.size(); ++i)
      {
        for (uint j = 0; j < num_threads; ++j)
        {
          eim_bivqual[i] += eim_bivqual_threads[j][i];
        }
      }
    }
    if (eim_mode == 2 || eim_mode == 4)
    {
      eim_bivquant_ll.resize(promispairs.size(), 0);
      eim_bivquant_lh.resize(promispairs.size(), 0);
      eim_bivquant_hl.resize(promispairs.size(), 0);
      eim_bivquant_hh.resize(promispairs.size(), 0);
      for (size_t i = 0; i < promispairs.size(); ++i)
      {
        for (uint j = 0; j < num_threads; ++j)
        {
          eim_bivquant_ll[i] += eim_bivquant_ll_threads[j][i];
          eim_bivquant_lh[i] += eim_bivquant_lh_threads[j][i];
          eim_bivquant_hl[i] += eim_bivquant_hl_threads[j][i];
          eim_bivquant_hh[i] += eim_bivquant_hh_threads[j][i];
        }
      }
    }
  }

  eim_univ_threads.clear();
  eim_bivpooled_threads.clear();
  eim_bivqual_threads.clear();
  eim_bivquant_ll_threads.clear();
  eim_bivquant_lh_threads.clear();
  eim_bivquant_hl_threads.clear();
  eim_bivquant_hh_threads.clear();

  for (size_t i = 0; i < eim_univ.size(); ++i)
  {
    eim_univ[i] /= num_trees;
  }
  if (eim_mode != 5)
  {

    if (eim_mode == 1)
    {
      for (size_t i = 0; i < eim_bivpooled.size(); ++i)
      {
        eim_bivpooled[i] /= num_trees;
      }
    }
    if (eim_mode == 2 || eim_mode == 3)
    {
      for (size_t i = 0; i < eim_bivqual.size(); ++i)
      {
        eim_bivqual[i] /= num_trees;
      }
    }
    if (eim_mode == 2 || eim_mode == 4)
    {
      for (size_t i = 0; i < eim_bivquant_ll.size(); ++i)
      {
        eim_bivquant_ll[i] /= num_trees;
        eim_bivquant_lh[i] /= num_trees;
        eim_bivquant_hl[i] /= num_trees;
        eim_bivquant_hh[i] /= num_trees;
      }
    }
  }
}

void Forest::computeImportanceMuw() {
	// Empty on purpose (virtual function only implemented in classification and probability)
}

void Forest::growTreesInThread(uint thread_idx, std::vector<double>* variable_importance) {
  if (thread_ranges.size() > thread_idx + 1) {
    for (size_t i = thread_ranges[thread_idx]; i < thread_ranges[thread_idx + 1]; ++i) {
      trees[i]->grow(variable_importance);

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

void Forest::predictTreesInThread(uint thread_idx, const Data* prediction_data, bool oob_prediction) {
  if (thread_ranges.size() > thread_idx + 1) {
    for (size_t i = thread_ranges[thread_idx]; i < thread_ranges[thread_idx + 1]; ++i) {
	  if (divfortype == 1) {
	      trees[i]->predict(prediction_data, oob_prediction);
	  }
	  if (divfortype == 2) {
  		  trees[i]->predictMultivariate(prediction_data, oob_prediction);
	  }
	  if (divfortype == 3) {
  		  trees[i]->predictMuw(prediction_data, oob_prediction);
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

void Forest::predictInternalInThread(uint thread_idx) {
  // Create thread ranges
  std::vector<uint> predict_ranges;
  equalSplit(predict_ranges, 0, num_samples - 1, num_threads);

  if (predict_ranges.size() > thread_idx + 1) {
    for (size_t i = predict_ranges[thread_idx]; i < predict_ranges[thread_idx + 1]; ++i) {
      predictInternal(i);

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

void Forest::computeTreePermutationImportanceInThread(uint thread_idx, std::vector<double>& importance,
    std::vector<double>& variance) {
  if (thread_ranges.size() > thread_idx + 1) {
    for (size_t i = thread_ranges[thread_idx]; i < thread_ranges[thread_idx + 1]; ++i) {
      trees[i]->computePermutationImportance(importance, variance);

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

void Forest::computeTreePermutationImportanceMultivariateInThread(uint thread_idx, std::vector<double>& importanceuniv,
     std::vector<double>& importancebivpooled, std::vector<double>& importancebivqual,
	 std::vector<double>& importancebivquant_ll, std::vector<double>& importancebivquant_lh, std::vector<double>& importancebivquant_hl,
	 std::vector<double>& importancebivquant_hh) {
  if (thread_ranges.size() > thread_idx + 1) {
    for (size_t i = thread_ranges[thread_idx]; i < thread_ranges[thread_idx + 1]; ++i) {
      trees[i]->computePermutationImportanceMultivariate(importanceuniv, importancebivpooled, importancebivqual, 
	    importancebivquant_ll, importancebivquant_lh, importancebivquant_hl, importancebivquant_hh);

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

void Forest::setSplitWeightVector(std::vector<std::vector<double>>& split_select_weights) {

// Size should be 1 x num_independent_variables or num_trees x num_independent_variables
  if (split_select_weights.size() != 1 && split_select_weights.size() != num_trees) {
    throw std::runtime_error("Size of split select weights not equal to 1 or number of trees.");
  }

// Reserve space
  size_t num_weights = num_independent_variables;
  if (importance_mode == IMP_GINI_CORRECTED) {
    num_weights = 2 * num_independent_variables;
  }
  if (split_select_weights.size() == 1) {
    this->split_select_weights[0].resize(num_weights);
  } else {
    this->split_select_weights.clear();
    this->split_select_weights.resize(num_trees, std::vector<double>(num_weights));
  }
  this->split_select_varIDs.resize(num_weights);
  deterministic_varIDs.reserve(num_weights);

  // Split up in deterministic and weighted variables, ignore zero weights
  size_t num_zero_weights = 0;
  for (size_t i = 0; i < split_select_weights.size(); ++i) {

    // Size should be 1 x num_independent_variables or num_trees x num_independent_variables
    if (split_select_weights[i].size() != num_independent_variables) {
      throw std::runtime_error("Number of split select weights not equal to number of independent variables.");
    }

    for (size_t j = 0; j < split_select_weights[i].size(); ++j) {
      double weight = split_select_weights[i][j];

      if (i == 0) {
        size_t varID = j;
        for (auto& skip : data->getNoSplitVariables()) {
          if (varID >= skip) {
            ++varID;
          }
        }

        if (weight == 1) {
          deterministic_varIDs.push_back(varID);
        } else if (weight < 1 && weight > 0) {
          this->split_select_varIDs[j] = varID;
          this->split_select_weights[i][j] = weight;
        } else if (weight == 0) {
          ++num_zero_weights;
        } else if (weight < 0 || weight > 1) {
          throw std::runtime_error("One or more split select weights not in range [0,1].");
        }

      } else {
        if (weight < 1 && weight > 0) {
          this->split_select_weights[i][j] = weight;
        } else if (weight < 0 || weight > 1) {
          throw std::runtime_error("One or more split select weights not in range [0,1].");
        }
      }
    }

    // Copy weights for corrected impurity importance
    if (importance_mode == IMP_GINI_CORRECTED) {
      std::vector<double>* sw = &(this->split_select_weights[i]);
      std::copy_n(sw->begin(), num_independent_variables, sw->begin() + num_independent_variables);

      for (size_t k = 0; k < num_independent_variables; ++k) {
        split_select_varIDs[num_independent_variables + k] = num_variables + k;
      }

      size_t num_deterministic_varIDs = deterministic_varIDs.size();
      for (size_t k = 0; k < num_deterministic_varIDs; ++k) {
        size_t varID = deterministic_varIDs[k];
        for (auto& skip : data->getNoSplitVariables()) {
          if (varID >= skip) {
            --varID;
          }
        }
        deterministic_varIDs.push_back(varID + num_variables);
      }
    }
  }

  if (num_weights - deterministic_varIDs.size() - num_zero_weights < mtry) {
    throw std::runtime_error("Too many zeros or ones in split select weights. Need at least mtry variables to split at.");
  }
}

void Forest::setAlwaysSplitVariables(const std::vector<std::string>& always_split_variable_names) {

  deterministic_varIDs.reserve(num_independent_variables);

  for (auto& variable_name : always_split_variable_names) {
    size_t varID = data->getVariableID(variable_name);
    deterministic_varIDs.push_back(varID);
  }

  if (deterministic_varIDs.size() + this->mtry > num_independent_variables) {
    throw std::runtime_error(
        "Number of variables to be always considered for splitting plus mtry cannot be larger than number of independent variables.");
  }

  // Also add variables for corrected impurity importance
  if (importance_mode == IMP_GINI_CORRECTED) {
    size_t num_deterministic_varIDs = deterministic_varIDs.size();
    for (size_t k = 0; k < num_deterministic_varIDs; ++k) {
      size_t varID = deterministic_varIDs[k];
      for (auto& skip : data->getNoSplitVariables()) {
        if (varID >= skip) {
          --varID;
        }
      }
      deterministic_varIDs.push_back(varID + num_variables);
    }
  }
}

void Forest::showProgress(std::string operation, size_t max_progress) {
  using std::chrono::steady_clock;
  using std::chrono::duration_cast;
  using std::chrono::seconds;

  steady_clock::time_point start_time = steady_clock::now();
  steady_clock::time_point last_time = steady_clock::now();
  std::unique_lock<std::mutex> lock(mutex);

// Wait for message from threads and show output if enough time elapsed
  while (progress < max_progress) {
    condition_variable.wait(lock);
    seconds elapsed_time = duration_cast<seconds>(steady_clock::now() - last_time);

    // Check for user interrupt
#ifdef R_BUILD
    if (!aborted && checkInterrupt()) {
      aborted = true;
    }
    if (aborted && aborted_threads >= num_threads) {
      return;
    }
#endif

    if (progress > 0 && elapsed_time.count() > STATUS_INTERVAL) {
      double relative_progress = (double) progress / (double) max_progress;
      seconds time_from_start = duration_cast<seconds>(steady_clock::now() - start_time);
      uint remaining_time = (1 / relative_progress - 1) * time_from_start.count();
      if (verbose_out) {
        *verbose_out << operation << " Progress: " << round(100 * relative_progress) << "%. Estimated remaining time: "
            << beautifyTime(remaining_time) << "." << std::endl;
      }
      last_time = steady_clock::now();
    }
  }
}

} // namespace diversityForest
