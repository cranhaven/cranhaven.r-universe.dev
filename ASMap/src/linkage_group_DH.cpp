/*
 *  linkage_group_DH.cpp
 *  ApproxMap
 *
 *  Created by yonghui on 4/9/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "linkage_group_DH.h"
#include <limits>

bool cmp(pair<double, pair<int,int> > element1, pair<double, pair<int,int> > element2)
{
    return element1.first < element2.first;
}


const vector<vector<double> >& linkage_group::get_pair_wise_distance() const
{
    return pair_wise_distances;
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


void linkage_group::generate_distance_in_cM(vector<vector<double> >& distance_in_cM){
    distance_in_cM.resize(number_of_bins);
    for (int ii = 0; ii < number_of_bins; ii++) {
        distance_in_cM[ii].resize(number_of_bins);
    }
    for (int ii = 0; ii < number_of_bins; ii++) {
        for (int jj = 0; jj < number_of_bins; jj++) {
            double r = pair_wise_distances[ii][jj] / number_of_individuals;
            if (r >= 0.5) {
                r = r - ZERO_PLUS;
            }
            distance_in_cM[ii][jj] = df->CM(r);
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group::generate_distance_in_ML(vector<vector<double> >& distance_in_ML){
    distance_in_ML.resize(number_of_bins);
    for (int ii = 0; ii < number_of_bins; ii++) {
        distance_in_ML[ii].resize(number_of_bins);
    }
    for (int ii = 0; ii < number_of_bins; ii++) {
        for (int jj = 0; jj < number_of_bins; jj++) {
            double r = pair_wise_distances[ii][jj] / number_of_individuals;
            if (r >= 0.5) {
                r = r - ZERO_PLUS;
            }
            if (r == 0.0) {
                distance_in_ML[ii][jj] = 0.0;
            } else {
                distance_in_ML[ii][jj] = -(r * log(r) + (1 - r) * log(1 - r));
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group::dump_common() const {

  Rprintf("number of bins: %d\n",number_of_bins);
  Rprintf("number of individuals: %d\n", number_of_individuals);
  Rprintf("current_order:\n");
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      Rprintf("%d,",current_order[ii]);
    }
  Rprintf("\n");;

  Rprintf("lowerbound: %f the upperbound: %f\n",
	  MST_lower_bound,current_upper_bound);

  /*Print out the information regarding the MST*/
  Rprintf("The MST:\n");
  for (int ii = 0 ; ii < number_of_bins ; ii++)
    {
      Rprintf("%d,", MST[ii]);
    }

  vector<int> tmp_count(number_of_bins, 0);
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      tmp_count[MST[ii]] = tmp_count[MST[ii]] + 1;
    }
  Rprintf("\n");

  Rprintf("The indegree for each of the vertices:\n");
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      Rprintf("%d,", tmp_count[ii]);
    }

  Rprintf("\n");

  Rprintf("df function:");
  df->print_df_name();
  Rprintf("\n");
  Rprintf("the distance between consecutive pairs:\n");
  for (int ii = 1 ; ii < number_of_bins; ii++)
    {
      Rprintf("%f,", pair_wise_distances[current_order[ii]][current_order[ii-1]]);
    }
  Rprintf("\n");
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group::bad_genotypes(vector<pair<int,int> >& bad_genotypes) const{
    bad_genotypes.clear();
    for(unsigned int ii = 0; ii < suspicious_data.size(); ii++) {
        bad_genotypes.push_back(suspicious_data[ii]);
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group::dump_distance_matrix() {
  char buffer[10];
  Rprintf("distance matrix within linkage_group\n");
  Rprintf("matrix dimension: %zu\n", pair_wise_distances.size());
  for (unsigned int ii = 0; ii < pair_wise_distances.size(); ii++) {
    for (unsigned int jj = 0; jj < pair_wise_distances[ii].size(); jj++) {
      snprintf(buffer, 10, "%.2f ", pair_wise_distances[ii][jj]);
      Rprintf("%s",buffer);
    }
    Rprintf("\n");
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group::return_order(vector<int>& out_order,
                                 double & _lowerbound,
                                 double & _upper_bound,
                                 double & _cost_after_initialization,
                                 vector<double> & _distances) const {
    out_order = current_order;
    _lowerbound = MST_lower_bound;
    _upper_bound = current_upper_bound;
    _cost_after_initialization = cost_after_initialization;
    _distances.clear();
    _distances.resize(number_of_bins-1);
    for (int ii = 1 ; ii < number_of_bins; ii++)
    {
        _distances[ii-1] = df->CM(pair_wise_distances[current_order[ii]][current_order[ii-1]] /number_of_individuals);
    }

}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

linkage_group_DH::linkage_group_DH(int _number_of_bins,
                                   int _number_of_individuals,
                                   bool _detect_bad_data,
                                   ObjFunc _objective_function,
                                   DF* _df,
                                   const vector<vector<double> > & _raw_data,
                                   const vector<int> & _current_order,
                                   const vector<pair<int, int> > & _missing_data,
                                   const vector<int>& _bin_sizes) {
    number_of_bins = _number_of_bins;
    number_of_individuals = _number_of_individuals;
    detect_bad_data = _detect_bad_data;
    objective_function = _objective_function;
    raw_data = _raw_data;
    current_order = _current_order;
    missing_data = _missing_data;
    bin_sizes = _bin_sizes;
    df = _df;
    /*perform some consistency check*/
    if (raw_data.size() != (unsigned int)number_of_bins) {
      Rprintf("BAD DATA\n");
    }

    pair_wise_distances.resize(number_of_bins);
    for (int ii = 0; ii < number_of_bins; ii++) {
        (pair_wise_distances[ii]).resize(number_of_bins);
    }

    /*
        added by yonghui on Mar 13
        initialize the data_status vector
    */
    iteration_number = 2; // it is initialized to be 2
    data_status.clear();
    data_status.resize(number_of_bins);
    for (int ii = 0; ii < number_of_bins; ii++) {
        data_status[ii].resize(number_of_individuals);
    }
    for (int ii = 0; ii < number_of_bins; ii++) {
        for (int jj = 0; jj < number_of_individuals; jj++) {
            data_status[ii][jj] = 0;
        }
    }
    for (unsigned int ii = 0; ii < _missing_data.size(); ii++) {
        int marker_id = _missing_data[ii].first;
        int individual_id = _missing_data[ii].second;
        data_status[marker_id][individual_id] = 1;
    }

    /* Calculate the pair-wise distance*/
    /* At the begining, we only rely on known genotype calls to estimate the distance between markers*/
    /* The reason for doing so is that if we interpret missing genotype calls as .5 A and .5 B
       the distance will be unncessary amplified.
       For example, let's assume two markers are identical except for those missing calls. Let's further assume
       that each one has about 10% missing. Intuitively, the two markers are very similar, but
       using calculate_pair_wise_distance function, the distance between the two markers is about 20cM.
       The procedure that follows will try to place the two markers at the distancre of 20cM, and as a result,
       the iterative estimation procedure will likely to stuck in the local optima, and won't be able to
       correclty estimate the missing call */
    calculate_pair_wise_distance_initialize();

    current_upper_bound = 0 ;
    for (int ii = 1 ; ii < number_of_bins; ii++) {
        current_upper_bound = current_upper_bound + pair_wise_distances[current_order[ii-1]][current_order[ii]];
    }
    cost_after_initialization = 0 ;
    MST_lower_bound = 0;
    suspicious_data.clear();
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

linkage_group_DH::~linkage_group_DH()
{

}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


int linkage_group_DH::detect_bad_markers(){
  extern int trace;

  int total_bad_this_iter = 0;
  if (number_of_bins < 3) { // the size of the LG is too small
    return total_bad_this_iter;
  }

  double mask_threshold = kMaskThreshold - kMaskDecrement * (iteration_number - 3);
  if (mask_threshold < kMinMaskThreshold) {
    mask_threshold = kMinMaskThreshold;
  }

  for (int ii = 0; ii < number_of_bins; ii++) {
    if (bin_sizes[ii] > 1) { // skip those bins which represent multiple markers
      // those bins are very unlikely to have bad data
      continue;
    }
    // for each marker, identify the at most kBadDetMaxNum closest markers to it
    vector<pair<double, int> > distances;
    for (int jj = 0; jj < number_of_bins; jj++) {
      if (ii != jj) {
	distances.push_back(make_pair(pair_wise_distances[ii][jj], jj));
      }
    }
    if(distances.size() != ((unsigned int)number_of_bins - 1))
      Rf_error("distances.size() != (number_of_bins - 1)\n");
    //assert(distances.size() == (number_of_bins - 1));
    sort(distances.begin(), distances.end());
    if(distances[0].first > distances[1].first)
      Rf_error("distances[0].first > distances[1].first\n");
    //assert(distances[0].first <= distances[1].first);
    int bad_det_max_num = kBadDetMaxNum;
    if (distances.size() < (unsigned int)kBadDetMaxNum) {
      bad_det_max_num = distances.size();
    }
    // now for every individual, test if it is a bad marker
    for (int jj = 0; jj < number_of_individuals; jj++) {
      if (data_status[ii][jj] != 0) {
	continue;
      }
      double total_prob = 0.0;
      double total_weight = 0.0;
      for (int kk = 0; kk < bad_det_max_num; kk++) {
	if(distances[kk].first > 0.0) {
	  total_prob = total_prob +
	    (1 / distances[kk].first) *
	    (1 / distances[kk].first) *
	    raw_data[distances[kk].second][jj] *
	    bin_sizes[distances[kk].second];
	  total_weight = total_weight +
	    (1 / distances[kk].first) *
	    (1 / distances[kk].first) *
	    bin_sizes[distances[kk].second];
	}
      }
      double p_estimate = 0.5;
      if (total_weight > 0.0) {
	p_estimate = total_prob / total_weight;
      }
      if (p_estimate > 1.0) {
	p_estimate = 1.0;
      }
      double p_diff = p_estimate - raw_data[ii][jj];
      if (p_diff < 0.0) {
	p_diff = - p_diff;
      }

      if (p_diff > mask_threshold) { // identified a new bad marker
	suspicious_data.push_back(make_pair(ii, jj));
	suspicious_data_backup.push_back(raw_data[ii][jj]);
	data_status[ii][jj] = iteration_number;
	total_bad_this_iter = total_bad_this_iter + 1;
      }
    }
  }
  if(trace) {
    Rprintf("mask threshold in this iteration: %f\n", mask_threshold);
    Rprintf("identified %d data points in this iteration\n", total_bad_this_iter);
  }
  return total_bad_this_iter;
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


void linkage_group_DH::revert_suspicious_data(){
  if(suspicious_data.size() != suspicious_data_backup.size())
    Rf_error("suspicious_data.size() != suspicious_data_backup.size()\n");
  //assert(suspicious_data.size() == suspicious_data_backup.size());
      for (unsigned int ii = 0; ii < suspicious_data.size(); ii++){
          int marker_id = suspicious_data[ii].first;
          int indi_id = suspicious_data[ii].second;
          raw_data[marker_id][indi_id] = suspicious_data_backup[ii];
      }
}

//////////////////////////////////////////////////////////////////////////////////////
void linkage_group_DH::calculate_pair_wise_distance()
{
  /*Calculate the pair-wise distance*/
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      for (int jj = ii ; jj < number_of_bins; jj++)
        {
	  pair_wise_distances[ii][jj]=0;
	  if (ii == jj)
            {
	      pair_wise_distances[ii][jj]=0;
            }
	  else
            {
	      for (int kk = 0 ; kk < number_of_individuals; kk++)
                {
		  if(raw_data[ii][kk] > 1.0)
		    Rf_error("raw_data[ii][kk] > 1.0\n");
		  if(raw_data[ii][kk] < 0.0)
		    Rf_error("raw_data[ii][kk] < 0.0\n");
		  if(raw_data[jj][kk] > 1.0)
		    Rf_error("raw_data[jj][kk] > 1.0\n");
		  if(raw_data[jj][kk] < 0.0)
		    Rf_error("raw_data[jj][kk] < 0.0\n");
		  // assert(raw_data[ii][kk] <= 1.0);
		  // assert(raw_data[ii][kk] >= 0.0);
		  // assert(raw_data[jj][kk] <= 1.0);
		  // assert(raw_data[jj][kk] >= 0.0);
		  pair_wise_distances[ii][jj] = pair_wise_distances[ii][jj] +
		    (1 - raw_data[ii][kk]) * raw_data[jj][kk] +
		    (1 - raw_data[jj][kk]) * raw_data[ii][kk];
                }
            }
	  pair_wise_distances[jj][ii] = pair_wise_distances[ii][jj];
        }
    }
}
//////////////////////////////////////////////////////////////////////////////////////

void linkage_group_DH::calculate_pair_wise_distance_initialize() {
    /*Calculate the pair-wise distance*/
    for (int ii = 0; ii < number_of_bins; ii++) {
        for (int jj = ii; jj < number_of_bins; jj++) {
            pair_wise_distances[ii][jj] = 0;
            double none_missing = 0;
            if (ii == jj) {
                pair_wise_distances[ii][jj]=0;
            } else {
                for (int kk = 0 ; kk < number_of_individuals; kk++) {
                    if ((data_status[ii][kk] == 0) and (data_status[jj][kk]) == 0) {
                        none_missing = none_missing + 1.0;
                        pair_wise_distances[ii][jj] = pair_wise_distances[ii][jj] +
                            (1 - raw_data[ii][kk]) * raw_data[jj][kk] +
                            (1 - raw_data[jj][kk]) * raw_data[ii][kk];
                    }
                }
                if (none_missing > 0.0) {
                    pair_wise_distances[ii][jj] = pair_wise_distances[ii][jj] / none_missing * number_of_individuals;
                } else {
		  Rprintf("caution, too many missing calls\n");
		  pair_wise_distances[ii][jj] = number_of_individuals / 2.0;
                }
            }
            pair_wise_distances[jj][ii] = pair_wise_distances[ii][jj];
        }
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group_DH::dump(SEXP &map) const {
  double *x;
  extern int trace;

  if(trace) {
  dump_common();
  Rprintf("The raw data ordered\n");
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      int jj = current_order[ii];
      for (int kk = 0 ; kk < number_of_individuals ; kk++)
        {
	  if (raw_data[jj][kk] > 0.5)
            {
	      Rprintf(".");
            }
	  else if (raw_data[jj][kk] < 0.5)
            {
	      Rprintf("#");
            }
	  else
            {
	      Rprintf("-");
            }
        }
      Rprintf("\n");
    }
  Rprintf("Imputed values (ordered)\n");
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      int jj = current_order[ii];
      for (int kk = 0 ; kk < number_of_individuals ; kk++)
        {
	  Rprintf(" %4.2f",raw_data[jj][kk]);
        }
      Rprintf("\n");
    }
  }
  // labels in data order
  SET_VECTOR_ELT(map, 1, Rf_allocMatrix(REALSXP, number_of_bins,number_of_individuals ));
  x = REAL(VECTOR_ELT(map,1));
  for (int ii = 0 ; ii < number_of_bins; ii++)
    {
      // data order //int jj = current_order[ii];
      for (int kk = 0 ; kk < number_of_individuals ; kk++)
        {
	  x[kk*number_of_bins+ii] = raw_data[ii][kk];
        }
    }
}
//////////////////////////////////////////////////////////////////////////////////////

void linkage_group_DH::estimate_missing_data(){
    if (number_of_bins < 3) { // the size of the LG is too small
        return;
    }
    for (int ii = 0; ii < number_of_bins; ii++) {
        // for each marker, identify the at most kBadDetMaxNum closest markers to it
        vector<pair<double, int> > distances;
        for (int jj = 0; jj < number_of_bins; jj++) {
            if (ii != jj) {
                distances.push_back(make_pair(pair_wise_distances[ii][jj], jj));
            }
        }
	if(distances.size() != ((unsigned int)number_of_bins - 1))
	 Rf_error("distances.size() != (number_of_bins - 1)\n");
       //assert(distances.size() == (number_of_bins - 1));
        sort(distances.begin(), distances.end());
        if(distances[0].first > distances[1].first)
	  Rf_error("distances[0].first > distances[1].first\n");
        //assert(distances[0].first <= distances[1].first);
        int bad_det_max_num = kBadDetMaxNum;
        if (distances.size() < (unsigned int)kBadDetMaxNum) {
            bad_det_max_num = distances.size();
        }
        // now for every missing data, estimate its probability
        for (int jj = 0; jj < number_of_individuals; jj++) {
            if (data_status[ii][jj] == 0) {
                continue;
            }
            double total_prob = 0.0;
            double total_weight = 0.0;
            for (int kk = 0; kk < bad_det_max_num; kk++) {
                if(distances[kk].first > 0.0) {
                    total_prob = total_prob +
                                 (1 / distances[kk].first) *
                                 (1 / distances[kk].first) *
                                 raw_data[distances[kk].second][jj] *
                                 bin_sizes[distances[kk].second];
                    total_weight = total_weight +
                                   (1 / distances[kk].first) *
                                   (1 / distances[kk].first) *
                                   bin_sizes[distances[kk].second];
                }
            }
            double p_estimate = 0.5;
            if (total_weight > 0.0) {
                p_estimate = total_prob / total_weight;
            }
            if (p_estimate > 1.0) {
                p_estimate = 1.0;
            }
            raw_data[ii][jj] = p_estimate;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void linkage_group_DH::order_markers() {
  extern int trace;

  if(trace) Rprintf("order markers version 2\n");
  unsigned int cumulative_errors = 0;
  double crt_number_of_errors = 0;
  calculate_pair_wise_distance_initialize();
  MSTOpt opt_iter_initial(pair_wise_distances, number_of_bins, 1);
  opt_iter_initial.Opt_Order(current_order,
			     MST,
			     MST_lower_bound,
			     current_upper_bound,
			     cost_after_initialization);
  crt_number_of_errors = current_upper_bound;
  if(trace) Rprintf("initial number of cross-overs: %f\n",crt_number_of_errors);
  bool one_more_iteration = true;
  while (one_more_iteration) {
    iteration_number = iteration_number + 1;
    int new_errors_detected = 0;
    if (detect_bad_data) {
      new_errors_detected = detect_bad_markers();
      cumulative_errors = cumulative_errors + new_errors_detected;
      if(cumulative_errors != suspicious_data.size())
	Rf_error("cumulative_errors != suspicious_data.size()\n");
      //assert(cumulative_errors == suspicious_data.size());
    }
    if ((missing_data.size() > 0) or (suspicious_data.size() > 0)) {
      estimate_missing_data();
    }
    calculate_pair_wise_distance();
    if (iteration_number >= kMaxErrorDectionRounds + 2) {one_more_iteration = false;}
    if (new_errors_detected == 0) {one_more_iteration = false;}
    MSTOpt opt_iter(pair_wise_distances, number_of_bins, 1);
    opt_iter.Opt_Order(current_order,
		       MST,
		       MST_lower_bound,
		       current_upper_bound,
		       cost_after_initialization);
    if(trace) Rprintf("current number of errors plus cross-overs: %f\n",
		      current_upper_bound + suspicious_data.size());
    if(current_upper_bound + suspicious_data.size() < crt_number_of_errors) {
      crt_number_of_errors = current_upper_bound + suspicious_data.size();
    } else {
      one_more_iteration = false;
    }

  }
  estimate_missing_data();
  calculate_pair_wise_distance();

  // call the MSTOPT sub-routine
  vector<vector<double> >  distance_to_optimize;

  if (objective_function == OBJF_ML) {
    generate_distance_in_ML(distance_to_optimize);
  } else if (objective_function == OBJF_CM) {
    generate_distance_in_cM(distance_to_optimize);
  } else {
    distance_to_optimize = pair_wise_distances;
  }

  MSTOpt opt_iter_final(distance_to_optimize, number_of_bins, 1);
  opt_iter_final.Opt_Order(current_order,
			   MST,
			   MST_lower_bound,
			   current_upper_bound,
			   cost_after_initialization);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
