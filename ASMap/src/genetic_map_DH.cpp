/*
 *  single_mapping_population_raw_data.cpp
 *  ApproxMap
 *
 *  Created by yonghui on 4/7/07.linkage_group_DH
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */
#define R_NO_REMAP
#include <string>
#include <sstream>
#include "genetic_map_DH.h"
#include <Rdefines.h>
#include <Rinternals.h>

SEXP ielem(SEXP list, int i);
SEXP elem(SEXP list, const char *str);
bool is_number(std::string& s);

genetic_map::genetic_map() {
    clustering_prob_cut_off = PROB_HOEFFDING_CUT_OFF;
    number_of_loci = 0;
    number_of_individual = 0;
    population_name = "mstmap-R";
    population_type = "";
    number_of_connected_components = 0;
    total_number_of_missing_obs = 0;
    objective_function = OBJF_COUNT;
    detect_bad_data = false;
}
//////////////////////////////////////////////////////////////////////////////

genetic_map::~genetic_map() {
    delete df_;
}

//////////////////////////////////////////////////////////////////////////////
int genetic_map::read_raw_mapping_data(SEXP &Plist, SEXP &data) {

  SEXP names = Rf_protect(Rf_getAttrib(data, R_NamesSymbol));
  SEXP row_names = Rf_protect(Rf_getAttrib(data, Rf_install("row.names")));
  string tmp_str;
  extern int trace;

  total_number_of_missing_obs = 0 ;

  //raw_mapping_data_file >> population_type;
  population_type = CHAR(STRING_ELT(elem(Plist, "population_type"),0));

  //raw_mapping_data_file >> population_name;
  //population_name = CHAR(STRING_ELT(elem(Plist, "population_name"),0));

  //raw_mapping_data_file >> distance_function;
  distance_function = CHAR(STRING_ELT(elem(Plist, "distance_function"),0));
  if (distance_function == HALDANE)
    df_ = new DF_Haldane();
  else if (distance_function == KOSAMBI)
    df_ = new DF_Kosambi();

  //raw_mapping_data_file >> clustering_prob_cut_off;
  clustering_prob_cut_off = REAL(elem(Plist, "cut_off_p_value"))[0];

  //raw_mapping_data_file >> no_map_dist;
  no_map_dist = REAL(elem(Plist, "no_map_dist"))[0];

  //raw_mapping_data_file >> no_map_size;
  no_map_size = INTEGER(elem(Plist, "no_map_size"))[0];

  //raw_mapping_data_file >> missing_threshold;
  missing_threshold = REAL(elem(Plist, "missing_threshold"))[0];

  estimation_before_clustering = (bool)INTEGER(elem(Plist, "estimation_before_clustering"))[0];
  detect_bad_data = (bool)INTEGER(elem(Plist, "detect_bad_data"))[0];

  tmp_str = CHAR(STRING_ELT(elem(Plist, "objective_function"),0));
  // objective function

  if (tmp_str == "ML") {
    objective_function = OBJF_ML;
  } else if (tmp_str == "COUNT") {
    objective_function = OBJF_COUNT;
  } else if (tmp_str == "CM") {
    objective_function = OBJF_CM;
  }
  //raw_mapping_data_file >> number_of_loci;
  number_of_loci = Rf_length(row_names);

  //raw_mapping_data_file >> number_of_individual;
  number_of_individual = Rf_length(names);

  // added by yonghui on Mar 7th
  // read in the individual names
  individual_names.resize(number_of_individual);
  for (int ii = 0; ii < number_of_individual; ii++)
    individual_names[ii] = CHAR(STRING_ELT(names,ii));

  int killed_markers = 0;
  vector<string> marker_data;
  marker_data.resize(number_of_individual);

  for (int ii = 0 ; ii < number_of_loci; ii ++) {
    int num_missing = 0;
    string marker_name_ii;
    marker_name_ii = CHAR(STRING_ELT(row_names,ii));
    for (int jj = 0 ; jj < number_of_individual ; jj++) {
      string SNP_jj;
      ostringstream sstr;
      //raw_mapping_data_file >> SNP_jj;
      if(!Rf_isNumeric(ielem(data,jj))) {
	SNP_jj = CHAR(STRING_ELT(ielem(data,jj),ii));
	if ((SNP_jj == "a") or (SNP_jj == "A")) { // homozygous A
	  marker_data[jj] = "A";
	}
	else if ((SNP_jj == "b") or (SNP_jj == "B")) { // homozygous B
	  marker_data[jj] = "B";
	}
	else if ((SNP_jj == "X") or (SNP_jj == "x") or (SNP_jj == "AB") or (SNP_jj == "ab")) { // heterozygous
	  marker_data[jj] = "X";
	}
	else if ((SNP_jj == "U") or (SNP_jj == "-") or (SNP_jj == "_") or (SNP_jj == "u")) { // missing allele
	  num_missing = num_missing + 1;
	  marker_data[jj] = "-";
	}
	else {
	Rf_error("unrecognzed marker at line  %d marker: %s   column %d\n",
		ii+1,marker_name_ii.c_str(),jj + 1);
	//assert(false); // crash the program on error
	UNPROTECT(2);
	return -1;
	}
      }
      else {
	sstr << REAL(Rf_coerceVector(ielem(data,jj),REALSXP))[ii];
	marker_data[jj] = sstr.str();
      }
    }
    if (num_missing < missing_threshold * number_of_individual) {
      raw_mapping_data.push_back(marker_data);
      marker_names.push_back(marker_name_ii);
      total_number_of_missing_obs = total_number_of_missing_obs + num_missing;
    }
    else {
      killed_markers = killed_markers + 1;
      Rprintf("caution! marker: %s was killed due to too many missing genotype calls\n",
	      marker_name_ii.c_str());
    }
  }
  if((int)number_of_loci != (int)raw_mapping_data.size() + killed_markers)
    Rf_error("number_of_loci != raw_mapping_data.size() + killed_markers\n");
  if(raw_mapping_data.size() != marker_names.size())
    Rf_error("raw_mapping_data.size() != marker_names.size()\n");
  //assert(number_of_loci == raw_mapping_data.size() + killed_markers);
  //assert(raw_mapping_data.size() == marker_names.size());
  number_of_loci = raw_mapping_data.size();
  if(trace) Rprintf("Found %d missing values\n",total_number_of_missing_obs);
  UNPROTECT(2);
  return 0;
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map::condense_markers_into_bins()
{
  vector<bool> visitted(number_of_loci, false);
  for (int ii = 0 ; ii < number_of_connected_components; ii++) {
    vector<vector<int> > bins_ii;
    for (unsigned int jj = 0 ; jj < (connected_components[ii]).size(); jj ++) {
      if (not visitted[connected_components[ii][jj]]) {
	vector<int> bin_ii_jj_markers;
	int kk = connected_components[ii][jj];
	bin_ii_jj_markers.push_back(kk);
	for (vector<int>::iterator iter1 = (connected_components[ii]).begin();
	     iter1 != (connected_components[ii]).end();
	     iter1++) {
	  if ((pair_wise_distances[kk][*iter1] <= ZERO_PLUS ) and (*iter1 != kk) and (not visitted[*iter1])) {
	    bin_ii_jj_markers.push_back(*iter1);
	  }
	}
	for (vector<int>::iterator iter1 = bin_ii_jj_markers.begin();
	     iter1 != bin_ii_jj_markers.end();
	     iter1++) {
	  visitted[*iter1] = true;
	}
	bins_ii.push_back(bin_ii_jj_markers);
      }
    }
    linkage_group_bins.push_back(bins_ii);
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void genetic_map::dump_distance_matrix() {
  char buffer[10];
  Rprintf("matrix dimension: %zu\n", pair_wise_distances.size());
  for (unsigned int ii = 0; ii < pair_wise_distances.size(); ii++) {
    for (unsigned int jj = 0; jj < pair_wise_distances[ii].size(); jj++) {
      snprintf(buffer, 10, "%.2f ", pair_wise_distances[ii][jj]);
      Rprintf("%s", buffer);
    }
    Rprintf("\n");
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map::dump()
{
  extern int trace;

  if(trace) {
    Rprintf("%s\n",population_name.c_str());
    Rprintf("%s\n",population_type.c_str());
    Rprintf("%s\n",distance_function.c_str());
    Rprintf("%d\n",number_of_loci);
    Rprintf("%d\n",number_of_individual);
    for (int ii = 0 ; ii < number_of_loci; ii ++) {
      for (int jj = 0 ; jj < number_of_individual; jj ++) {
	if (raw_mapping_data[ii][jj] == "A") {
	  Rprintf(".");
	} else if (raw_mapping_data[ii][jj] == "B") {
	  Rprintf("#");
	} else if (raw_mapping_data[ii][jj] == "X") {
	  Rprintf("X"); // heterozygous
	}
	else if(is_number(raw_mapping_data[ii][jj])) {
	  Rprintf("%4.1f",atof(raw_mapping_data[ii][jj].c_str()));
	} else {
	  Rprintf("-"); // heterozygous
	}

      }
      Rprintf("\n");
    }
    Rprintf("the number of connected components %d\n", number_of_connected_components);
    for (int ii = 0 ; ii < number_of_connected_components; ii++)
      {
	Rprintf("%zu,",(connected_components[ii]).size());
      }
    Rprintf("\n");
  }
  /*perform some consistency check*/

  /*1. Make sure the total number of markers in the linkage groups add up to number_of_loci*/
  int tmp_total = 0;
  for (int ii = 0 ; ii < number_of_connected_components; ii++)
    {
      tmp_total = tmp_total + (connected_components[ii]).size();
      unsigned int tmp_total_ii = 0 ;
      for (unsigned int jj = 0 ; jj < (linkage_group_bins[ii]).size(); jj++) {
	tmp_total_ii = tmp_total_ii + (linkage_group_bins[ii][jj]).size();
      }
      if (tmp_total_ii != (connected_components[ii]).size()) {
	Rprintf("ERROR, tmp_total_ii NOT equal to connected_components[ii]\n");
      }

    }
  if (tmp_total != number_of_loci)
    {
      Rprintf("ERROR, tmp_total NOT equal to number_of_loci\n");
    }

}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map::dump_connected_components_edges() {
  Rprintf("dump edges\n");
  double threshold = calculate_hoeffding_bound(clustering_prob_cut_off);
  Rprintf("calculate_hoeffding_bound: %f\n",threshold);
  for (int ii = 0; ii < number_of_connected_components; ii++) {
    Rprintf("==============================================\n");
    Rprintf("\t");
    vector<int> markers;
    for (unsigned int jj = 0; jj < linkage_group_bins[ii].size(); jj++) {
      markers.insert(markers.end(),
		     linkage_group_bins[ii][orders[ii][jj]].begin(),
		     linkage_group_bins[ii][orders[ii][jj]].end());
    }
    if(markers.size() != connected_components[ii].size())
      Rf_error("markers.size() != connected_components[ii].size()\n");
    //assert(markers.size() == connected_components[ii].size());
    for (unsigned int jj = 0; jj < markers.size(); jj++) {
      Rprintf("%s\t",marker_names[markers[jj]].c_str());
    }
    Rprintf("\n");
    for (unsigned int jj = 0; jj < markers.size(); jj++) {
      Rprintf("%s\t",marker_names[markers[jj]].c_str());
      for (unsigned int kk = 0; kk < markers.size(); kk++) {
	if (pair_wise_distances[markers[jj]][markers[kk]] < threshold) {
	  // Rprintf(pair_wise_distances[markers[jj]][markers[kk]];
	  Rprintf("#");
	} else {
	  Rprintf(".");
	}
	Rprintf("\t");
      }
      Rprintf("\n");
    }
  }

  for (int ii = 0; ii < number_of_connected_components; ii++) {
    Rprintf("==============================================\n");
    Rprintf("\t");
    vector<int> markers;
    for (unsigned int jj = 0; jj < linkage_group_bins[ii].size(); jj++) {
      markers.insert(markers.end(),
		     linkage_group_bins[ii][orders[ii][jj]].begin(),
		     linkage_group_bins[ii][orders[ii][jj]].end());
    }
    if(markers.size() != connected_components[ii].size())
      Rf_error("markers.size() != connected_components[ii].size()\n");
    //assert(markers.size() == connected_components[ii].size());
    for (unsigned int jj = 0; jj < markers.size(); jj++) {
      Rprintf("%s\t",marker_names[markers[jj]].c_str());
    }
    Rprintf("\n");
    for (unsigned int jj = 0; jj < markers.size(); jj++) {
      Rprintf("%s\t",marker_names[markers[jj]].c_str());
      for (unsigned int kk = 0; kk < markers.size(); kk++) {
	if (pair_wise_distances[markers[jj]][markers[kk]] < threshold) {
	  Rprintf("%f",pair_wise_distances[markers[jj]][markers[kk]]);
	} else {
	  Rprintf(".");
	}
	Rprintf("\t");
      }
      Rprintf("\n");
    }
  }

}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

double genetic_map::calculate_hoeffding_bound(double prob_cut_off) {
    double t;
    if (prob_cut_off >= 1)
    {
        t = numeric_limits<double>::max();
        return t;
    }
    else
    {
        t = sqrt((log(prob_cut_off)) / (-2*number_of_individual)); //according to the hoeffding bound
    }
    return number_of_individual*(0.5-t);
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int genetic_map::cluster() {
  extern int trace;

  double number_of_recombinations_cut_off  = calculate_hoeffding_bound(clustering_prob_cut_off);
  if(trace)
    Rprintf("number_of_recombinations_cut_off: %g\n",number_of_recombinations_cut_off);

  double no_map_threshold = number_of_individual * df_->RP(no_map_dist);
  set<int> un_mapped_markers;

  if (no_map_threshold < number_of_recombinations_cut_off) {
    // splice our those suspicious markers
    vector<bool> visitted;
    visitted.resize(number_of_loci);
    for (int ii = 0; ii < number_of_loci ; ii++) {
      visitted[ii] = false;
    }

    for (int ii = 0 ; ii < number_of_loci ; ii ++) {
      if (visitted[ii] == false) {//start a new connected component
	vector<int> crt_cc;
	queue<int> fifo_queue;
	fifo_queue.push(ii);
	while (not fifo_queue.empty()) {
	  int head = fifo_queue.front();
	  fifo_queue.pop();
	  if (visitted[head] == false) {
	    visitted[head] = true;
	    crt_cc.push_back(head);
	    for (int jj = 0 ; jj < number_of_loci; jj ++) {
	      if (pair_wise_distances[head][jj] < no_map_threshold) {
		fifo_queue.push(jj);
	      }
	    }
	  }
	}//end while
	if (crt_cc.size() <= (unsigned int)no_map_size) {
	  connected_components.push_back(crt_cc);
	  un_mapped_markers.insert(crt_cc.begin(), crt_cc.end());
	}
      }
    }
  }

  vector<bool> visitted;
  visitted.resize(number_of_loci);
  for (int ii = 0; ii < number_of_loci ; ii++)
    {
      visitted[ii] = false;
    }
  for (set<int>::iterator iter1 = un_mapped_markers.begin(); iter1 != un_mapped_markers.end(); ++iter1) {
    visitted[*iter1] = true;
  }

  for (int ii = 0 ; ii < number_of_loci ; ii ++)
    {
      if (visitted[ii] == false) //start a new connected component
        {
	  queue<int> fifo_queue;
	  connected_components.push_back(vector<int>());
	  int last_cc_id = connected_components.size() - 1;
	  fifo_queue.push(ii);
	  while (not fifo_queue.empty()) {
	    int head = fifo_queue.front();
	    fifo_queue.pop();
	    if (visitted[head] == false) {
	      visitted[head] = true;
	      connected_components[last_cc_id].push_back(head);
	      for (int jj = 0 ; jj < number_of_loci; jj ++) {
		if ((pair_wise_distances[head][jj] < number_of_recombinations_cut_off) and
		    (visitted[jj] == false)) {
		  fifo_queue.push(jj);
		}
	      }
	    }
	  }//end while
        }
    }
  number_of_connected_components = connected_components.size();
  return connected_components.size();
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map::condense_bin(){
  lg_bins_condensed.resize(linkage_group_bins.size());
  dist_condensed.resize(linkage_group_bins.size());
  // for each linkage group condense two bins if they are too close to each other
  for (unsigned int ii = 0; ii < linkage_group_bins.size(); ii++) {
    lg_bins_condensed[ii].push_back(linkage_group_bins[ii][orders[ii][0]]);
    int crt_bin_id = 0;
    for (unsigned int jj = 1; jj < orders[ii].size(); jj++) {
      // determine whether or not to create a new bin
      if (distance_between_adjacent_pairs[ii][jj-1] < COMBINE_BINS_TH) {
	// condense the next bin into the current bin
	lg_bins_condensed[ii][crt_bin_id].insert(lg_bins_condensed[ii][crt_bin_id].end(),
						 linkage_group_bins[ii][orders[ii][jj]].begin(),
						 linkage_group_bins[ii][orders[ii][jj]].end());
      } else {
	crt_bin_id = crt_bin_id + 1;
	lg_bins_condensed[ii].push_back(linkage_group_bins[ii][orders[ii][jj]]);
	dist_condensed[ii].push_back(distance_between_adjacent_pairs[ii][jj-1]);
      }
    }
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map::write_output(SEXP &map)
{
  SEXP node, dist, mNames, cNames, iNames, dimnames;
  vector<string> rownames;
  double *P_dist;
  int kount;

  extern int trace;

  // map allocated & protected in generate_map
  // unprotect at end here
  PROTECT(iNames=Rf_allocVector(STRSXP,individual_names.size()));
  for(unsigned int jj = 0; jj < individual_names.size(); jj++)
    SET_STRING_ELT(iNames, jj, Rf_mkChar(individual_names[jj].c_str()));

  Rprintf("Number of linkage groups: %d\n", number_of_connected_components);
  Rprintf("The size of the linkage groups are: ");
  for (int ii = 0 ; ii < number_of_connected_components ; ii++)
    {
      Rprintf("%zu\t",(connected_components[ii]).size());
    }
  Rprintf("\n");

  Rprintf("The number of bins in each linkage group: ");
  for (int ii = 0 ; ii < number_of_connected_components; ii++)
    {
      Rprintf("%zu\t",(lg_bins_condensed[ii]).size());
    }
  Rprintf("\n");

  for (int ii = 0 ; ii < number_of_connected_components; ii++)
    {
      PROTECT(dist=Rf_allocVector(REALSXP,(connected_components[ii]).size()));
      PROTECT(mNames=Rf_allocVector(STRSXP,(connected_components[ii]).size()));
      node = VECTOR_ELT(map,ii);
      SET_VECTOR_ELT(node,0,dist);
      P_dist = REAL(VECTOR_ELT(node,0));
      kount=0;
      rownames.clear();
      if(trace) {
	char buffer1[100];
	char buffer2[100];
	char buffer3[100];
	snprintf(buffer1, 100, "%.3f",lowerbounds[ii]);
	snprintf(buffer2, 100, "%.3f",upperbounds[ii]);
	snprintf(buffer3, 100, "%.3f",approx_bounds[ii]);
	Rprintf(";lowerbound: %s upperbound: %s", buffer1, buffer2);
	Rprintf(" cost after initialization: %s\n", buffer3);
	Rprintf("group lg %d\n", ii);
	Rprintf(";BEGINOFGROUP\n");
      }
      if(lg_bins_condensed[ii].size() <= 0)
	Rf_error("lg_bins_condensed[ii].size() <= 0\n");
      //assert(lg_bins_condensed[ii].size() > 0);
      for (vector<int>::iterator iter2 = (lg_bins_condensed[ii][0]).begin();
	   iter2 != (lg_bins_condensed[ii][0]).end();
	   iter2++) {
	if(trace) Rprintf("%s\t%s\n",marker_names[*iter2].c_str(),"0.000");
	P_dist[kount]=0.0e0;
	SET_STRING_ELT(mNames, kount, Rf_mkChar(marker_names[*iter2].c_str()));
	++kount;
      }
      double cum_dist = 0.0;
      if(lg_bins_condensed[ii].size() != dist_condensed[ii].size() + 1)
	Rf_error("lg_bins_condensed[ii].size() != dist_condensed[ii].size() + 1\n");
      //assert(lg_bins_condensed[ii].size() == dist_condensed[ii].size() + 1);
      for (unsigned int jj = 1; jj < lg_bins_condensed[ii].size(); jj++) {
	cum_dist = cum_dist + dist_condensed[ii][jj-1];
	for (vector<int>::iterator iter2 = (lg_bins_condensed[ii][jj]).begin();
	     iter2 != (lg_bins_condensed[ii][jj]).end();
	     iter2++) {
	  if(trace) {
	    char buffer[100];
	    snprintf(buffer, 100, "%.3f", cum_dist);
	    Rprintf("%s\t%s\n",marker_names[*iter2].c_str(),buffer);
	  }
	  P_dist[kount]=cum_dist;
	  SET_STRING_ELT(mNames, kount, Rf_mkChar(marker_names[*iter2].c_str()));
	  ++kount;
	}
      }

      // marker labels in data order
      for (unsigned int jj = 0; jj < linkage_group_bins[ii].size(); jj++) {
	rownames.push_back(marker_names[linkage_group_bins[ii][jj][0]]);
      }

      if(trace) Rprintf(";ENDOFGROUP\n\n");
      Rf_setAttrib(dist, R_NamesSymbol, mNames);
      PROTECT(dimnames = Rf_allocVector(VECSXP, 2));
      PROTECT(cNames=Rf_allocVector(STRSXP,rownames.size()));
      for(unsigned int jj = 0; jj < rownames.size(); jj++)
	SET_STRING_ELT(cNames, jj, Rf_mkChar(rownames[jj].c_str()));
      SET_VECTOR_ELT(dimnames,0,cNames);
      SET_VECTOR_ELT(dimnames,1,iNames);
      Rf_setAttrib(VECTOR_ELT(node,1),R_DimNamesSymbol,dimnames);
      UNPROTECT(4);
    }
  UNPROTECT(2); //iNames, map
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

genetic_map_DH::~genetic_map_DH(){
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map_DH::calculate_pair_wise_distance()
{
  pair_wise_distances.resize(number_of_loci);
  for (int ii = 0 ; ii < number_of_loci; ii++)
    {
      pair_wise_distances[ii].resize(number_of_loci, 0.0);
    }

  for (int ii = 0; ii < number_of_loci; ii++)
    {
      for (int jj = ii ; jj < number_of_loci; jj++)
        {
	  double distance_ii_jj = 0;
	  double none_missing = 0;
	  for (int kk = 0 ; kk < number_of_individual; kk++)
            {

	      if ((raw_mapping_data[ii][kk] != "-") and
		  (raw_mapping_data[jj][kk] != "-")) {
		none_missing = none_missing + 1.0;
		if (raw_mapping_data[ii][kk] != raw_mapping_data[jj][kk]) {
		  distance_ii_jj = distance_ii_jj + 1.0;
		}
	      }
            }
	  if (none_missing < 0.5 * number_of_individual) {
	    Rprintf("caution: too many missing for pair:(%s %s)\n",
		    marker_names[ii].c_str(),marker_names[jj].c_str());
	  }

	  if (none_missing < 0.25 * number_of_individual) { // almost everything is missing, adjust the estimate
	    distance_ii_jj = 0.5 * number_of_individual;
	    none_missing = number_of_individual;
	  }
	  pair_wise_distances[ii][jj] = (distance_ii_jj / none_missing) * number_of_individual;
	  pair_wise_distances[jj][ii] = pair_wise_distances[ii][jj];
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

linkage_group_DH* genetic_map_DH::construct_linkage_group(int group_id)
{
  int _number_of_bins = (linkage_group_bins[group_id]).size();
  int _number_of_individuals = number_of_individual;

  /*Store the probability for each allele to be A*/
  vector<vector< double > > _raw_data ;

  vector<pair<int, int> >  _missing_data;

  vector<int> _current_order;

  _raw_data.resize(_number_of_bins);
  for (int ii = 0 ; ii < _number_of_bins; ii++)
    {
      _raw_data[ii].resize(_number_of_individuals);
      for (int jj = 0; jj < _number_of_individuals; jj++)
        {
	  if (raw_mapping_data[linkage_group_bins[group_id][ii][0]][jj] == "A")
            {
	      /*If an allele is A, then its probability being A is A*/
	      _raw_data[ii][jj] = 1.0;
            }
	  else if (raw_mapping_data[linkage_group_bins[group_id][ii][0]][jj] == "B")
            {
	      /*If an allele is B, then its probability of being A is 0*/
	      _raw_data[ii][jj] = 0.0;
            }
	  else if (is_number(raw_mapping_data[linkage_group_bins[group_id][ii][0]][jj]))
	    {
	      _raw_data[ii][jj] = atof(raw_mapping_data[linkage_group_bins[group_id][ii][0]][jj].c_str());
	    }
	  else
            {
	      /*If an allele is missing, then, assign probability 0.5 for it to be A*/
	      _raw_data[ii][jj] = 0.5;
	      _missing_data.push_back(make_pair(ii,jj)); /*ii is the id for the marker, and jj is the id for the individual*/
            }
        }
    }
  for (int ii = 0 ; ii < _number_of_bins; ii ++)
    {
      _current_order.push_back(ii);
    }
  vector<int> bin_sizes;
  for (int ii = 0; ii < _number_of_bins; ii++) {
    bin_sizes.push_back(linkage_group_bins[group_id][ii].size());
  }
  linkage_group_DH * to_be_returned = new linkage_group_DH(_number_of_bins,
							   _number_of_individuals,
							   detect_bad_data,
							   objective_function,
							   df_,
							   _raw_data,
							   _current_order,
							   _missing_data,
							   bin_sizes);
  return to_be_returned;
}
//////////////////////////////////////////////////////////////////////////

linkage_group_DH* genetic_map_DH::construct_linkage_group_whole_map()
{
    int _number_of_bins = number_of_loci;
    int _number_of_individuals = number_of_individual;

    /*Store the probability for each allele to be A*/
    vector<vector< double > > _raw_data ;

    vector<pair<int, int> >  _missing_data;

    vector<int> _current_order;

    _raw_data.resize(_number_of_bins);
    for (int ii = 0 ; ii < _number_of_bins; ii++)
    {
        _raw_data[ii].resize(_number_of_individuals);
        for (int jj = 0; jj < _number_of_individuals; jj++)
        {
            if (raw_mapping_data[ii][jj] == "A")
            {
                /*If an allele is A, then its probability being A is A*/
                _raw_data[ii][jj] = 1.0;
            }
            else if (raw_mapping_data[ii][jj] == "B")
            {
                /*If an allele is B, then its probability of being A is 0*/
                _raw_data[ii][jj] = 0.0;
            }
	    else if(is_number(raw_mapping_data[ii][jj]))
	      {
		_raw_data[ii][jj] = atof(raw_mapping_data[ii][jj].c_str());
	      }
            else
            {
                /*If an allele is missing, then, assign probability 0.5 for it to be A*/
                _raw_data[ii][jj] = 0.5;
                _missing_data.push_back(make_pair(ii,jj)); /*ii is the id for the marker, and jj is the id for the individual*/
            }
        }
    }
    for (int ii = 0 ; ii < _number_of_bins; ii ++)
    {
        _current_order.push_back(ii);
    }
    vector<int> bin_sizes;
    for (int ii = 0; ii < _number_of_bins; ii++) {
        bin_sizes.push_back(1);
    }
    linkage_group_DH* to_be_returned = new linkage_group_DH(_number_of_bins,
                                                             _number_of_individuals,
                                                             false, // this is fixed to be false for whole map
                                                             OBJF_COUNT,
                                                             df_,
                                                             _raw_data,
                                                             _current_order,
                                                             _missing_data,
                                                             bin_sizes);
    return to_be_returned;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void genetic_map_DH::print_suspicious_data(){
  Rprintf("\n");
  for (unsigned int ii = 0; ii < suspicious_data.size(); ii++) {
    Rprintf("%s \t %s\n", suspicious_data[ii].first.c_str(),
	    suspicious_data[ii].second.c_str());
  }
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map_DH::generate_map(SEXP &map)
{
  SEXP newnode, lNames;
  const char* comp[] = {"map","imputed_values"};
  extern int trace;

  pair_wise_distances.resize(number_of_loci);
  for (int ii = 0 ; ii < number_of_loci; ii++)
    {
      pair_wise_distances[ii].resize(number_of_loci, 0.0);
    }
  /*
    if the total number of missing observations exceeds certain threshold,
    we need to estimate the missing data before clustering
  */
  if ((total_number_of_missing_obs >= ESTIMATION_BEFORE_CLUSTERING * number_of_loci * number_of_individual) and
      (estimation_before_clustering)) {
    linkage_group_DH * linkage_group_whole_map = construct_linkage_group_whole_map();
    linkage_group_whole_map->order_markers();
    const vector<vector<double> > & new_dist = linkage_group_whole_map-> get_pair_wise_distance();
    for (int ii = 0 ; ii < number_of_loci; ii++)
      {
	for (int jj = 0 ; jj < number_of_loci; jj++)
	  {
	    pair_wise_distances[ii][jj] = new_dist[ii][jj];
	  }
      }
    // linkage_group_whole_map->dump_distance_matrix();
    delete linkage_group_whole_map;
  } else {
    if(trace) Rprintf("calculating the pair-wise hamming distance\n");
    calculate_pair_wise_distance();
    if(trace) Rprintf("finished calculating the pair-wise hamming distance\n");
  }
  // dump_distance_matrix();
  cluster();
  // dump_distance_matrix();
  if(trace) Rprintf("found %d connected components\n", number_of_connected_components);

  condense_markers_into_bins();
  // dump_distance_matrix();

  orders.resize(number_of_connected_components);
  upperbounds.resize(number_of_connected_components);
  lowerbounds.resize(number_of_connected_components);
  approx_bounds.resize(number_of_connected_components);
  distance_between_adjacent_pairs.resize(number_of_connected_components);

  // unprotect in write_output
  PROTECT(map = Rf_allocVector(VECSXP,number_of_connected_components));

  for (int ii = 0 ; ii < number_of_connected_components; ii++)
    {
      SET_VECTOR_ELT(map,ii,newnode=NEW_LIST(2));
      lNames = PROTECT(Rf_allocVector(STRSXP, Rf_length(newnode)));
      for(int nn=0; nn < Rf_length(newnode); nn++)
	SET_STRING_ELT(lNames, nn, Rf_mkChar(comp[nn]));
      Rf_setAttrib(newnode, R_NamesSymbol, lNames);
      UNPROTECT(1);

      linkage_group_DH * current_linkage_group = construct_linkage_group(ii);

      current_linkage_group->order_markers();
      current_linkage_group->return_order(orders[ii],
					  lowerbounds[ii],
					  upperbounds[ii],
					  approx_bounds[ii],
					  distance_between_adjacent_pairs[ii]);
      vector<pair<int, int> > bad_data_ii;
      current_linkage_group->bad_genotypes(bad_data_ii);
      for (unsigned int jj = 0; jj < bad_data_ii.size(); jj++) {
	int bin_id = bad_data_ii[jj].first;
	int indi_id = bad_data_ii[jj].second;
	for (unsigned int kk = 0; kk < linkage_group_bins[ii][bin_id].size(); kk++) {
	  string marker_name = marker_names[linkage_group_bins[ii][bin_id][kk]];
	  string indi_name = individual_names[indi_id];
	  suspicious_data.push_back(make_pair(marker_name, indi_name));
	}
      }
      current_linkage_group->dump(newnode);
      delete current_linkage_group;
      if(trace) Rprintf("finished the %d linkage group\n", ii+1);
    }

    // Added by Yonghui on Oct 20, 2007
    // The last step is to condense adjacent bins if they are too close to each other
  condense_bin();
  if(trace) {
      Rprintf("suspicious data detected by our algorithm\n");
      print_suspicious_data();
      Rprintf("double cross overs based on the current order\n");
      print_double_cross_overs();
      // dump the distance matrix
      // dump_connected_components_edges();
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void genetic_map_DH::print_double_cross_overs(){
  for (unsigned int ii = 0; ii < lg_bins_condensed.size(); ii++) {
    if (lg_bins_condensed[ii].size() < 3) {
      continue;
    }
    for (unsigned int jj = 0; jj < lg_bins_condensed[ii].size(); jj++){
      if (lg_bins_condensed[ii][jj].size() > 1) {
	continue;
      }
      int marker_id = lg_bins_condensed[ii][jj][0];
      int pre_marker_id = -1;
      if (jj == 0){ // it is the first bin
	pre_marker_id = lg_bins_condensed[ii][1][0];
      } else {
	pre_marker_id = lg_bins_condensed[ii][jj - 1][0];
      }

      int next_marker_id = -1;
      if (jj == lg_bins_condensed[ii].size() - 1) { // it is the last bin
	next_marker_id = lg_bins_condensed[ii][lg_bins_condensed[ii].size() - 2][0];
      } else {
	next_marker_id = lg_bins_condensed[ii][jj + 1][0];
      }

      for (int kk = 0; kk < number_of_individual; kk++) {
	if (raw_mapping_data[marker_id][kk] == "-") { // ignore missing
	  continue;
	}
	if ((raw_mapping_data[marker_id][kk] != raw_mapping_data[pre_marker_id][kk]) and
	    (raw_mapping_data[marker_id][kk] != raw_mapping_data[next_marker_id][kk])) {
	  // this is a double cross-over
	  Rprintf("%s,%s\n",marker_names[marker_id].c_str(),individual_names[kk].c_str());
	}
      }
    }
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

