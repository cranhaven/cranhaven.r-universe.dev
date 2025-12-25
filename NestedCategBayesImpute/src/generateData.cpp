#include <Rcpp.h>
using namespace Rcpp;
#include "samplehouseholds.h"
#include "checkconstraints.h"

//input index is for debugging only, can be taken out later on
IntegerMatrix Concatenate(List data, int index) {
  //Rcout << index << std::endl;
  int totalcolumns = 0;
  int rows;
  for (int i = 0; i < data.length(); i++) {
    IntegerMatrix current = data[i];
    if (current != R_NilValue) {
      totalcolumns += current.ncol();
      rows = current.nrow();
    }
  }
  if (rows <= 0 || totalcolumns <=0) {
    IntegerMatrix Empty(0,0);
    return Empty;
  }
  IntegerMatrix result(rows, totalcolumns);
  int offset = 0;
  for (int i = 0; i < data.length(); i++) {
    IntegerMatrix current = data[i];
    if (current != R_NilValue) {
      std::copy(current.begin(),current.end(),result.begin() + offset);
    }
    offset += current.length();
  }
  return result;
}

List GenerateData(int hh_size,List lambda, NumericMatrix omega, NumericMatrix phi,
                  NumericVector pi, IntegerVector d, int batches_done,
                         int valid_hh_needed, int blocksize, int synindex, bool HHhead_at_group_level, int Parallel) {
  List Individuals_extra;
  List G_extra;
  List HHData_extra;
  List synIndividuals;
  int batch_index = 0;
  int valid_hh_found = 0;
  int p = d.length();

  while (valid_hh_found < valid_hh_needed) {
    batch_index ++;
    //generate a batch of 10K household
    List checked_households;
    if (HHhead_at_group_level) {
      IntegerMatrix data_to_check = samplehouseholds(phi,omega, pi, d, lambda, batch_index+batches_done, blocksize,hh_size, 1, Parallel);
      checked_households = checkconstraints_HHhead_at_group_level(data_to_check,valid_hh_needed-valid_hh_found, hh_size, Parallel);
    } else {
      IntegerMatrix data_to_check = samplehouseholds(phi,omega, pi, d, lambda,batch_index+batches_done, blocksize,hh_size, 0, Parallel);
      checked_households = checkconstraints(data_to_check,valid_hh_needed-valid_hh_found, hh_size);
    }

    IntegerMatrix Households = checked_households["Households"];
    int possible = checked_households["possible"];
    IntegerMatrix synHouseholds = checked_households["synHouseholds"];
    if (Households != R_NilValue) {
      int DIM = p + lambda.length() + 1;
      IntegerVector hhrow(Households.ncol());
      G_extra.push_back(Households(Range(hh_size * DIM,hh_size * DIM), Range(0,Households.ncol()-1)));
      HHData_extra.push_back(Households(Range(p+2,DIM-1), Range(0,Households.ncol()-1)));
      Individuals_extra.push_back(households2individuals(Households, hh_size));
    }

    valid_hh_found += possible;
    if (synindex > 0) {
      if (synHouseholds != R_NilValue) {
        synIndividuals.push_back(households2individuals(synHouseholds,hh_size));
      }
    }
  }

  batch_index += batches_done;
  return(List::create(Named("Individuals_extra", Concatenate(Individuals_extra,1)),
                      Named("G_extra", Concatenate(G_extra,2)),
                      Named("HHData_extra", Concatenate(HHData_extra,3)),
                      Named("synIndividuals", Concatenate(synIndividuals,4)),
                      Named("batch.index", batch_index)
                        ));
}

// [[Rcpp::export]]
List GetImpossibleHouseholds(IntegerVector d,IntegerVector n_star_h, List lambda,
                             NumericMatrix omega, NumericMatrix phi, NumericVector pi,
                             int blocksize,  int n, int synindex, bool HHhead_at_group_level, bool Parallel) {
  int cumsize = 0;
  NumericMatrix hh_size_new(n_star_h.length(),1);
  List hh_index;
  List ImpossibleIndividuals;
  List G_extra;
  List HHdata_extra;
  List synIndividuals_all;

  int batches_done = 0;
  for (int hh_size  = 1; hh_size <=n_star_h.length(); hh_size++) {
    int hh_size_real;
    if (HHhead_at_group_level) {
      hh_size_real = hh_size;
    } else {
      hh_size_real = hh_size + 1;
    }
    List batch = NULL;
    while (true) { // in rare locations no valid imposible household it found, rerun it
      try {
        int parallel = Parallel ? 1: 0;
        batch = GenerateData(hh_size_real,lambda, omega, phi,pi, d, batches_done,
                          n_star_h[hh_size - 1],blocksize,synindex,HHhead_at_group_level, parallel);
        IntegerMatrix G_test = batch["G_extra"];
        if (G_test != R_NilValue) {
          break;
        }
      } catch (...) {
        // do nothing, just rerun generateData
      }
    }
    IntegerMatrix G_extra_1 = batch["G_extra"];
    IntegerMatrix Individuals_extra = batch["Individuals_extra"];
    IntegerMatrix HHData_extra_1 = batch["HHData_extra"];
    if (G_extra_1 != R_NilValue) {
      hh_size_new[hh_size - 1] = G_extra_1.length();
    } else {
      hh_size_new[hh_size - 1] = 0;
    }
    cumsize += hh_size_new[hh_size-1];
    ImpossibleIndividuals.push_back(Individuals_extra);
    G_extra.push_back(G_extra_1);

    if (HHData_extra_1 != R_NilValue) {
      IntegerMatrix HHData_extra_wth_size(HHData_extra_1.nrow() + 1, HHData_extra_1.ncol());
      for (int i = 0; i < HHData_extra_1.ncol(); i++) {
        for (int j = 0; j < HHData_extra_1.nrow(); j++) {
          HHData_extra_wth_size(j,i) = HHData_extra_1(j,i);
        }
        HHData_extra_wth_size(HHData_extra_1.nrow(),i) = hh_size;
      }
      HHdata_extra.push_back(HHData_extra_wth_size);
    }
    if (synindex > 0) {
      IntegerMatrix synIndividuals = batch["synIndividuals"];
      synIndividuals_all.push_back(synIndividuals);
    }
    int batch_index = batch["batch.index"];
    batches_done = batch_index;
  }

  IntegerMatrix hh_index_combined = Concatenate(hh_index,5);
  IntegerMatrix ImpossibleIndividuals_combined = Concatenate(ImpossibleIndividuals,6);
  for (int i = 0; i < hh_index.length(); i++) {
    ImpossibleIndividuals_combined(0,i) = hh_index(0,i);
    ImpossibleIndividuals_combined(0,i) += n;
  }

  int DIM = ImpossibleIndividuals_combined.nrow() - 2;
  IntegerMatrix IndividualData_extra(DIM,ImpossibleIndividuals_combined.ncol());
  IntegerMatrix G_Individuals_and_M_extra(2,ImpossibleIndividuals_combined.ncol());

  for (int i = 0; i < ImpossibleIndividuals_combined.ncol(); i++) {
    for (int j = 0; j < DIM; j++) {
      IndividualData_extra(j,i) = ImpossibleIndividuals_combined(j,i);
    }
    G_Individuals_and_M_extra(0,i) = ImpossibleIndividuals_combined(DIM,i);
    G_Individuals_and_M_extra(1,i) = ImpossibleIndividuals_combined(DIM+1,i);
  }
  return(List::create(Named("G_Individuals_and_M_extra", G_Individuals_and_M_extra),
                      Named("G_extra", Concatenate(G_extra,7)),
                      Named("IndividualData_extra", IndividualData_extra),
                      Named("HHdata_extra", Concatenate(HHdata_extra,8)),
                      Named("hh_size_new", hh_size_new),
                      Named("synIndividuals_all", Concatenate(synIndividuals_all,9))
                        ));
}
