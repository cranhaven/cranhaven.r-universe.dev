#include <Rcpp.h>
using namespace Rcpp;
#include "SpecialFunctions.h"

NumericVector gammarand(int n, double shape, double rate) {
  MTRand mt;
  mt.seed();
  vector<double> result;
  SpecialFunctions::gammarand(shape,1.0 /rate,n,mt,result);
  NumericVector r(result.begin(),result.end());
  return r;
}

NumericMatrix samplePhi(IntegerMatrix counts) {
  NumericMatrix result(counts.rows(),counts.cols());
  MTRand mt;
  mt.seed();
  for (int i = 0; i < counts.length(); i++) {
    result[i] = SpecialFunctions::gammarand(1 + counts[i], 1, mt);
  }

  return result;
}

// [[Rcpp::export]]
NumericMatrix UpdatePhi(IntegerMatrix data, IntegerMatrix M_all, int FF, int SS, IntegerVector d, int maxd) {
  MTRand mt;
  mt.seed();
  int p = d.length();
  int groups =  FF * SS;
  int phi_rows = maxd * p;
  NumericMatrix phi(phi_rows , groups);
  int n = M_all.ncol();
  IntegerVector groupIndex(n);
  for (int i = 0; i <n; i++) {
    groupIndex[i] = SS*(M_all(0,i)-1) + M_all(1,i) - 1;
  }
  for (int j = 0; j < p; j++) {

    NumericMatrix counts(groups,d[j]);
    for (int i = 0; i < n; i++) { //group count
      counts[groupIndex[i] + (data(j,i)-1) * groups]++;
    }
    for (int i = 0; i < counts.length();i++) { //gammarand sampling
        counts[i] = SpecialFunctions::gammarand(1 + counts[i], 1, mt);
    }

    for (int k =0; k < groups; k++) { //normalization
      int base = k * phi_rows + j * maxd;
      double dsum = 0;
      for (int i = 0; i < d[j];i++) {
        dsum+=counts(k,i);
      }
      if (dsum <=0 ) {dsum =1;}
      for (int i = 0; i < d[j];i++) {
        phi[base + i] = counts(k,i) / dsum;
        //counts(k,i) /= dsum;
      }
    }
  }
  return phi;
}

// [[Rcpp::export]]
NumericMatrix UpdatePhiWeighted(List data, List M_all, int FF, int SS, IntegerVector d, int maxd, NumericVector struc_weight) {
  MTRand mt;
  mt.seed();
  int p = d.length();
  int groups =  FF * SS;
  int phi_rows = maxd * p;
  NumericMatrix phi(phi_rows , groups);

  std::vector<IntegerVector> groupIndexes;
  for (int i = 0; i < struc_weight.length(); i++) {
    IntegerMatrix M = M_all[i];
    int n = M.ncol();
    IntegerVector groupIndex(n);
    for (int g = 0; g <n; g++) {
      groupIndex[g] = SS*(M(0,g)-1) + M(1,g) - 1;
    }
    groupIndexes.push_back(groupIndex);
  }

  for (int j = 0; j < p; j++) {
    NumericMatrix counts(groups,d[j]);
    for (int s = 0; s < struc_weight.length(); s++) {
      double weight = 1.0 / struc_weight[s];
      IntegerMatrix current_data = data[s];
      for (int i = 0; i < current_data.ncol(); i++) { //group count
        counts[groupIndexes[s][j] + (current_data(j,i)-1) * groups] += weight;
      }
    }

    for (int i = 0; i < counts.length();i++) { //gammarand sampling
      counts[i] = SpecialFunctions::gammarand(1 + counts[i], 1, mt);
    }

    for (int k =0; k < groups; k++) { //normalization
      int base = k * phi_rows + j * maxd;
      double dsum = 0;
      for (int i = 0; i < d[j];i++) {
        dsum+=counts(k,i);
      }
      if (dsum <=0 ) {dsum =1;}
      for (int i = 0; i < d[j];i++) {
        phi[base + i] = counts(k,i) / dsum;
        //counts(k,i) /= dsum;
      }
    }
  }
  return phi;
}


