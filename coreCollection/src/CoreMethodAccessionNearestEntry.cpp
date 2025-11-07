#include "CoreMethodAccessionNearestEntry.h"
using namespace Rcpp;

const std::string METHOD_ACCESSION_NEAREST_ENTRY = "A-NE";

Rcpp::IntegerVector CoreMethodAccessionNearestEntry::adjustRandomNeighbour (Rcpp::IntegerVector c, int i) {
  return c;
}

double CoreMethodAccessionNearestEntry::measure (Rcpp::IntegerVector & c) {
  return CoreMethodAccessionNearestEntry::measure(distanceMatrix, c);
}

//TODO: improve speed
double CoreMethodAccessionNearestEntry::measure (Rcpp::NumericMatrix & dm, Rcpp::IntegerVector & c) {
  double d, nd, sumOfDistances = 0.0;
  int nl = 0, N = dm.nrow();
  for(int i=0; i<N; i++) {
    d = 0.0;
    for(int j=0;j<c.length(); j++) {
      nd = dm[nl+c[j]];
      if(((j==0)&&(c[j]!=i)) || (d>nd)) {
        d = nd;
      }
    }
    sumOfDistances+=d;
    nl+=N;
  }
  return(sumOfDistances/(double) N);
}

bool CoreMethodAccessionNearestEntry::improvement (double m1, double m2) {
  return (m1>m2);
}




