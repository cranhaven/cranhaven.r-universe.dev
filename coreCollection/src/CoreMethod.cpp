#include "CoreMethod.h"

using namespace Rcpp;

std::string CoreMethod::getMethod() {
  return method;
}

CoreMethod::CoreMethod(std::string m, Rcpp::NumericMatrix & dm, Rcpp::List & g) {
  method = m;
  distanceMatrix = dm;
  groups = g;
  //administration
  Rcpp::IntegerVector srp(0);
  Rcpp::IntegerVector sfp(0);
  for (int i = 0; i < g.length(); ++i) {
    Rcpp::NumericVector col = g[i];
    if(col.length()>1) {
      srp.push_back(i);
    } else {
      sfp.push_back(i);
    }
  }
  selectedRandomPositions = srp;
  selectedFixedPositions = sfp;
  accessionNumber = dm.nrow();
  coreNumber = g.length();
  fixedCoreNumber = sfp.length();
  randomCoreNumber = srp.length();
}

Rcpp::IntegerVector CoreMethod::getRandomNeighbour(Rcpp::IntegerVector c1) {
  int i, j, m, n = selectedRandomPositions.length();
  Rcpp::IntegerVector c2 = clone(c1);
  Rcpp::NumericVector col;
  if(n==0) {
    return c2;
  } else {
    //i = rand() % n;
    i = ((int) Rcpp::runif(1,0,n)[0]) % n;
    col = groups[selectedRandomPositions[i]];
    m = col.length();
    //j = rand() % m;
    j = ((int) Rcpp::runif(1,0,m)[0]) % m;
    c2[selectedRandomPositions[i]] = col[j];
    c2 = adjustRandomNeighbour(c2, i);
  }
  return c2;
}

Rcpp::IntegerVector CoreMethod::getRandom() {
  Rcpp::IntegerVector c(groups.length());
  int i, j, l, n=selectedRandomPositions.length(), m=selectedFixedPositions.length();
  Rcpp::NumericVector col;
  for(i=0;i<n;i++) {
    col = groups[selectedRandomPositions[i]];
    l = col.length();
    //j = rand() % col.length();
    j = ((int) Rcpp::runif(1,0,l)[0]) % l;
    c[selectedRandomPositions[i]] = col[j];
  }
  for(i=0;i<m;i++) {
    col = groups[selectedFixedPositions[i]];
    c[selectedFixedPositions[i]] = col[0];
  }
  return c;
}

