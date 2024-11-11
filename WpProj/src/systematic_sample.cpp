#include "systematic_sample.h"

void sample_systematic(vectorI & samps, const vector & weight, const int nsamp ) {
  Rcpp::RNGScope scope;
  
  Rcpp::NumericVector draw = Rcpp::runif(1);
  double u = draw(0)/double(nsamp);
  double sampWeight = weight(0);
  int i = 0;
  
  for ( int j = 0; j < nsamp; j++ ){
    while(sampWeight < u) {
      i++;
      sampWeight += weight(i);
    }
    samps(j) = i;
    u += 1.0/double(nsamp);
  }
  
}
