#ifndef SYSTEMATIC_SAMPLE_H
#define SYSTEMATIC_SAMPLE_H

#include "approxOT_types.h"

//' Samples from a multinomial systematically
//'
//' @param samps An Eigen::VectorXi giving the sampled indicators
//' @param v An Eigen::VectorXd of sample weights
//' @param nsamp An int denoting the number of samples to take
//' @return void
//' @keywords internal
void sample_systematic(vectorI & samps, const vector & weight, const int nsamp );
  
#endif //SYSTEMATIC_SAMPLE_H
