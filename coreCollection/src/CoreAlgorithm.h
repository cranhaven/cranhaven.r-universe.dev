#ifndef CORE_ALGORITHM_H
#define CORE_ALGORITHM_H

#include "CoreMethod.h"
#include <Rcpp.h>

class CoreAlgorithm {
  public:
    virtual Rcpp::IntegerVector getCore (CoreMethod &m) {
      Rcpp::Rcout << "Call to default getCore, should not happen!" << std::endl;
      return Rcpp::IntegerVector(0);
    }
    CoreAlgorithm();
};

#endif
