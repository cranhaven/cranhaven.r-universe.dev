#ifndef CORE_ALGORITHM_RANDOM_DESCENT_H
#define CORE_ALGORITHM_RANDOM_DESCENT_H

#include "CoreAlgorithm.h"

extern const std::string ALGORITHM_RANDOM_DESCENT;

class CoreAlgorithmRandomDescent: public CoreAlgorithm {
  public:
    Rcpp::IntegerVector getCore (CoreMethod &m);
};

#endif
