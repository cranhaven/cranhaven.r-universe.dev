#ifndef RFDISTCONTAINER_H
#define RFDISTCONTAINER_H

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include "tbb/concurrent_unordered_map.h"

#include "hash.h"

template<T>
class matrixContainer {
  matrixContainer() {};
  
  
  
private:
  tbb::concurrent_unordered_map< std::pair<uint32_t, uint32_t>, T> mContainer_;
};

#endif
