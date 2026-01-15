#include "jack.h"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template <typename numT>
numT ipow(numT base, unsigned exp){
  numT result(1);
  unsigned int n = exp, b = 1, p = 0;
  while(exp) {
    if(exp & 1) {
      result *= base;
      p += b;
      if(p == n) {
        break;
      }
    }
    exp >>= 1;
    base *= base;
    b *= 2;
  }
  return result;
}

template gmpq   ipow<gmpq>(gmpq, unsigned);
template double ipow<double>(double, unsigned);

