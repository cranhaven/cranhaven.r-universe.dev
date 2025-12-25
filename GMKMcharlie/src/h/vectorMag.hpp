# pragma once
# include "raisePower.hpp"

// No root.
template<typename indtype, typename valtype, bool takeRoot>
inline valtype minkMag(valtype *x, indtype size, valtype p)
{
  valtype rst = 0;
  if(p == 2) // Euclidean distance. Primary.
  {
    rst = std::inner_product(x, x + size, x, 0.0);
    if(takeRoot) rst = std::sqrt(rst);
  }
  else if(p == 1) // L1 norm.
  {
    for(valtype *xend = x + size; x < xend; ++x) rst += std::abs(*x);
  }
  else if(p == 0) rst = *std::max_element(x, x + size); // max norm.
  else if(p >= 3 and p <= 35 and std::abs(int(p) / p - 1) < 1e-10) // p equals an integer and 3 <= p <= 35.
  {
    for(valtype *xend = x + size; x < xend; ++x)
      rst += raisePower(std::abs(*x), p);
    if(takeRoot) rst = std::pow(rst, 1 / p);
  }
  else
  {
    for(valtype *xend = x + size; x < xend; ++x)
      rst += std::pow(std::abs(*x), p);
    if(takeRoot) rst = std::pow(rst, 1 / p);
  }
  return rst;
}













































