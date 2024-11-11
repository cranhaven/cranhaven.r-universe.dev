#include<cassert>
#include<algorithm>
#include "cpputil.hpp"

double quantile(double* vin, double quan, int len)
{
  assert(len>0);
  assert(quan >= 0.0 && quan < 1.0);
  int pos = (int)(quan * len);
  std::nth_element(vin, vin+pos, vin+len);
  return vin[pos];
}

void writeVector(std::ostream& os, std::string name, double* vec, int len)
{
  if(len<=0) return;
  os<<name<<"=("<<vec[0];
  for(int i = 1; i< len; ++i)
    os<<","<<vec[i];
  os<<")";
}

